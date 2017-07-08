# Gary Koplik
# Summer, 2017
# app.R
#   shiny app for index of economic well-being

# load libraries
library(shiny)
library(shinyBS)
library(leaflet)
library(dplyr)
library(albersusa)
library(sp)

#### get all necessary data plot ready ####

# load shiny-ready data
load("./county.Rdata")

# set colorpalette
qpal <- colorBin(
  palette = "RdBu",
  domain = county@data[["Total.Index.Value"]],
  bins = c(1, 2, 3, 4, 5, 6),
  reverse = T)

# for changing the projection
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  # Lambert Azimuth Equal Area projection
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))


#### UI ####

ui <- fluidPage(
  
  # set background color theme (downloaded .css file)
  # stored in from www file inside shiny app directory
  theme = "united.css",
  
  # change the background color on the leaflet map
  tags$head(
    tags$style(HTML(".leaflet-container { background: #ffffff; }"))
  ),
  
  # Application title
  titlePanel("Index of Economic Well-Being (2012)"),
  
  h4(strong("Blue is above average"),
     style = "color: #000080"),
  
  h4(strong("Red is below average"),
     style = "color: #800000"),
  
  # Sidebar with slider inputs weights of different variable
  #  and draw map button
  sidebarLayout(
    sidebarPanel(
      # obesity
      sliderInput("obesity",
                  "Weighting of Obesity:",
                  min = 0,
                  max = 1,
                  value = 0.25),
      # no high school
      sliderInput("no_high_school",
                  "Weighting of Percent With No High School Diploma:",
                  min = 0,
                  max = 1,
                  value = 0.25),
      # poverty
      sliderInput("poverty",
                  "Weighting of poverty:",
                  min = 0,
                  max = 1,
                  value = 0.25),
      # unemployment
      sliderInput("unemployment",
                  "Weight of Unemployment:",
                  min = 0,
                  max = 1,
                  value = 0.25),
      width = 3
      
    ),
    
    # Show alerts and map output
    mainPanel(
      bsAlert("alert"),
      # note https://github.com/rstudio/leaflet/issues/59
      #   widget dimensions must go here for shiny
      leafletOutput("mymap", height = 600),
      p()
    )
  )
)

#### server ####

server <- function(input, output, session) {
  
  # make leaflet map 
  output$mymap <- renderLeaflet({
    
    # check whether weights add up to 1 when trying to re-render map
    #   warn user if \sum weights != 1
    # dismiss previous alert specified by alertID
    closeAlert(session, "weightsAlert")
    # check to see if new alert needed
    if(input$obesity + input$poverty + input$no_high_school +
       input$unemployment != 1){
      createAlert(session,
                  anchorId = "alert", 
                  alertId = "weightsAlert",
                  title = "Invalid Weights Selection",
                  content = "Weights Must Add Up to Exactly 1.00",
                  append = FALSE)
      # don't make the map if the weights don't add up to 1.00
      return()
    }
    # update the index value based on slider weights   
    county@data[["Total.Index.Value"]] <-
      county@data$obesity_quantile * input$obesity +
      county@data$poverty_quantile * input$poverty +
      county@data$education_quantile * input$no_high_school +
      county@data$unemployment_quantile * input$unemployment
    
    
    # popup label for each county
    # <strong> bolds text and <br/> goes to the next line
    popup <- paste0("<strong>", county@data$name, " County", "</strong>",
                    "<br/> Index Value: ",
                    county@data[["Total.Index.Value"]],
                    "<br/> Obesity: ",
                    county@data[["Obesity"]], "%",
                    "<br/> Percent With No High School Diploma: ",
                    county@data[["Percent_Less_Than_High_School"]], "%",
                    "<br/> Poverty: ",
                    county@data[["Poverty.Percent..All.Ages"]], "%",
                    "<br/> Unemployment: ",
                    county@data[["Unemployment.Rate"]], "%"
    )
    # make map
    leaflet(county, options = leafletOptions(crs = epsg2163)) %>%
      # add polygons of color
      addPolygons(stroke = T,
                  fillOpacity = 1, color = "black", opacity = 1,
                  fillColor = ~qpal(county@data[["Total.Index.Value"]]),
                  weight = 0.5,
                  popup = popup,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      # add legend
      #   the function in here is a result of the numbers not
      #     representing the actual cut points
      # see https://github.com/rstudio/leaflet/issues/211
      addLegend(pal = qpal,
                values = county@data[["Total.Index.Value"]],
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  cuts = round(cuts, digits = 2)
                  paste0(cuts[-n], " &ndash; ", cuts[-1])
                },
                title = "Index Value",
                opacity = 1) %>%
      # specify starting viewpoint
      setView(lng = -90, lat = 37.5, zoom = 3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
