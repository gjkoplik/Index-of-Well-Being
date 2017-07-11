# Gary Koplik
# Summer, 2017
# drafting_leaflet_widgets.R


# confirm can start from scratch by clearing environment before
rm(list = ls() )

# load libraries
library(leaflet)
library(dplyr)
library(albersusa)
library(sp)
library(htmlwidgets)

#### get all necessary data plot ready ####

# load plot data
load("../data/toplot.Rdata")

toplot$USEFUL_FIPS <- as.character(toplot$USEFUL_FIPS)

# load median household income data
hhold_income <- read.csv("../data/median_hhold_income.csv",
                         stringsAsFactors = F) %>%
  select(USEFUL_FIPS,
         "med_hhold_income" = Median.household.income..In.2013.Inflation.adjusted.dollars.)

hhold_income$USEFUL_FIPS <- as.character(hhold_income$USEFUL_FIPS)

toplot <- toplot %>%
  left_join(hhold_income)

# load county data

temp <- counties_composite()

# keeping temp as a reference because the simplification is slow
#   if starting from scratch later
county <- temp

# join in the data into the counties shapefile

# need the 01 values to turn to 1 to match csv data
#   and match as a factor
county$fips <- as.character(as.numeric(county$fips))

# join in the toplot data
county@data <- left_join(county@data, toplot,
                         by = c("fips" = "USEFUL_FIPS"))


#### function to make leaflet map ####

# makes a leaflet map
# inputs: colName (str) - name of column of toplot data set
#   mapTitle (str) - title wanted over legend on map
makeDynamicMap <- function(colName, mapTitle){
  
  # set colorpalette
  qpal <- colorQuantile(
    palette = "RdBu",
    domain = county@data[[colName]],
    reverse = T,
    n = 5)
  
  # for changing the projection
  epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    # Lambert Azimuth Equal Area projection
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7))
  
  
  # popup label for each county
  popup <- paste0(county@data$name, " County: ",
                  county@data[[colName]], "%")
  
  # make the map
  map <- leaflet(county,
                 options = leafletOptions(crs = epsg2163)) %>%
    # add polygons of color
    addPolygons(stroke = T,
                fillOpacity = 1, color = "black", opacity = 1,
                fillColor = ~qpal(county@data[[colName]]),
                weight = 0.5,
                popup = popup,
                # popupOptions = popupOptions(),
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    # add legend
    #   the function in here is a result of the numbers not
    #     representing the actual cut points
    # see https://github.com/rstudio/leaflet/issues/211
    addLegend(pal = qpal,
              values = county@data[[colName]],
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                # cuts_before <<- cuts
                # cuts <<- cuts
                cuts = round(cuts, digits = 2)
                # cuts_after <<- cuts
                paste0(cuts[-n], "%", " &ndash; ", cuts[-1], "%")
              },
              title = mapTitle,
              opacity = 1) %>%
    # specify starting viewpoint
    setView(lng = -90, lat = 37.5, zoom = 3)
  return(map)
}

#### make all relevant html widgets and save them ####

# Note these will not replace old files
#   will need to delete old widgets in the directory path
#   if you resave

# poverty
poverty <- makeDynamicMap("Poverty.Percent..All.Ages",
                          "Poverty Percentage (2012)")
saveWidget(poverty, "../maps/poverty_dynamic.html")

# obesity
obesity <- makeDynamicMap("Obesity",
                          "Obesity Percentage (2012)")
saveWidget(obesity, "../maps/obesity_dynamic.html")

# Percent Didn't finish High School
nohighschool <- makeDynamicMap("Percent_Less_Than_High_School",
                               "Percent Without High School Diploma (2012)")
saveWidget(nohighschool, "../maps/no_high_school_dynamic.html")

# unemployment
unemployment <- makeDynamicMap("Unemployment.Rate",
                               "Unemployment Rate (2012)")
saveWidget(unemployment, "../maps/unemployment_dynamic.html")


#### median household income ####

# hardcoding this figure because units and high / low importance
#   different than other figures

# set colorpalette
qpal <- colorQuantile(
  palette = "RdBu",
  domain = county@data[["med_hhold_income"]],
  # reverse = T,
  n = 5)

# for changing the projection
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  # Lambert Azimuth Equal Area projection
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))


# popup label for each county
popup <- paste0(county@data$name, " County: ", "$",
                county@data[["med_hhold_income"]])

# make the map
med_hhold_income <- leaflet(county,
                            options = leafletOptions(crs = epsg2163)) %>%
  # add polygons of color
  addPolygons(stroke = T,
              fillOpacity = 1, color = "black", opacity = 1,
              fillColor = ~qpal(county@data[["med_hhold_income"]]),
              weight = 0.5,
              popup = popup,
              # popupOptions = popupOptions(),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  # add legend
  #   the function in here is a result of the numbers not
  #     representing the actual cut points
  # see https://github.com/rstudio/leaflet/issues/211
  addLegend(pal = qpal,
            values = county@data[["med_hhold_income"]],
            labFormat = function(type, cuts, p) {
              n = length(cuts)
              # cuts_before <<- cuts
              # cuts <<- cuts
              cuts = round(cuts, digits = 2)
              # cuts_after <<- cuts
              paste0("$", cuts[-n], " &ndash; ", "$", cuts[-1])
            },
            title = "Median Household Income (2012)",
            opacity = 1) %>%
  # specify starting viewpoint
  setView(lng = -90, lat = 37.5, zoom = 3)

saveWidget(med_hhold_income, "../maps/med_hhold_income_dynamic.html")


# save county.Rdata for the shiny app (relevant for uploading to shinyapps.io)
save(county, file = "../Index_of_well_being_shiny/county.Rdata")
# also save it in data file so all data files in one place
save(county, file = "../data/county.Rdata")

