# Gary Koplik 
# Summer, 2017
# make_data.R

# clear old data to confirm can run from scratch
rm(list = ls() )

# load libraries
library(stringr)
library(plyr)
library(ggplot2)
library(maps)
library(sp)
library(rgdal)
# library(tigris)
library(dplyr)

# read in data set
dat <- read.csv("../data/index_vals.csv", stringsAsFactors = F)

# switch things reading in as characters to numeric
dat$Obesity_Percent <- as.numeric(dat$Obesity_Percent)
dat$Poverty.Percent..All.Ages <- as.numeric(dat$Poverty.Percent..All.Ages)

# switch fips from numeric to factor
dat$USEFUL_FIPS <- as.factor(dat$USEFUL_FIPS)

# manipulate county name strings to line up with mapping df
#   drop the " County" at the end of each county name
#   and make lowercase
dat$County <- tolower(str_sub(dat$County, end = -8))

dat <- plyr::rename(dat, c("County" = "subregion"))


# testing what format the data will need to be in
#   to make maps

# shoutout to StackOverflow
# http://stackoverflow.com/questions/37912418/how-does-geom-map-map-id-function-work

# and github
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

# settled on this format:

# counties shapefile
counties <- map_data("county")


# grabbing counties through tigris
# tigris_counties <- counties(cb = TRUE, resolution = '20m', year = 2013)

# grab the states to color in state boundaries on map
statebounds <- map_data("state")

# join the counties data with the data to plot
# need uniqueness of state + county (because county names repeat)
dat$State <- tolower(dat$State)

# removing this for leaflet purposes
# NOTE: this leftjoin screws up leaflet but works for ggplot
# toplot <- left_join(dat, counties,
#                     by = c("subregion" = "subregion",
#                            "State" = "region"))

# giving data set "more informative" name for later
#   at least it's better than "dat"...
toplot <- dat

# rename toplot and counties values so rollover values look better
toplot <- plyr::rename(toplot, c("Obesity_Percent" = "Obesity"))
toplot <- plyr::rename(toplot, c("subregion" = "County"))
counties <- plyr::rename(counties, c("subregion" = "County"))

# save transformed data sets
save(toplot, file = "../data/toplot.Rdata")
save(statebounds, file = "../data/statebounds.Rdata")
save(counties, file = "../data/counties.Rdata")
