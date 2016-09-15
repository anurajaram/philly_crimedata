# author - Anupama Rajaram
# Date - Sep 13, 2016
# Description - Data Exploration and Inferential Statistics
# Dataset - Philadelphia Crime Data from Kaggle

# call source file with all required packages.
source("C:/anu/data analytics/twitter API - aug 2016/workspace_prep.R")
library(ggmap)
library(ggplot2)
library(dplyr)
library(data.table)

# Read the crime data
phildata <- fread("crime.csv")


# summarise latitude and longitude data
summary(phildata$Lon)
summary(phildata$Lat)
# note, even with 16722 NAs (missing values), it is less than ~0.8% of missing data.


# summarise crime categories
summary(phildata$Text_General_Code)

# checking for date time manipulation
phildata$dt = as.Date(phildata$Dispatch_Date)
phildata$year = as.numeric(format(phildata$dt, "%Y"))
phildata$mth = as.numeric(format(phildata$dt, "%m"))
phildata$day = as.numeric(format(phildata$dt, "%d"))


# contour map for Philadelphia
phil = c(lon = -75.19, lat = 39.98)
philmap = get_map(location = phil, zoom = 12, color = "bw")

philm <- readRDS("phillymapobj.rds")

map_crime <- function(crime_data, crime_category_name) 
  {
      titlestr <- paste("Heatmap for crimes related to", crime_category_name )
      filterdf <- filter(crime_data, Text_General_Code %in% crime_category_name)
      plotimg <- ggmap(philm, extent = "device") +
        geom_density2d(data = filterdf, aes(x = Lon, y = Lat), size = 0.3) +
        stat_density2d(data = filterdf, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..)
                       , size = 0.01, bins = 16, geom = 'polygon') +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
        ggtitle(titlestr)
        theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) 
      
      
  return(plotimg)
}

ggmap(philmap)
map_crime(phildata, c('Thefts'))  # image stored as theft_philly.jpeg
map_crime(phildata, c('Fraud')) # image = frauddf.jpg
map_crime(phildata, c('Aggravated Assault No Firearm'))
map_crime(phildata, c('Narcotic / Drug Law Violations'))  # image = narcotic_philly.jpg
map_crime(phildata, c('Burglary Residential')) # image = burglary_philly.jpg



