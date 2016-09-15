# author - Anupama Rajaram
# Date - Sep 13, 2016
# Description - Data Exploration and Inferential Statistics
# Dataset - Philadelphia Crime Data from Kaggle

# call source file with all required packages.
library(data.table)   # for fread() 
library(sqldf)
library(bit64)
library(plyr)
library(sqldf)

library(heatmaply)
library(plotly)
library(htmltools)
library(ggvis)

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')

library("maps")
library(leaflet)
library(ggmap)
library(ggplot2)
library(dplyr)

# Read the crime data
phildata <- fread("crime.csv")


# contour map for Philadelphia
phil = c(lon = -75.19, lat = 39.98)
philmap = get_map(location = phil, zoom = 12, color = "bw")


# We write a function to generate contour map filtered for a specific crime category.
# Colors indiacte whether crime is high or low. 
# red= high_count, green=low_count
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


# Call function to view crime-density map for Thefts
map_crime(phildata, c('Thefts'))


# Call function to view crime-density map for Thefts
map_crime(phildata, c('Fraud'))


# Call function to view crime-density map for Aggravated assault
map_crime(phildata, c('Aggravated Assault No Firearm'))


# Call function to view crime-density map for Narcotics
map_crime(phildata, c('Narcotic / Drug Law Violations'))


# Call function to view crime-density map for Burglary
map_crime(phildata, c('Burglary Residential'))




