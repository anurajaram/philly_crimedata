# author - Anupama Rajaram
# Date - Sep 13, 2016
# Description - Data Exploration and Visualization
# Dataset - Philadelphia Crime Data from Kaggle
# Code with output can also be run online or viewed on the Kaggle website 
# link = https://www.kaggle.com/anu2analytics/d/mchirico/philadelphiacrimedata/crime-area-heatmap-r/code

# call source file with all required packages.
source("C:/anu/data analytics/twitter API - aug 2016/workspace_prep.R")
library(ggmap)
library(lattice)

# Read the crime data
phildata <- fread("crime.csv")


# summarise latitude and longitude data
summary(phildata$Lon)
summary(phildata$Lat)


# checkin for NAs
sapply(phildata, function(x) sum(is.na(x)))
# note, even with 16722 NAs (missing values), it is less than ~0.8% of missing data.


# Summarise the data for crime categories
summary(phildata$Text_General_Code)

# checking for date time manipulation
phildata$dt = as.Date(phildata$Dispatch_Date)
phildata$year = as.numeric(format(phildata$dt, "%Y"))
phildata$mth = as.numeric(format(phildata$dt, "%m"))
phildata$day = as.numeric(format(phildata$dt, "%d"))


# Group crime by dates and Police Districts
crimephilly = sqldf("select Police_Districts as 'District', 
                 day, year, mth, Hour,
                 Text_General_Code as 'Category', 
                 count(*) as 'count' 
                 from phildata
                 group by Police_Districts, day, year, mth, Hour, Text_General_Code")


# ----------- Chart No. 1 -------------- #
# Graphical Visualization of crimes by category
data_plot = crimephilly %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  transform(Category = reorder(Category,-count))

ggplot(data_plot) + 
  geom_bar(aes(x=Category, y=count),
           stat="identity")+
  coord_flip()+
  theme(legend.position="None")+
  ggtitle("Number of crimes in individual category")+
  ylab("Number of crimes")+
  xlab("Category of crime")


# ----------- Chart No. 2 -------------- #
# Graphical Visualization of crimes by Police district
data_plot2 = crimephilly %>%
  group_by(District) %>%
  summarise(count = n()) %>%
  transform(District = reorder(District,-count))

ggplot(data_plot2) + 
  geom_bar(aes(x=District, y=count),
           stat="identity")+
  coord_flip()+
  theme(legend.position="None")+
  ggtitle("Number of crimes in each District")+
  ylab("Number of crimes")+
  xlab("Philadelphia District")


# ----------- Chart No. 3 -------------- #
# Crime count by category and year 
# note this image has a fixed scale hence we can clearly see the top 5 crime categories.
ggplot(data=crimephilly, aes(x=year)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~Category)

# The graph also shows that vandalism and narcotics have decreased in the past few years.


# ----------- Chart No. 4 -------------- #
# Crime count by Ploice District and year 
# note this image has a fixed scale hence we can clearly see the top 5 crime areas
ggplot(data=crimephilly, aes(x=year)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~District)
# the graph shows that crime is maximum in districts 11,15,16.
# However, crime rates overall show a declining rate. 



# ----------- Chart No. 5,6 -------------- #
# Crime count by category and Police District for 2014 and 2015 
# We will subset the data for 2014 and 2015.
crime2014 <- subset(crimephilly, year == 2014)
crime2015 <- subset(crimephilly, year == 2015)

# Crime chart for year 2014
crimeplot14 <- ggplot(data=crime2014, aes(x=District)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~Category) +
  ggtitle("Crime Report 2014")
ggplotly(crimeplot14)

# Crime chart for year 2015
crimeplot15 <- ggplot(data=crime2015, aes(x=District)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~Category) +ggtitle("Crime Report 2015")
ggplotly(crimeplot15)



