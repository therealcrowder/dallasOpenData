City of Dallas Open Data Portal
Mapping Burglaries with Leaflet
================
Mike Crowder
June - 2018

-----------------------------------------------
The data displayed in the maps below are from the [Dallas Open Data Website](https://www.dallasopendata.com/). This dataset is from the [Police Incidents](https://www.dallasopendata.com/Public-Safety/Police-Incidents/tbnj-w5hb) dataset that is updated daily.

### 2016 Burglary Map


```r
###Capstone Project
##Dallas Open Data-Police Incidents
## Dot Maps
#6/10/2018

###Scrape data using Socrata API
## Install the required package with:
## install.packages("RSocrata")

library(RSocrata)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)

###Install packages for mapping
#install.packages("leaflet")
#install.packages("leaflet.extras")
library(leaflet)
library(leaflet.extras)


#Use soDA URL to access Incident Data; set limit on number of rows per page to 500 for now
url = "https://www.dallasopendata.com/resource/qqc2-eivj.csv?$where=offincident like '%BURGLARY%'"

df <- read.socrata(url, app_token = NULL, email= NULL, password= NULL)

#Check for correct data loaded
head(df)

# remove all records that do not have any information from the column Location1
df1 <- df[!(is.na(df$location1) | df$location1==""),]

# View Data 
#View(df1)


# Take the Point and () out of the location1 data
# Seperate Point and ()
df1$latlon <- gsub(".*\\((.*)\\).*","\\1", df1$location1)
# Spilt out into list 2 columns
LatLon <- str_split_fixed((df1$latlon)," ",2)
# Name columns in list
colnames(LatLon) <- c("Longitude", "Latitude")
# Covert into dataframe
df_LatLon <-as.data.frame(LatLon)

# Bring into current frame
# Convert to Numeric from factor
df1$Latitude <- as.numeric(as.character(df_LatLon$Latitude))
df1$Longitude <- as.numeric(as.character(df_LatLon$Longitude))

#remove LatLon list and data frame df_LatLon
rm(LatLon)
rm(df_LatLon)

# Check UCR Offense
buildingTypes <- group_by(df1, ucroffense)
summarise(buildingTypes, sites = n_distinct(incidentnum))

# Break data into 2016 and 2017
# We are using date 1, which is the date of the occurrence of
# the incident
# It would not appear that 2014 is within range
# df_2014 <- df1[ which(df1$year1=='2014'),]
df_2015 <- df1[ which(df1$year1=='2015'
            & df1$ucroffense==c("BURGLARY-BUSINESS","BURGLARY-RESIDENCE")),]
df_2016 <- df1[ which(df1$year1=='2016'
            & df1$ucroffense==c("BURGLARY-BUSINESS","BURGLARY-RESIDENCE")),]
df_2017 <- df1[ which(df1$year1=='2017' 
            & df1$ucroffense==c("BURGLARY-BUSINESS","BURGLARY-RESIDENCE")),]

# 2016 Map
d_2016 <- leaflet() %>% setView(lng = -96.7970, lat = 32.7767, zoom = 11)
d_2016 %>% addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(lng = df_2016$Longitude,
    lat = df_2016$Latitude,
    popup = paste("MO: ", df_2016$mo, "<br>",
      "Premise: ", df_2016$premise, "<br>",
      "Date: ", df_2016$date1, "<br>",
      "Day: ", df_2016$day1,
      "Time: ", df_2016$time1,
      "Zip: ", df_2016$zipcode),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.75,
    color = pal(df_2016$ucroffense))%>%
  addLegend("bottomright", pal = pal, values = df_2016$ucroffense,
    title = "2016 Dallas Burglaries")
```
