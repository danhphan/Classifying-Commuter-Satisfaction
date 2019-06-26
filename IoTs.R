
# IOTS

rm(list = ls())
gc()
# Load library and data sources
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggmap)


mpostcodes <- read_csv("C:/Users/danph/Google Drive/Persona/MIT Study/R Projects/Statistical Learning/data/MPID_Postcodes.csv")
mdata <- read_csv("C:/Users/danph/Google Drive/Persona/MIT Study/R Projects/Statistical Learning/data/MPID_data.csv")

# Explore data
# MPID Postcodes table
dim(mpostcodes)
mpostcodes <- mpostcodes[,1:6]
str(mpostcodes)
class(mpostcodes)
mpostcodes[which(mpostcodes$Area %in% "Ryde"),]
# Get needed features
mpostcodes$`Total Postcodes` <- NULL

# post_frequence <- mpostcodes %>% select(Pcode) %>% group_by(Pcode) %>% 
#   mutate(countP=n()) %>% arrange(desc(countP))

# MDID data table
dim(mdata)
mdata <- mdata[,1:28]
str(mdata)
head(mdata[,20:28])

# Get needed features in mdata
features <- c("Precinct","Age Group","Postcode","Travel Mode - Mon","Travel Mode - Tue","Travel Mode - Wed",
              "Travel Mode - Thu","Travel Mode - Fri","Arrival Time","Arrival Time Range",
              "Departure Time","Departure Time Range","Commute Satisfaction")
new_names <- c("Precinct","AgeGroup","Postcode","Mon","Tue","Wed",
              "Thu","Fri","ArrivalTime","ArrivalTimeRange",
              "DepartureTime","DepartureTimeRange","Satisfaction")

mdata_sub <- mdata[,features]
dim(mdata_sub)
str(mdata_sub)
# Rename column
colnames(mdata_sub) <- new_names

# Reshape data by melting
ids <- c("Precinct","AgeGroup","Postcode","ArrivalTime","ArrivalTimeRange",
         "DepartureTime","DepartureTimeRange","Satisfaction")

melt_data <- melt(mdata_sub,id=ids)
dim(mdata_sub)
dim(melt_data)
str(melt_data)
colnames(melt_data) <- c("Precinct","AgeGroup","Postcode","ArrivalTime","ArrivalTimeRange",
                         "DepartureTime","DepartureTimeRange","Satisfaction","Day","TravelMethod")
melt_data$Day <- as.character(melt_data$Day)
unique(melt_data$TravelMethod)

# Check frequency of travel type
freq_travel <- count(melt_data,melt_data$TravelMethod)
freq_travel <- as.data.frame(freq_travel)
colnames(freq_travel) <- c("TravelMethod","Frequency")
freq_travel <- freq_travel[order(-freq_travel$Frequency),]
#View(freq_travel)
barplot(freq_travel$Frequency,names.arg = freq_travel$TravelMethod)

# Sel TravelMethod to three types: 
# + Car: Drove car alone, Carpool, Taxi, Uber, etc. 
# + Train
# + Bus
# + Motorbike: Motorbike / scooter
# + Others: Other, ON LEAVE - DAY OFF, Worked remotely, Worked Remotely, Worked<U+00A0>remotely
# + DELETE Others rows

melt_data$TravelMethod[which((melt_data$TravelMethod %in% c("Drove car alone","Carpool","Taxi, Uber, etc.")))] <- "Car"
melt_data$TravelMethod[which((melt_data$TravelMethod %in% c("Other","ON LEAVE - DAY OFF",
                                                                "Worked remotely","Worked Remotely","Worked<U+00A0>remotely")))] <- "Other"
melt_data$TravelMethod[which(substr(melt_data$TravelMethod,1,6) %in% c("Worked"))] <- "Other"
melt_data$TravelMethod[which(melt_data$TravelMethod %in% c("Motorbike / scooter"))] <- "Motorbike"
# Remove NAs and Other values
dim(melt_data)
melt_data <- na.omit(melt_data)
melt_data <- melt_data[-which(melt_data$TravelMethod %in% "Other"),]
dim(melt_data)

# Check frequency of new travel types
freq_travel <- count(melt_data,melt_data$TravelMethod)
freq_travel <- as.data.frame(freq_travel)
colnames(freq_travel) <- c("TravelMethod","Frequency")
freq_travel <- freq_travel[order(-freq_travel$Frequency),]
#View(freq_travel)
barplot(freq_travel$Frequency,names.arg = freq_travel$TravelMethod,
        xlab="Travel Method", ylab="Number of Commuters",main="Numbers of travellers by Travel method")

# Join with postcode table to get commuter postcode Longtitude and Lattitude
mdata_full <- melt_data %>% left_join(mpostcodes,by=c("Postcode"="Pcode"))
dim(mdata_full)
str(mdata_full)

unique(mdata_full$TravelMethod)
unique(mdata_full$Day)



# Peak hour each day
unique(mdata_full$ArrivalTimeRange)
freq_time <- count(mdata_full,mdata_full$ArrivalTimeRange)
colnames(freq_time) <- c("Hour","Frequency")
barplot(freq_time$Frequency,names.arg = freq_time$Hour)

# Peak hour Monday
mdata_full_mon <- mdata_full[which(mdata_full$Day %in% "Mon"),]
freq_time <- count(mdata_full_mon,mdata_full_mon$ArrivalTimeRange)
colnames(freq_time) <- c("Hour","Frequency")
barplot(freq_time$Frequency,names.arg = freq_time$Hour)

ggplot(mdata_full,aes(ArrivalTimeRange)) + geom_freqpoly() +
  facet_wrap(~Day)

ggplot(mdata_full, aes(ArrivalTimeRange, full=Day,colour=Day)) + 
  geom_density(alpha=0.1)


# Store csv file for Tablue analysis
write.csv(mdata_full,file = "MqConnectData.csv",row.names = FALSE)

# Arriving Peak hours is from 6am - 10am
##########################################################################
str(mdata_full)
# Print Map: Where travellers come from
loc_center <- geocode("2 Byfield St, Macquarie Park NSW 2113")
loc_center <- as.numeric(loc_center)
macqpark <- get_map(location=loc_center,zoom=11)
macq_map <- ggmap(macqpark,extent="device",legend="topleft")
macq_map +
  stat_density2d(
    aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = mdata_full,
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "black", high = "red") + 
  facet_wrap(~Day)
# Monday
macq_map +
  stat_density2d(
    aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = mdata_full_mon,
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~TravelMethod)

# Monday + only use public transport: Train and Bus
mdata_full_mon_pub <- mdata_full_mon[which(mdata_full_mon$TravelMethod %in% c("Train","Bus")),]
macq_map +
  stat_density2d(
    aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = mdata_full_mon_pub,
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~TravelMethod) + 
  theme(legend.position="bottom")

# Monday + only use Car
mdata_full_mon_car <- mdata_full_mon[which(mdata_full_mon$TravelMethod %in% c("Car")),]
macq_map +
  stat_density2d(
    aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = mdata_full_mon_car,
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~TravelMethod)



?stat_density2d

################################################################
# Print Map

loc <- geocode("21 Byfield St, Macquarie Park NSW 2113")
loc <- as.numeric(loc)
loc
macqpark <-get_map(location=loc,zoom=11)
macqmap <- ggmap(macqpark, extent = "device", legend = "topleft")

# Check from 5am, 6am, 7am
sub_data <- mdata_full[which(mdata_full$hours %in% c("05","06","07")),]
macqmap +
  stat_density2d(
    aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = sub_data,
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "black", high = "red") + 
  facet_wrap(~hours) + 
  theme(legend.position="bottom")


#############################
# Test qmap

help(qmap)

qmap('2 Lyonpark Rd Macquarie Park NSW 2113', zoom = 15, maptype = 'hybrid') +
  geom_polygon(aes(x = Long, y = Lat, group = Area), data = data,
               colour = 'white', fill = 'black', alpha = .4, size = .3)

# qmap(location = "Macquarie university")
# qmap(location = "Macquarie university", zoom = 14)
# qmap(location = "20 ethel street, eastwood, nsw, 2122", zoom = 14, source = "osm")
# qmap(location = "baylor university", zoom = 14, source = "osm", scale = 20000)
# qmap(location = "baylor university", zoom = 14, maptype = "satellite")
# qmap(location = "baylor university", zoom = 14, maptype = "hybrid")
# qmap(location = "baylor university", zoom = 14, maptype = "toner", source = "stamen")
# qmap(location = "baylor university", zoom = 14, maptype = "watercolor", source = "stamen")
# qmap(location = "Macquarie university", zoom = 14, maptype = "terrain-background", source = "stamen")
# qmap(location = "baylor university", zoom = 14, maptype = "toner-lite", source = "stamen")
