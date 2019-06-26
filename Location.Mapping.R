
# Mapping postcodes, longtitude and lattitude to map

require(ggplot2)


mpostcodes <- read_csv("D:/Drive/Persona/MIT Study/R Projects/Statistical Learning/data/MPID_Postcodes.csv")

dim(mpostcodes)
mpostcodes <- mpostcodes[,1:6]
str(mpostcodes)
mpostcodes <- as.data.frame(mpostcodes)
str(mpostcodes)

poacoord <- data.frame('poa'=mpostcodes$Pcode, 'lat'=mpostcodes$Lat, 'long'=mpostcodes$Long)

# Ratio Aus born vs elsewhere born
ratio <- data.frame('poa'=rep(10,100),'Freq'=rep(20,100))


popcount <- na.omit(merge(ratio, poacoord, all=FALSE))
popcount <- popcount[rev(order(popcount$Freq)),] # Place the small bubbles on top
popcount$cat <- sapply(popcount$Freq, function(x) if (x < 6) {'> 1:6'} else {'< 1:6'})


ggplot(popcount, aes(x=long, y=lat, colour=cat)) +
  scale_size(range = c(1,20), name = 'Population') +
  geom_point() +
  coord_equal()


#install.packages("ggmap")
library(ggmap)

set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('ryde', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')

paris <- get_map(location = "paris")
str(paris)
ggmap(paris, extent = "normal")

########################################################
## Crime in houston

str(crime)
dim(crime)
qmap('houston', zoom = 13)
gglocator(2)

violent_crimes <- subset(crime,offense != "auto theft" & offense != "theft" & offense != "burglary")
dim(violent_crimes)
str(violent_crimes)
head(violent_crimes)
# Oder violent crimes
violent_crimes$offense <- factor(violent_crimes$offense, levels = c("robbery", "aggravated assault", "rape", "murder"))
theme_set(theme_bw(16))
HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")
HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense, size = offense),
             data = violent_crimes)
HoustonMap +
  stat_bin2d(
    aes(x = lon, y = lat, colour = offense, fill = offense),
    size = .5, bins = 30, alpha = 1/2,
    data = violent_crimes
  )

dim(violent_crimes)
head(violent_crimes)

head(violent_crimes)
houston <- get_map("houston", zoom = 14)
HoustonMap <- ggmap(houston, extent = "device", legend = "topleft")

HoustonMap +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = violent_crimes,
    geom = "polygon"
  )

# overlay <- stat_density2d(
#   aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
#   bins = 4, geom = "polygon",
#   data = violent_crimes
# )
# HoustonMap + overlay + inset(
#   grob = ggplotGrob(ggplot() + overlay + theme_inset()),
#   xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062
# )

houston <-get_map(location="houston",zoom=14,color="bw",
                  source="osm")
HoustonMap <- ggmap(houston,base_layer = ggplot(aes(x=lon,y=lat), data = violent_crimes))
HoustonMap +
  stat_density2d(aes(x=lon,y=lat,fill=..level.., alpha=..level..),
                 bins=5, geom = "polygon", date=violent_crimes) +
  scale_fill_gradient(low="black",high="red")  + 
  facet_wrap(~day)

geocode("Macquarie university", output = "more")

gc <- geocode("baylor university")
(gc <- as.numeric(gc))
revgeocode(gc)
gc2 <- geocode("Macquarie university")
gc2
(gc2 <- as.numeric(gc2))
revgeocode(gc2)

gc3 <- geocode("20 Ethel st, eastwood, nsw, 2122")
gc3

gc4 <-geocode("27 Abuklea Rd, Marsfield NSW,2122")
gc4


revgeocode(gc2, output="more")
from <- c("houston","houston","dallas")
to <- c("waco,texas","san antonio","houston")
mapdist(from,to)

######## FROM 10 ethel to macquarie
from <- c("10 ethel street, eastwood, NSW")
to <- c("Macquarie University")
mapdist(from,to)


# Check limit of google query count
distQueryCheck()
.GoogleDistQueryCount


legs_df <- route(
  'marrs mclean science, baylor university',
  '220 south 3rd street, waco, tx 76701',
  alternatives = FALSE)

legs_df
qmap('420 clay avenue, waco, tx', zoom = 15, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')

qmap('416 clay avenue, waco, tx', zoom = 15, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') 
  



legs_df2 <- route(from,to,alternatives = TRUE)
legs_df2
qmap('94 Balaclava Rd Marsfield NSW 2122', zoom = 15, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df2)) +
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df2
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')

qmap('166 Balaclava Rd Marsfield NSW 2122', zoom = 15, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df2))+
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df2
  )+
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')



#### SHAPE FILE ####
# get an example shape file
download.file('http://www.census.gov/geo/cob/bdy/tr/tr00shp/tr48_d00_shp.zip',
              destfile = 'census.zip')

