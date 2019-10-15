
library(tidyr)
library(magrittr)
library(stringr)
library(leaflet)
library(googleVis)
library(rgeos)
library(geosphere)
library(plotly)
library(gapminder)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(devtools)
library(choroplethrZip)
library(GGally)
library(scales)
library(htmlwidgets)
library(gridExtra)
library(rgdal)
library(htmltools)
library(geojson)
library(geojsonio)

### Pull in Cleaned Data Set Joined with SubWay Data

listingsClean <- read.csv("./listingsNew.csv", stringsAsFactors = F)

listingsClean <- listingsClean %>% 
  mutate(host_is_superhost = ifelse(host_is_superhost =="", "f",host_is_superhost))


#Clean distance to nearest subway
#############################################################
listingsClean <- listingsClean %>% 
  mutate(distToNearSub = ifelse(boro =="Staten Island", 0,distToNearSub)) #staten island does not have trains

listingsClean <- listingsClean %>% mutate(distToNearSub = ifelse(distToNearSub > 2640, 0, distToNearSub)) #checking to see if distance is greater than .5 mile
listingsClean <- listingsClean %>% mutate(distToNearSub = na_if(distToNearSub,0))

#Clean distance to second nearest subway
###########################################################
listingsClean <- listingsClean %>% 
  mutate(distToNearSub2 = ifelse(boro =="Staten Island", 0,distToNearSub2)) #staten island does not have trains

listingsClean <- listingsClean %>% mutate(distToNearSub2 = ifelse(distToNearSub2 > 2640, 0, distToNearSub2)) #checking to see if distance is greater than .5 mile
listingsClean <- listingsClean %>% mutate(distToNearSub2 = na_if(distToNearSub2,0))



#Scatter Plot to check price vs distance in feet from nearest subway
p <- ggplot(data = subset(listingsClean, !is.na(distToNearSub)), 
            aes(x = price, y = distToNearSub, 
                color = boro, 
                text = paste("Nearest subway line:", 
                             LinesServiced,
                            "Neighborhood:", hood, sep = "\n"))) +
  geom_point(alpha = (1/3)) + scale_x_log10()  

p <- ggplotly(p)
#p

cor(listingsClean$price,listingsClean$distToNearSub, use = "complete.obs")
colnames(listingsClean)

linearMod <- lm(price ~ distToNearSub, data=listingsClean)

linearMod
summary(linearMod)

#?cor
#leaflet()
#leaflet() %>% addTiles()
#leaflet() %>% addTiles() %>%
#  addMarkers(map = ., 
#             lng = listingsClean$Station.Longitude,
#             lat = listingsClean$Station.Latitude,
             #old line
#             popup = listingsClean$Station_Name)

print(object.size(listingsClean),units="Mb")
head(listingsClean)


head(listingsClean)

hoodsNearSubway <- listingsClean %>% group_by(boro,hood, LinesServiced) %>% 
  summarise(avgDistance = mean(distToNearSub,na.rm = T), 
            avgDistance2 = mean(distToNearSub2, na.rm = T), 
            avgPrice = mean(price), 
            totalListings=n())

head(hoodsNearSubway)


manBKhoodsNearSubway <- hoodsNearSubway %>%  filter(boro == "Manhattan" | boro == "Brooklyn")

top10hoodsByDistance <- manBKhoodsNearSubway %>%  
  filter(!is.na(avgDistance)) %>% 
  arrange(avgDistance) %>% top_n(10,avgDistance)


top10hoodsByPrice <- manBKhoodsNearSubway %>%  
  filter(!is.na(avgDistance)) %>% 
  arrange(desc(avgPrice)) %>% top_n(10,avgPrice)

head(top10hoodsByDistance)

head(top10hoodsByPrice)



stationsTop <- listingsClean %>% filter(boro == "Manhattan" | boro == "Brooklyn") %>% 
  group_by(Station_Name,hood, LinesServiced) %>% 
  summarise( totalListingsServiced=n(), avgPrice = mean(price)) %>% arrange(desc(totalListingsServiced)) %>% 
  top_n(10,totalListingsServiced)

stationsBottom <- listingsClean %>% filter(boro == "Manhattan" | boro == "Brooklyn") %>% 
  group_by(Station_Name,hood, LinesServiced) %>% 
  summarise( totalListingsServiced=n(), avgPrice = mean(price)) %>% arrange(totalListingsServiced) %>% 
  top_n(10,totalListingsServiced)

head(stationsTop)
head(stationsBottom)

Stationshood <- listingsClean %>% 
  filter(boro == "Manhattan" | boro == "Brooklyn") %>% 
  group_by(Station_Name,hood, LinesServiced) %>%
  filter(hood == "Midtown") %>% 
  summarise( totalListingsServiced=n(), avgPrice = mean(price)) %>% arrange(desc(totalListingsServiced)) %>% 
  top_n(10,totalListingsServiced)
head(Stationshood)

totalAirBnb = listingsClean %>% summarise(totalListings = n())
topBoro <- listingsClean %>% group_by(boro) %>% summarise(avgPrice = mean(price), totalList = n())
totalAirBnb

topBoro

topBoro <- topBoro %>%  mutate(pctOfTotal = percent(totalList/totalAirBnb$totalListings))
topBoro

ggplot(topBoro,aes(x = boro, y = totalList)) +geom_bar(stat = "identity") + ylab("Total Listings") +
  xlab("Borough") + geom_text(aes(label = pctOfTotal), vjust =-.5)

listingsManBK <- listingsClean %>%  filter(boro == "Manhattan" | boro == "Brooklyn")


write.csv(listingsManBK, "listingsManBK.csv")

leaflet(listingsManBK) %>% 
  addTiles() %>% 
  addMarkers(~longitude,~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),
             popup = paste0("<b> Name: </b>", listingsManBK$name,
                            "<br/><b> Listing ID: </b>", listingsManBK$id,
                            "<br> <b> Price: </b>", listingsManBK$boro,
                            "<br> <b> Price$: </b>", dollar(listingsManBK$price),
                            "<br/><b> Room Type: </b>",listingsManBK$room_type,
                            "<br/><b> Number of Rooms: <b>", listingsManBK$bedrooms,
                            "<br/><b> Property Type: </b>",listingsManBK$property_type)) %>% 
  setView(-74.00,40.71,zoom = 12) %>% 
  addProviderTiles("CartoDB.Positron")




#####Create Zip Code Heat Map of Avg Prices

listingsManBK <- listingsManBK %>% filter(zipcode != "")
listingsManBK <- listingsManBK %>% filter(zipcode != " ")
listingsManBK <- listingsManBK %>% mutate(zipcode = str_remove_all(zipcode,"NY "))
listingsManBK <- listingsManBK %>% mutate(zipcode = substr(zipcode,1,5))
listingsManBK <- listingsManBK %>% filter(str_length(zipcode) == 5)
listingsManBK <- listingsManBK %>% filter(zipcode != "91766")
listingsManBK <- listingsManBK %>% filter(zipcode != "10129")
listingsManBK <- listingsManBK %>% filter(zipcode != "10270")
listingsManBK <- listingsManBK %>% filter(zipcode != "10281")
#listingsDF <- listingsDF %>% filter(zipcode != "10249") #can test to add back
listingsManBK <- listingsManBK %>% filter(zipcode != "11249")
listingsManBK <- listingsManBK %>% filter(zipcode != "10705") #Yonkers
listingsManBK <- listingsManBK %>% filter(zipcode != "10550") #Mt Vernon is not NY as much they want to be
listingsManBK <- listingsManBK %>% filter(zipcode != "11003") #Elmont
listingsManBK <- listingsManBK %>% filter(zipcode != "11559") #Nasau County LI na Fam
listingsManBK$priceNew <- as.numeric(gsub(",","",substring(listingsManBK$price,2)))

priceByZip <- listingsManBK %>% group_by(zipcode = zipcode) %>% 
  summarise(medianPrice = mean(price,na.rm = T), totalListings = n())

priceByZip

colnames(priceByZip) <- c("region","Mean_Price", "Total_Listings")
priceByZip$region <- as.character(priceByZip$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)

nycZipShp <- readOGR("./nyc_zip_code_tabulation_areas_polygons.shp")

class(nycZipShp)

nycZipShp2 <- subset(nycZipShp, is.element(nycZipShp$postalcode,priceByZip$region))
nycZipShp3 <- subset(nycZipShp2, !duplicated(nycZipShp2$postalcode))
#is.element(priceByZip$region,nycZipShp2$postalcode)



priceByZip2 <- priceByZip[order(match(priceByZip$region, nycZipShp3$postalcode)),]
class(priceByZip2)

is.element(nycZipShp2$postalcode,priceByZip2$region)
is.element(priceByZip$region,nycZipShp2$postalcode)
min(priceByZip2$Mean_Price)
max(priceByZip2$Mean_Price)


#bins first break down
#bins <- c(50,100,150,200,250,300,350,400,450,500)

bins <- c(1,100,200,300,400,500)

#old color
#pal <- colorBin("RdYlBu", domain = priceByZip2$value, bins = bins)

#new color
pal <- colorBin("Reds", domain = priceByZip2$Mean_Price, bins = bins)

labels <- paste("<p>Zip Code: ", priceByZip2$region, "</p>",
                "<p>", "Avg Price: $", round(priceByZip2$Mean_Price,2), "</p>",
                sep ="",
                "<p>", "Total Listings: ", priceByZip2$Total_Listings, "</p>")

m <- leaflet() %>% 
  setView(-73.85,40.77,10.5, 2) %>% 
  addProviderTiles(providers$HikeBike.HikeBike) %>% 
  addPolygons(data = nycZipShp3,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(priceByZip2$Mean_Price),
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666666",
                #dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = lapply(labels, HTML)) %>% 
  addLegend(pal = pal,
            values = priceByZip2$Mean_Price,
            opacity=0.5,
            position = "topright",
            title = "Avg. Price Per Night")
m


nycNeighborhoods2 <- geojsonio::geojson_read("./neighbourhoods.geojson", what = "sp")

class(nycNeighborhoods)
class(nycNeighborhoods2)
#is.element(nycNeighborhoods$neighbourhood,
           
priceByhood <- listingsManBK %>% group_by(neighborhood = hood) %>% 
             summarise(meanPrice = mean(price,na.rm = T), totalListings = n())
           
priceByhood
           
colnames(priceByhood) <- c("region","Mean_Price", "Total_Listings")
priceByhood$region <- as.character(priceByhood$region)

is.element(nycNeighborhoods$neighbourhood, priceByhood$region)
is.element(priceByhood$region, nycNeighborhoods$neighbourhood)

nychoods2 <- subset(nycNeighborhoods, is.element(nycNeighborhoods$neighbourhood,priceByhood$region))
nychoods3 <- subset(nychoods2, !duplicated(nychoods2$neighbourhood))
           
           
priceByhood2 <-priceByhood[order(match(priceByhood$region, nychoods3$neighbourhood)),]
class(priceByhood2)
           
is.element(nychoods3$neighbourhood,priceByhood2$region)
is.element(priceByhood2$region,nychoods3$neighbourhood)
min(priceByhood2$Mean_Price)
max(priceByhood2$Mean_Price)
           
           

           
bins <- c(1,100,200,300,400,500)
           
           #old color
           #pal <- colorBin("RdYlBu", domain = priceByZip2$value, bins = bins)
           
           #new color
pal <- colorBin("Reds", domain = priceByhood2$Mean_Price, bins = bins)
           
labels <- paste("<p>Neighborhood: ", priceByhood2$region,
                           "<p>", "Avg Price: $", round(priceByhood2$Mean_Price,2),
                           sep ="",
                           "<p>", "Total Listings: ", priceByhood2$Total_Listings, "</p>")
           
hoodMap <- leaflet() %>% 
  setView(-73.85,40.77,10.5, 2) %>% 
  addProviderTiles(providers$HikeBike.HikeBike) %>% 
  addPolygons(data = nychoods3,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(priceByhood2$Mean_Price),
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666666",
                #dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = lapply(labels, HTML)) %>% 
  addLegend(pal = pal,
            values = priceByhood2$Mean_Price,
            opacity=0.5,
            position = "topright",
            title = "Avg. Price Per Night")
hoodMap  


hoodMap <- leaflet() %>% 
  setView(-73.85,40.77,10.5, 2) %>% 
  addProviderTiles(providers$HikeBike.HikeBike) %>% 
  addPolygons(data = nychoods3,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(priceByhood2$Mean_Price),
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666666",
                #dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = lapply(labels, HTML)) %>% 
  addLegend(pal = pal,
            values = priceByhood2$Mean_Price,
            opacity=0.5,
            position = "topright",
            title = "Avg. Price Per Night")

hoodMap 


trainStationTraffic <- listingsManBK %>% group_by(Station_Name) %>% summarise(totalAirBnbServed = n(), avgPricenearStop = mean(price))

tranStationTraffic2 <- left_join(listingsManBK,trainStationTraffic, by = "Station_Name")


head(tranStationTraffic2)


trainsServiced <- leaflet() %>% 
  setView(-73.85,40.77,10.5, 2) %>% 
  addProviderTiles(providers$HikeBike.HikeBike) %>%
  addCircles(lng = tranStationTraffic2$Station.Longitude, lat = tranStationTraffic2$Station.Latitude,
             popup = paste("<dl><dt>Station:", tranStationTraffic2$Station_Name, '</dt>',
                           "AirBNBs that rely on Staion:", tranStationTraffic2$totalAirBnbServed,'</dt>',
                           "<dt>Avg Price near Stop:", dollar(tranStationTraffic2$avgPricenearStop),'</dt>',
                           "Service Lines:", tranStationTraffic2$LinesServiced),
             radius = tranStationTraffic2$totalAirBnbServed * 0.4,
             #color = tranStationTraffic2$Station_Name,
             stroke = F,
             weight = 1,
             fillOpacity = 0.01)
trainsServiced  

##Relative value
## This normalizes average price by neighborhood and returns a bar graph showing the neighborhood st deviations
## from the borough average

borough <- "Manhattan"
avgPriceMan <- listingsManBK  %>% filter(boro == "Manhattan") %>% summarise(avgPriceMan = mean(price))
sdMan <- listingsManBK  %>% filter(boro == "Manhattan") %>% summarise(sdMan = sd(price))

listingsMan <- listingsManBK %>% filter(boro == "Manhattan") %>% 
  group_by(hood) %>%  summarise(avgPhood = mean(price),
                                avgArea = mean(review_scores_location,na.rm = T)) %>% 
  mutate(relativePrice = round(((avgPhood - avgPriceMan$avgPriceMan)/sdMan$sdMan),2)) #%>% 
#select(hood,avg, relativePrice) 

head(listingsMan)

listingsMan <- listingsMan %>% mutate(relative = ifelse(relativePrice < 0, "below","above")) %>% arrange(relativePrice)
listingsMan$hood <- factor(listingsMan$hood, levels = listingsMan$hood)

class(listingsMan)
class(listingsManBK)
head(listingsMan)


###plot by relative price
ggplot(listingsMan, aes(x = hood, y = relativePrice, label = relativePrice)) +
  geom_bar(stat = 'identity', aes(fill = relative), width =.5) +
  scale_fill_manual(name = "Price",
                    labels = c("Above Average","Below Average"),
                    values = c("above"="#00ba38", "below" = "#f8766d"))+
  labs(subtitle=paste("Normalised price for", borough,"Neighborhoods. Avg Price Per Night: $", round(avgPriceMan,2)),
       title = "Diverging Bars") + coord_flip() +xlab("Neighborhood") + ylab("Standard Deviations") 
 

?factor


##Plot by Location score
avgScoreMan <- listingsManBK  %>% filter(boro == "Manhattan") %>% summarise(avgNeiScore = mean(review_scores_location,na.rm=T))
sdManNeigh <- listingsManBK  %>% filter(boro == "Manhattan") %>% summarise(sdMan = sd(review_scores_location,na.rm=T))


#Get the # of standarddeviations for relative neighborhood score
listingsManNeigh <- listingsManBK %>% filter(boro == "Manhattan") %>% 
  group_by(hood) %>%  summarise(avgArea = mean(review_scores_location,na.rm = T)) %>% 
  mutate(relativeScore = (avgArea - avgScoreMan$avgNeiScore)/sdManNeigh$sdMan) #%>% 
#select(hood,avg, relativePrice) 

head(listingsManNeigh)
class(listingsManNeigh$avgArea)

#classify neighborhood as above or below average
listingsManNeigh <- listingsManNeigh %>% mutate(relative = ifelse(relativeScore < 0, "below","above")) %>% arrange(relativeScore)
listingsManNeigh$hood <- factor(listingsManNeigh$hood, levels = listingsManNeigh$hood)

head(listingsManNeigh)
class(listingsManNeigh$relativeScore)
class(avgScoreMan$avgNeiScore)
###plot by relative price
ggplot(listingsManNeigh, aes(x = hood, y = relativeScore, label = relativeScore)) +
  geom_bar(stat = 'identity', aes(fill = relative), width =.5) +
  scale_fill_manual(name = "Neighborhood Score",
                    labels = c("Above Average","Below Average"),
                    values = c("above"="#00ba38", "below" = "#f8766d"))+
  labs(subtitle=paste("Normalised score for", borough,"Neighborhoods. Avg Neighborhood Score: ", round(avgScoreMan,2)),
       title = "Diverging Bars") + coord_flip() +xlab("Neighborhood") + ylab("Standard Deviations") 

write.csv(listingsManBK, "listingsManBK.csv")

listingsJustBK <- listingsManBK %>% filter(boro == "Brooklyn")
listingsJustMan <- listingsManBK %>% filter(boro == "Manhattan")

##impact on brooklyn subways
BkSubWay <- lm(price ~ distToNearSub, listingsJustBK)
BkSubWay
summary(BkSubWay)

AllSubWay <- lm(price ~ distToNearSub, listingsManBK)
AllSubWay
summary(AllSubWay)

AllSubWayLoc <- lm(review_scores_location ~ distToNearSub, listingsManBK)
AllSubWayLoc
summary(AllSubWayLoc)

cor(listingsManBK$review_scores_location,listingsManBK$distToNearSub, use = "complete.obs")

ManSubway <- lm(price ~ distToNearSub, listingsJustMan)
ManSubway
summary(ManSubway)
lm(price ~ distToNearSub, listingsJustMan)differFeet
differFeetYr <- differFeet * 200
differFeetYr
