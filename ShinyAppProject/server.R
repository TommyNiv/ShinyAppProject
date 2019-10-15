#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(tidyr)
library(magrittr)
library(stringr)
library(googleVis)
library(rgeos)
library(geosphere)
library(plotly)
library(gapminder)
library(devtools)
library(GGally)
library(scales)
library(htmlwidgets)
library(gridExtra)
library(rgdal)
library(htmltools)
library(geojson)
library(geojsonio)
library(data.table)


#import filtered Manhattan and Brooklyn Listings

#listingsManBk <- read.csv("./listingsManBK.csv",stringsAsFactors = F)

#nycNeighborhoods <- geojsonio::geojson_read("./neighbourhoods.geojson", what = "sp")

priceByhood <- listingsManBK %>% group_by(neighborhood = hood) %>% 
    summarise(meanPrice = mean(price,na.rm = T), totalListings = n(),meanRating = mean(review_scores_location, na.rm = T))


colnames(priceByhood) <- c("region","Mean_Price", "Total_Listings","Mean_Rating")
priceByhood$region <- as.character(priceByhood$region)

nychoods2 <- subset(nycNeighborhoods, is.element(nycNeighborhoods$neighbourhood,priceByhood$region))
nychoods3 <- subset(nychoods2, !duplicated(nychoods2$neighbourhood))


#priceByhood2 <-priceByhood[order(match(priceByhood$region, nychoods3$neighbourhood)),]
#Trains Stations
trainStationTraffic <- listingsManBK %>% group_by(Station_Name) %>% summarise(totalAirBnbServed = n(), avgPricenearStop = mean(price))

tranStationTraffic2 <- left_join(listingsManBK,trainStationTraffic, by = "Station_Name")

#Create Binds for leaflet maps
bins <- c(1,100,200,300,400,500)
bins2 <- c(9.0,9.2,9.4,9.6,9.8,10.0)


#Create Palette for Choropleth leaflet neighborhood Price head map
pal <- colorBin("Reds", domain = priceByhood2$Mean_Price, bins = bins)
pal2 <- colorBin("Greens", domain = priceByhood2$Mean_Rating, bins = bins2)

manSummary = listingsManBK %>% filter(boro == "Manhattan") %>% 
    group_by(Neighborhood = hood) %>% 
    summarise(Avg_Price_Per_Night = dollar(mean(price,na.rm = T)), 
              Total_Listings = n(),
              Mean_Location_Rating = round(mean(review_scores_location,na.rm=T),2)) %>% 
    arrange(desc(Avg_Price_Per_Night))

# Define server logic for Shiny App
#Functionality incuded - Choropleth map, normalized bar graph for price and neighborhood score, data table
shinyServer(function(input, output) {

   data_input <- reactive({
       listingsManBK %>% 
           group_by(neighborhood = hood) %>% 
           summarise(meanPrice = mean(price,na.rm = T), totalListings = n(),meanRating = mean(review_scores_location, na.rm = T))
   })
   
   
  
   #data_input_subset1 <- reactive({
    #   subset(nycNeighborhoods, is.element(nycNeighborhoods$neighbourhood,data_input()$neighborhood))
   #}) 
  
   #data_input_subset2 <- reactive({
    #   subset(data_input_subset1(), !duplicated(data_input_subset1$neighbourhood))
   #})
   
   data_input_ordered <- reactive({
       data_input()[order(match(data_input()$neighborhood, nychoods3$neighbourhood)),]
   })
   
   labels <- reactive({
       labels <- paste("<p>Neighborhood: ",  data_input_ordered()$neighborhood,
                       "<p>", "Avg Price Per Night: $", round(data_input_ordered()$meanPrice,2),
                       sep ="",
                       "<p>", "Total Listings: ", data_input_ordered()$totalListings, "</p>")
   })
   
   labels2 <- reactive({
       labels2 <- paste("<p>Neighborhood: ",  data_input_ordered()$neighborhood,
                       "<p>", "Avg Neighborhood Score: ", round(data_input_ordered()$meanRating,2),
                       sep ="",
                       "<p>", "Avg Price Per Night: $", round(data_input_ordered()$meanPrice,2),
                       "<p>", "Total Listings: ", data_input_ordered()$totalListings, "</p>")
   })
   
   #renderLeaflet map for price by neighbor, create choropleth map for neighborhood prices in Manhattan and brooklyn
   output$nycMap <- renderLeaflet(
       if(input$graphchoice == "Price"){
           if(input$layerchoice == "None"){
               leaflet() %>% 
                   setView(-73.85,40.77,10.5, 2) %>% 
                   addProviderTiles(providers$HikeBike.HikeBike) %>% 
                   addPolygons(data = nychoods3,
                               weight = 1,
                               smoothFactor = 0.5,
                               color = "white",
                               fillOpacity = 0.8,
                               fillColor = pal(data_input_ordered()$meanPrice),
                               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#666666",
                                   #dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                               ),
                               label = lapply(labels(), HTML)) %>% 
                   addLegend(pal = pal,
                             values = data_input_ordered()$meanPrice,
                             opacity=0.5,
                             position = "topright",
                             title = "Avg. Price Per Night") 
           }else if(input$layerchoice == "Subways"){
               leaflet() %>% 
                   setView(-73.85,40.77,10.5, 2) %>% 
                   addProviderTiles(providers$HikeBike.HikeBike) %>% 
                   addPolygons(data = nychoods3,
                               weight = 1,
                               smoothFactor = 0.5,
                               color = "white",
                               fillOpacity = 0.8,
                               fillColor = pal(data_input_ordered()$meanPrice),
                               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#666666",
                                   #dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                               ),
                               label = lapply(labels(), HTML)) %>% 
                   addLegend(pal = pal,
                             values = data_input_ordered()$meanPrice,
                             opacity=0.5,
                             position = "topright",
                             title = "Avg. Price Per Night") %>% 
                   addCircles(lng = tranStationTraffic2$Station.Longitude, lat = tranStationTraffic2$Station.Latitude,
                              popup = paste("<dl><dt>Station:", tranStationTraffic2$Station_Name, '</dt>',
                                            "<dt>AirBNBs that rely on Staion:", tranStationTraffic2$totalAirBnbServed,'</dt>',
                                            "<dt>Avg Price near Stop:", dollar(tranStationTraffic2$avgPricenearStop),'</dt>',
                                            "Service Lines:", tranStationTraffic2$LinesServiced),
                              radius = tranStationTraffic2$totalAirBnbServed * 0.4,
                              #color = tranStationTraffic2$Station_Name,
                              stroke = F,
                              weight = 1,
                              fillOpacity = 0.01)
           }else if(input$layerchoice == "Listings"){ #add listings cluster graph
               leaflet() %>% 
                   setView(-73.85,40.77,10.5, 2) %>% 
                   addProviderTiles(providers$HikeBike.HikeBike) %>% 
                   addPolygons(data = nychoods3,
                               weight = 1,
                               smoothFactor = 0.5,
                               color = "white",
                               fillOpacity = 0.8,
                               fillColor = pal(data_input_ordered()$meanPrice),
                               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#666666",
                                   #dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                               ),
                               label = lapply(labels(), HTML)) %>% 
                   addLegend(pal = pal,
                             values = data_input_ordered()$meanPrice,
                             opacity=0.5,
                             position = "topright",
                             title = "Avg. Price Per Night") %>% 
                   addMarkers(lng = listingsManBK$longitude,
                              lat = listingsManBK$latitude,
                              labelOptions = labelOptions(noHide = F),
                              clusterOptions = markerClusterOptions(),
                              popup = paste0("<b> Name: </b>", listingsManBK$name,
                                             "<br/><b> Listing ID: </b>", listingsManBK$id,
                                             "<br> <b> Price: $</b>", listingsManBK$price,
                                             "<br/><b> Room Type: </b>",listingsManBK$room_type,
                                             "<br/><b> Number of Rooms: <b>", listingsManBK$bedrooms,
                                             "<br/><b> Property Type: </b>",listingsManBK$property_type)) 
           }
       }else{ #plot by Area Score
           if(input$layerchoice == "None"){#Check Layer
               leaflet() %>% 
                   setView(-73.85,40.77,10.5, 2) %>% 
                   addProviderTiles(providers$HikeBike.HikeBike) %>% 
                   addPolygons(data = nychoods3,
                               weight = 1,
                               smoothFactor = 0.5,
                               color = "white",
                               fillOpacity = 0.8,
                               fillColor = pal2(data_input_ordered()$meanRating),
                               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#666666",
                                   #dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                               ),
                               label = lapply(labels2(), HTML)) %>% 
                   addLegend(pal = pal2,
                             values = data_input_ordered()$meanRating,
                             opacity=0.5,
                             position = "topright",
                             title = "Avg. Neighborhood Rating")
           }else if(input$layerchoice == "Subways"){
               leaflet() %>% 
                   setView(-73.85,40.77,10.5, 2) %>% 
                   addProviderTiles(providers$HikeBike.HikeBike) %>% 
                   addPolygons(data = nychoods3,
                               weight = 1,
                               smoothFactor = 0.5,
                               color = "white",
                               fillOpacity = 0.8,
                               fillColor = pal2(data_input_ordered()$meanRating),
                               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#666666",
                                   #dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                               ),
                               label = lapply(labels2(), HTML)) %>% 
                   addLegend(pal = pal2,
                             values = data_input_ordered()$meanRating,
                             opacity=0.5,
                             position = "topright",
                             title = "Avg. Neighborhood Rating") %>%
                   addCircles(lng = tranStationTraffic2$Station.Longitude, lat = tranStationTraffic2$Station.Latitude,
                              popup = paste("<dl><dt>Station:", tranStationTraffic2$Station_Name, '</dt>',
                                            "<dt>AirBNBs that rely on Staion:", tranStationTraffic2$totalAirBnbServed,'</dt>',
                                            "<dt>Avg Price near Stop:", dollar(tranStationTraffic2$avgPricenearStop),'</dt>',
                                            "Service Lines:", tranStationTraffic2$LinesServiced),
                              radius = tranStationTraffic2$totalAirBnbServed * 0.4,
                              #color = tranStationTraffic2$Station_Name,
                              stroke = F,
                              weight = 1,
                              fillOpacity = 0.01)
           }else if(input$layerchoice == "Listings"){
               leaflet() %>% 
                   setView(-73.85,40.77,10.5, 2) %>% 
                   addProviderTiles(providers$HikeBike.HikeBike) %>% 
                   addPolygons(data = nychoods3,
                               weight = 1,
                               smoothFactor = 0.5,
                               color = "white",
                               fillOpacity = 0.8,
                               fillColor = pal2(data_input_ordered()$meanRating),
                               highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#666666",
                                   #dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                               ),
                               label = lapply(labels2(), HTML)) %>% 
                   addLegend(pal = pal2,
                             values = data_input_ordered()$meanRating,
                             opacity=0.5,
                             position = "topright",
                             title = "Avg. Neighborhood Rating") %>% 
                   addMarkers(lng = listingsManBK$longitude,
                              lat = listingsManBK$latitude,
                              labelOptions = labelOptions(noHide = F),
                              clusterOptions = markerClusterOptions(),
                              popup = paste0("<b> Name: </b>", listingsManBK$name,
                                             "<br/><b> Listing ID: </b>", listingsManBK$id,
                                             "<br> <b> Price: $</b>", listingsManBK$price,
                                             "<br/><b> Room Type: </b>",listingsManBK$room_type,
                                             "<br/><b> Number of Rooms: <b>", listingsManBK$bedrooms,
                                             "<br/><b> Property Type: </b>",listingsManBK$property_type)) 
               } 
           
       }
       
       )#,
   
   #Create ggplot bargraph of standardized relative pricing by Neighborhood. 
   #Returns bar graph of ordered neighborhoods from high to low relative to borough average
   
   ##Created factored SD dataframe
   data_input_boro_avg <- reactive({
       listingsManBK  %>% 
           filter(boro == input$boroughs) %>% 
           summarise(avgPriceboro = mean(price, na.rm=T))
   })
   
   data_input_boro_sd <- reactive({
       listingsManBK  %>% 
           filter(boro == input$boroughs) %>% 
           summarise(sdboro = sd(price,na.rm=T))
   })
   
   data_input_by_boro <- reactive({
       listingsManBK %>% filter(boro == input$boroughs) %>% 
           group_by(hood) %>%  summarise(avgPhood = mean(price),
                                         avgArea = mean(review_scores_location,na.rm = T)) %>% 
           mutate(relativePrice = round(((avgPhood - data_input_boro_avg()$avgPriceboro)/data_input_boro_sd()$sdboro),2))
   })
   
   
   data_input_by_boro_relative <- reactive({
          plot_data <- data_input_by_boro() %>% 
           mutate(relative = ifelse(relativePrice < 0, "below","above")) %>% 
           arrange(relativePrice)
          plot_data$hood <- factor(plot_data$hood,levels = plot_data$hood)
          return(plot_data)
   })
   
   #create bar graph based on avg night price
       output$nycNeighborhoods <- renderPlot({
           ggplot(data_input_by_boro_relative(), aes(x = hood, y = relativePrice, label = relativePrice)) +
               geom_bar(stat = 'identity', aes(fill = relative), width =.5) +
               scale_fill_manual(name = "Price",
                                 labels = c("Above Average","Below Average"),
                                 values = c("above"="#00ba38", "below" = "#f8766d"))+
               labs(subtitle=paste("Normalised price for", input$boroughs, "- Neighborhoods. Avg Price Per Night: $", round(data_input_boro_avg(),2),
                                   ", Neighborhoods Standard Deviation Per Night: $", round(data_input_boro_sd(),2)),
                    title = "How Much More Per Night (Standard Deviations)?") + coord_flip() +
               xlab("Neighborhood") +ylab("Standard Deviations") 
       })
       
 
       ##Created factored SD dataframe for location scores
       data_boro_area_avg <- reactive({
           listingsManBK  %>% 
               filter(boro == input$boroughs) %>% 
               summarise(avgNeiScoreboro = mean(review_scores_location,na.rm = T))
       })
       
       data_area_sd <- reactive({
           listingsManBK  %>% 
               filter(boro == input$boroughs) %>% 
               summarise(sdboro = sd(review_scores_location, na.rm = T))
       })
       
       data_input_by_boro_area <- reactive({
           listingsManBK %>% filter(boro == input$boroughs) %>% 
               group_by(hood) %>%  summarise(avghood = mean(price,na.rm=T),
                                             avgArea = mean(review_scores_location,na.rm = T)) %>% 
               mutate(relativeScore2 = round(((avgArea - data_boro_area_avg()$avgNeiScoreboro)/data_area_sd()$sdboro),2))
       })
       
       
       data_input_by_boro_relative_area <- reactive({
           plot_data2 <- data_input_by_boro_area() %>% 
               mutate(relative2 = ifelse(relativeScore2 < 0, "below","above")) %>% 
               arrange(relativeScore2)
           plot_data2$hood <- factor(plot_data2$hood,levels = plot_data2$hood)
           return(plot_data2)
       })
             
#create bar graph based on avg area score       
       output$nycNeighborhoodsScore <- renderPlot({
           ggplot(data_input_by_boro_relative_area(), aes(x = hood, y = relativeScore2, label = relativeScore2)) +
               geom_bar(stat = 'identity', aes(fill = relative2), width =.5) +
               scale_fill_manual(name = "Neighborhood Score",
                                 labels = c("Above Average","Below Average"),
                                 values = c("above"="#00ba38", "below" = "#f8766d"))+
               labs(subtitle=paste("Normalised score for", input$boroughs, "- Neighborhoods. Avg Neighborhood Score: ", round(data_boro_area_avg(),2),
                                   ", Neighborhoods Standard Deviation is: ", round(data_area_sd(),2)),
                    title = "How Good Is the Location (Standard Deviations)?") + coord_flip() +
               xlab("Neighborhood") +ylab("Standard Deviations") 
       })
       
       #data_input2 <- reactive({
        #   listingsManBK %>% filter(hood == "Manhattan")
        #       group_by(neighborhood = hood) %>% 
         #      summarise(meanPrice = mean(price,na.rm = T), totalListings = n(), meanRating = mean(review_scores_location))
       #})
       
       #data_input2 <- reactive({
        #   listingsManBK %>% filter(boro == "Manhattan") %>% 
         #      group_by(Neighborhood = hood) %>% 
          #     summarise(Avg_Price_Per_Night = dollar(mean(price,na.rm = T)), 
           #              Total_Listings = n(),
            #             Mean_Location_Rating = round(mean(review_scores_location,na.rm=T),2)) %>% 
             #  arrange(desc(Avg_Price_Per_Night))
       #})
       
       output$nycSummary <- renderDataTable(
               data_input2()
           )
    
       ##
       data_input2 <- reactive({
           listingsManBK %>% filter(boro == input$boroughs) %>% #boro == "Manhattan
               group_by(Neighborhood = hood) %>% 
               summarise(Avg_Price_Per_Night = round(mean(price,na.rm = T),2), 
                         Total_Listings = n(),
                         Mean_Location_Rating = round(mean(review_scores_location,na.rm=T),2),
                         Max_Price = round(max(price,na.rm=T),2),
                         Min_Price = round(min(price,na.rm=T),2),
                         Avg_Number_BedRooms = round(mean(bedrooms,na.rm=T),3)) %>% 
               arrange(desc(Avg_Price_Per_Night))
       })
       
       ##Output for Data Table of Borough Summary Statistics
       output$nycSummary <- renderDataTable(
           data_input2()
       )
       
       
       ###############################
       ##Output Info Boxes    
       output$maxbox <- renderInfoBox({
           max_value <- max(data_input2()$Avg_Price_Per_Night)
           max_hood <- data_input2()$Neighborhood[data_input2()$Avg_Price_Per_Night == max_value]
           infoBox("Highest Avg Per Night",max_hood,dollar(max_value),icon = icon("search-dollar"))
       })
       
       output$minbox <- renderInfoBox({
           min_value <- min(data_input2()$Avg_Price_Per_Night)
           min_hood <- data_input2()$Neighborhood[data_input2()$Avg_Price_Per_Night == min_value]
           infoBox("Lowest Avg Per Night",min_hood,dollar(min_value),icon = icon("search-dollar"))
       })
       
       output$maxlocation <- renderInfoBox({
           max_value <- max(data_input2()$Mean_Location_Rating)
           max_hood <- data_input2()$Neighborhood[data_input2()$Mean_Location_Rating == max_value]
           infoBox("Location High",max_hood,max_value,icon = icon("search-location"))
       })
       
       output$minlocation <- renderInfoBox({
           min_value <- min(data_input2()$Mean_Location_Rating)
           min_hood <- data_input2()$Neighborhood[data_input2()$Mean_Location_Rating == min_value]
           infoBox("Location Low", min_hood,min_value,icon = icon("search-location"))
       })
       
       output$pctGreaterthan200<- renderInfoBox({
           totalNei <- NROW(data_input2()$Avg_Price_Per_Night)
           totalgreaterThan200<- sum(data_input2()$Avg_Price_Per_Night > 200)
           pctGreater = round(totalgreaterThan200/totalNei,4)
           infoBox("% Greater Than $200/N", percent(pctGreater),icon = icon("funnel-dollar"))
       })
       
       output$avgNumBr<- renderInfoBox({
           totAvgNumBr<- mean(data_input2()$Avg_Number_BedRooms)
           infoBox("Average Number of Bedrooms Offered (Entire Borough)", 
                   round(totAvgNumBr,2),icon = icon("bed"))
       })
       

})
