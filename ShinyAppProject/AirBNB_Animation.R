install.packages("ggmap")
install.packages("zipcode")
install.packages("lubridate")
install.packages("grDevices")
install.packages("Cairo")
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(maps)
library(ggmap)
library(lubridate)
library(data.table)
library(sp)
library(scales)
library(stringr)
library(zipcode)
library(dplyr)
library(tidyr)
library(zipcode)
library(grDevices)
library(Cairo)


#listings <- read.csv("./Data_CSVs/Air_BNB/listings.csv", stringsAsFactors = F)

head(listings$city)
str(listings)
colnames(listings)
unique(listings$neighbourhood_group_cleansed)

nyc_map <- get_map(c(left = -74.194098, bottom = 40.55, right = -73.762397, top = 40.888809), 
                  maptype ="toner-lite", source = "stamen")


ggmap(nyc_map)

listAnimation <- listings
class(listAnimation$host_since)
listAnimation$host_since[1]



#transofrm integer column host_since to as.Date format
#listAnimation2 <- listAnimation %>% mutate(chHost_since = as.Date(as.character(host_since), "%d/%m/%Y", tz=NULL))
#listAnimation2 <- listAnimation %>% mutate(chHost_since = as.character(host_since))
listAnimation2 <- listAnimation %>% mutate(chHost_since = as.Date(host_since, origin = "1900-01-01"))

class(listAnimation$host_since)
class(listAnimation2$host_since)
head(listAnimation2$host_since)
class(listAnimation2$chHost_since)
head(listAnimation2$chHost_since)

listAnimation <- listAnimation2 %>% 
                        mutate(join_year = year(chHost_since), join_month = month(chHost_since),
                               join_date = as.Date(paste(join_month, 1, join_year, sep = "/"), '%m/%d/%Y'))

class(listAnimation$join_date)

li_data_summary <- listAnimation %>% 
                        count(join_year, join_month) %>%  ungroup() %>% 
                        arrange(join_year, join_month) %>% 
                        mutate(cumm_n = cumsum(n))

li_data_summary <- li_data_summary[complete.cases(li_data_summary),]                      

li_data_summary <- inner_join(li_data_summary, select(listAnimation, zipcode, latitude, longitude, join_year, join_month, join_date, neighbourhood_group_cleansed),
                              by = c("join_year" = "join_year", "join_month" = "join_month"))

g <- ggmap(nyc_map, darken = c("0.8", "black"))

unique(li_data_summary$join_date)

my_plot <- function(df, plotdate, borough){
  g <- ggmap(nyc_map, darken = c("0.8", "black"))
  #Create 2 dataframes where smaller plots are all the airBNBs prior to current month, 2nd df is the current month additions
    old_df <- filter(df, join_date < plotdate,neighbourhood_group_cleansed == borough )
    new_df <- filter(df, join_date == plotdate, neighbourhood_group_cleansed == borough)
    g <- g + geom_point(data = old_df, aes(x = longitude, y = latitude), size = 2, color = "dodgerblue", alpha = 0.2)
    g <- g + geom_point(data = new_df, aes(x = longitude, y = latitude), size = 5, color = "dodgerblue", alpha = 0.2)
    g <- g + theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_blank())
    g <- g + annotate("text", x = -74.15, y = 40.85, label = "YEAR:", color = "white", size = rel(5), hjust = 0)
    g <- g + annotate("text", x = -74.15, y = 40.839, label = unique(new_df$join_year), color = "white", size = rel(6), fontface = 2, hjust = 0)
    g <- g + annotate("text", x = -74.15, y = 40.825, label = "LISTING COUNT:", color = "white", size = rel(5), hjust = 0)
    g <- g + annotate("text", x = -74.15, y = 40.814, label = comma(unique(new_df$cumm_n)), color = "white", size = rel(6), fontface = 2, hjust = 0)
    g <- g + annotate("text", x = -74.15, y = 40.80, label = "Borough:", color = "white", size = rel(5), hjust = 0)
    g <- g + annotate("text", x = -74.15, y = 40.79, label = unique(new_df$neighbourhood_group_cleansed), color = "white", size = rel(6), fontface = 2, hjust = 0)
  g
}

unique(li_data_summary$neighbourhood_group_cleansed)
my_plot(li_data_summary,"2011-11-01","Manhattan")

my_zip_plot <- function(df, plotdate, mapid){
  g <- ggmap(nyc_map, darken = c("0.8", "black"))
  #split the dataframe for all airbnb in NYC before the plot date
  old_df <- filter(df, join_date < plotdate)
  #split the dataframe for all airbnb in NYC for the plot date
  new_df <- filter(df, join_date == plotdate)
  #plot all the AirBNB before the current month. Make all the older store locations as shown as smaller circles
  g <- g + geom_point(data = old_df, aes(x = longitude, y = latitude), size = 2, color = "dodgerblue", alpha = 0.2)
  #plot add the AirBNB during the current month. Make the current month AirBNB larger
  g <- g + geom_point(data = new_df, aes(x = longitude, y = latitude), size = 5, color = "dodgerblue", alpha = 0.2)
  #remove axis marks, labels, and titles
  g <- g + theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_blank())
  #place the label for year
  g <- g + annotate("text", x = -74.15, y = 40.85, label = "YEAR:", color = "white", size = rel(5), hjust = 0)
  #place the value for the year
  g <- g + annotate("text", x = -74.15, y = 40.839, label = unique(new_df$join_year), color = "white", size = rel(6), fontface = 2, hjust = 0)
  #place the label for total AirBNB
  g <- g + annotate("text", x = -74.15, y = 40.825, label = "LISTING COUNT:", color = "white", size = rel(5), hjust = 0)
  #place the total AirBNB listed
  g <- g + annotate("text", x = -74.15, y = 40.814, label = comma(unique(new_df$cumm_n)), color = "white", size = rel(6), fontface = 2, hjust = 0)
  #generate the file name for the map. Using str_pad to make the filename same length and prefixed with zeroes.
  #create a maps directory inside the directory of this script
  filename <- paste0("/maps/img_", str_pad(mapid, 7, pad = "0"), ".png")
  ggsave(filename = filename, plot = g, width = 13, height = 7, dpi = 150, type = "cairo-png")
}



unique(li_data_summary$join_month)
colnames(li_data_summary)


#dir.create("/maps")
#dir.create("maps")

##creates png files of maps for each month 2008-2019 - takes a long time to run....stand back and wait.....

li_data_summary %>% 
  #was using group_indices_
  mutate(mapid = group_indices_(li_data_summary, .dots = 'join_date')) %>% 
  group_by(join_date) %>% 
  do(pl = my_zip_plot(li_data_summary, unique(.$join_date), unique(.$mapid)))

#makemovie_cmd <- paste0("C:/ffmpeg/ffmpeg/bin/ffmpeg -framerate 5 -y -i ", 
#                         "C:/maps/img_%7d.png",  
#                        " -c:v libx264 -pix_fmt yuv420p ",  
#                        "C:/Users/tnivo/OneDrive/Desktop/R/Project/maps/movie.mp4")
system(makemovie_cmd)
getwd()

makemovie_cmd
