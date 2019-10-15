#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


# Define UI for application 
# This dashboard allows data visualization for AirBNB Data in NYC
shinyUI(dashboardPage(
    skin = "red",
    dashboardHeader(title = "AirBNB in NYC"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("NYC AirBNB Interactive Data", tabName = "AirBNB", icon = icon("chart-pie")),
            menuItem("About", tabName = "about", icon = icon("address-card"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "AirBNB",
            fluidRow(box(width = 3,radioButtons("graphchoice", "View Map By:",
                                                choices = c("Price", "Neighborhood Score"), selected = "Price", inline = T)),
                     box(width = 4,radioButtons("layerchoice", "Add Layer:",
                                                choices = c("None", "Subways", "Listings"), selected = "None", inline = T))),
            fluidRow(box(width =12 , leafletOutput(outputId = "nycMap"))),
            fluidRow(box(width = 3,radioButtons("boroughs", "Select Borough Details:",
                                                choices = c("Manhattan", "Brooklyn"), selected = "Manhattan", inline = T))),
            fluidRow(infoBoxOutput("maxbox"),
                     infoBoxOutput("maxlocation"),
                     infoBoxOutput("pctGreaterthan200"),
                     infoBoxOutput("minbox"),
                     infoBoxOutput("minlocation"),
                     infoBoxOutput("avgNumBr")),
            fluidRow(box(width = 6,plotOutput("nycNeighborhoods")),
                     box(width = 6,plotOutput("nycNeighborhoodsScore"))),
            fluidRow(box(width = 12, dataTableOutput("nycSummary"),heigh = 300))
            
        ),
        tabItem(tabName = "about", "AirBNB has experienced major growth over the past decade in NYC. AirBNBs now total to over 40k unique listings.
                The old saying in real estate is 'Location, Location, Location!', thus, this project looks to explore the relationship
                between AirBNB price and location. Over 85% of all AirBNB listings are in Manhattan and Brooklyn, therefore the data has been trimmed to include
                just observations in those boroughs. The questions this project aims to explore are: 1) What is the pricing landscape for AirBNBs in NYC. 2) Which neighborhoods are the best value for a visitor to NY using AirBNB.
                3) What impact does proximity to Subway Stations have on AirBNB price. I hope you enjoy!")
        )
        )
        
    )
)
