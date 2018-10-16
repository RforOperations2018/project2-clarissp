require(shiny)
require(rgdal)
require(leaflet)
require(leaflet.extras)
require(dplyr)
require(readxl)
require(stringr)
require(shinydashboard)
require(reshape2)
require(dplyr)
require(ggplot2)
require(plotly)
require(shinythemes)
require(RSocrata)
require(httr)

#Importing token for the API 
token <- jsonlite::fromJSON("token.json")$token

#Reading in the county boundaries for all of PA using the SODA API 
county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.geojson")


#Subset the counties for the 10 Southwest county region that I need
sw_county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.geojson?$where=county_nam in('ALLEGHENY', 'ARMSTRONG','BEAVER','BUTLER','CAMBRIA','FAYETTE','GREENE','INDIANA','LAWERENCE','SOMERSET','WASHINGTION','WESTMORELAND')")
sw_county

#Header of the shiny dashboard 
header <- dashboardHeader(title = "Siting Grid-Scale Solar in Pennsylvania")

#Sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  id = "tabs",
  #Pages in the sidebar 
  menuItem("Environmental Resilient Network", tabName = "nature"),
  menuItem("Abandoned Mine Lands", tabName = "mines")
)

#Body of the shiny dashboard 
body <- dashboardBody(
  tabItems(
    tabItem("nature"),
    tabItem("mines")
    )
  )

ui <- dashboardPage(header, sidebar, body)

#Defines server logic
server <- function(input, output, session = session)

#Runs the application 
shinyApp(ui = ui, server = server)