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
county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.json")

#Attempt to subset the counties for the 10 Southwest county region that I need
test_county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.json?$county_nam=ALLEGHENY")
test_county
  
sw_county <- subset(county, county_nam %in% c("ALLEGHENY", "ARMSTRONG","BEAVER","BUTLER","CAMBRIA","FAYETTE","GREENE","INDIANA","LAWERENCE","SOMERSET","WASHINGTION","WESTMORELAND"))
sw_county

