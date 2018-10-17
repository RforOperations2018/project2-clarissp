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


#Subset the counties for the 10 Southwest county region that I need
sw_county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.json?$where=county_nam in('ALLEGHENY', 'ARMSTRONG','BEAVER','BUTLER','CAMBRIA','FAYETTE','GREENE','INDIANA','LAWERENCE','SOMERSET','WASHINGTION','WESTMORELAND')")
sw_county

aml <- read.csv("Abandoned_Mine_Land_Inventory_Polygons.csv")
amlsubset <- subset(aml, select = c("SF_TYPE", "SF_STATUS_CD", "SF_STATUS", "SF_PRIORITY", "SF_PROBLEM_CODE", "HEIGHT_FT", "VOLUME_CY"))
amlsubset

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
    tabItem("mines",
            fluidRow(
              width = 12,
              tabPanel("Bar Plot", plotlyOutput("amlbar")),
              tabPanel("Table", DT::dataTableOutput("amltable"))
              )
            )
    )
  )

ui <- dashboardPage(header, sidebar, body)

#Defines server logic
server <- function(input, output, session = session){
  output$amlbar <- renderPlotly({
    ggplot(data = amlsubset, aes(x = SF_PRIORITY, fill = SF_STATUS)) +
      geom_bar(stat = "count") +
      labs(title = "Abandoned Mine Lands (AML) in Pennsylvania", 
           x= "Priority of AML", 
           y= "Count of AMLs"
      ) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_bw() +
      theme(plot.title = 
              element_text(face = "bold", 
                           family = "American Typewriter"),
            axis.title.x = 
              element_text(
                family = "American Typewriter"
              ),
            axis.text.x = 
              element_text(
                family = "American Typewriter",
                angle = 90, 
                vjust = 0.5
              ),
            axis.title.y = 
              element_text(
                family = "American Typewriter"
              ),
            axis.text.y = 
              element_text(
                family = "American Typewriter"
              ),
            legend.position = "bottom", 
            legend.box = "horizontal"
      )
  })
  
  output$amltable <- DT::renderDataTable({
    amlsubset
    })
}


  
  

#Runs the application 
shinyApp(ui = ui, server = server)