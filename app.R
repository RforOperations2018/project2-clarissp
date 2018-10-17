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
require(jsonlite)

#Importing token for the API 
#token <- jsonlite::fromJSON("token.json")$token

#Reading in the county boundaries for all of PA using the SODA API 
#county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.json")


#Subset the counties for the 10 Southwest county region that I need
#sw_county <- read.socrata("https://data.pa.gov/resource/n96m-gp6j.json?$where=county_nam in('ALLEGHENY', 'ARMSTRONG','BEAVER','BUTLER','CAMBRIA','FAYETTE','GREENE','INDIANA','LAWERENCE','SOMERSET','WASHINGTION','WESTMORELAND')")
#sw_county

getEsri <- function(url) {
  g <- GET(URLencode(url))
  c <- content(g)
}

getEsriList <- function(url) {
  g <- GET(URLencode(url))
  fromJSON(content(g))$features %>% 
    unlist() %>%
    unname()
}

url <- URLencode("http://data-padep-1.opendata.arcgis.com/datasets/cea4b401782a4178b139c6b5c6a929f2_48.geojson?where=%20(COUNTY%20like%20'%25Indiana%25'%20OR%20COUNTY%20like%20'%25Somerset%25'%20OR%20COUNTY%20like%20'%25Greene%25'%20OR%20COUNTY%20like%20'%25Armstrong%25'%20OR%20COUNTY%20like%20'%25Washington%25'%20OR%20COUNTY%20like%20'%25Westmoreland%25'%20OR%20COUNTY%20like%20'%25Beaver%25')%20&geometry={"xmin":-9690178.58343066,"ymin":4758001.876679025,"xmax":-8018347.90077773,"ymax":5124899.612447773,"spatialReference":{"wkid":102100}}")

#Reading in Abandoned Mine Lands data from DEP 
aml <- read.csv("Abandoned_Mine_Land_Inventory_Polygons.csv") 
#amlsubset <- subset(aml, select = c("SF_TYPE", "SF_STATUS_CD", "SF_STATUS", "SF_PRIORITY", "SF_PROBLEM_CODE", "HEIGHT_FT", "VOLUME_CY"))
#amlsubset

#Header of the shiny dashboard 
header <- dashboardHeader(title = "Siting Grid-Scale Solar in Pennsylvania")

#Sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    #Pages in the sidebar 
    menuItem("Resilient Network", icon = icon("leaf"), tabName = "nature"),
    menuItem("Abandoned Mine Lands", icon = icon("globe"), tabName = "mines"),
    menuItem("Abandoned Mine Lands Dataset", icon = icon("database"),tabName = "minetable"),
    
    #Select input for Type of AMLs
    selectInput("amltype",
                "Abandoned Mine Land Type(s):",
                choices = sort(unique(aml$SF_TYPE)),
                multiple = TRUE,
                selected = c("Coal Deep Mine", "Coal Surface Mine")
                ),
    
    #Slider input for Height of AMLS 
    sliderInput("height",
                "Height of Abandoned Mine Lands:",
                min = min(aml$HEIGHT_FT, na.rm = T),
                max = max(aml$HEIGHT_FT, na.rm = T),
                value = c(min(aml$HEIGHT_FT, na.rm = T),
                          max(aml$HEIGHT_FT, na.rm = T)),
                step = 50),
    #Select input for Counties in Permit Map 
    selectInput("counties",
                "Select a County:",
                choices = County,
                selected = "Somerset"),
    
    #Reset button for filters 
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
  )

)

#Body of the shiny dashboard 
body <- dashboardBody(
  tabItems(
    tabItem("nature"),
    tabItem("mines",
            fluidRow(
              width = 12,
              tabPanel("Bar Plot", plotlyOutput("amlbar"))
              )
            ),
    tabItem("minetable",
            fluidPage(
              box(title = "Abandoned Mine Land Dataset", DT::dataTableOutput("amltable"), width = 12)
              )
            )
    )
  )

ui <- dashboardPage(header, sidebar, body, skin = "black")

#Defines server logic
server <- function(input, output, session = session){
  #Reactive function for all pages (global inputs)
  globalInput <- reactive({
    global <- aml %>%
      filter(HEIGHT_FT >= input$height[1] & HEIGHT_FT <= input$height[2])
             
      if (length(input$amltype) > 0 ) {
        global <- subset(aml, SF_TYPE %in% input$amltype)
      }
    
    url <- URLencode(paste0("http://data-padep-1.opendata.arcgis.com/datasets/cea4b401782a4178b139c6b5c6a929f2_48.geojson?where=%20(COUNTY%20like%20'%25Indiana%25'%20OR%20COUNTY%20like%20'%25Somerset%25'%20OR%20COUNTY%20like%20'%25Greene%25'%20OR%20COUNTY%20like%20'%25Armstrong%25'%20OR%20COUNTY%20like%20'%25Washington%25'%20OR%20COUNTY%20like%20'%25Westmoreland%25'%20OR%20COUNTY%20like%20'%25Beaver%25')%20&geometry={"xmin":-9690178.58343066,"ymin":4758001.876679025,"xmax":-8018347.90077773,"ymax":5124899.612447773,"spatialReference":{"wkid":102100}}"))
    sp <- getEsri(url) %>% 
      spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    return(global)
  })
  
  output$permitmap <- renderLeaflet({
    global <- globalInput()
    leaflet() %>% 
      addPolygons() %>%
  })
  
  output$amlbar <- renderPlotly({
    global <- globalInput()
    ggplot(data = aml, aes(x = SF_PRIORITY, fill = SF_STATUS)) +
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
    subset(globalInput(), select = c("SF_TYPE", "SF_STATUS_CD", "SF_STATUS", "SF_PRIORITY", "SF_PROBLEM_CODE", "HEIGHT_FT", "VOLUME_CY"))
    })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "amltype", selected = c("Coal Deep Mine", "Coal Surface Mine"))
    updateSliderInput(session, "height", value = c(min(aml$HEIGHT_FT, na.rm = T), max(aml$HEIGHT_FT, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}
  

#Runs the application 
shinyApp(ui = ui, server = server)