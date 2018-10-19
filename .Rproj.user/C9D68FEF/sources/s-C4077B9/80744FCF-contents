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

#Shapefile for County Boundaries 
pacounty <- readOGR("PA_Counties_clip.shp")

#Subsetting counties to Southwest counties 
swcounty <- c("Armstrong", "Allegheny", "Beaver", "Cambria", "Fayette", "Greene", "Indiana", "Somerset", "Washington", "Westmoreland")
pa_swcounty <- pacounty[pacounty$NAME %in% swcounty,]
proj4string(pa_swcounty) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
pa_swcounty <- spTransform(pa_swcounty, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#API for the Permit data 
permits <- readOGR("http://data-padep-1.opendata.arcgis.com/datasets/cea4b401782a4178b139c6b5c6a929f2_48.geojson")

#API for Environmental Good Samaritan Act points 
goodact <- readOGR("http://data-padep-1.opendata.arcgis.com/datasets/f5487b2bd296492097994a8ab5bd4c9b_261.geojson")

#Creating county column for Environmental Good Samaritan Act points (goodact)
goodact$county <- over(goodact, pa_swcounty, fn = NULL)

#Reading in Abandoned Mine Lands data from DEP 
#aml <- read.csv("Abandoned_Mine_Land_Inventory_Polygons.csv") 
#amlsubset <- subset(aml, select = c("SF_TYPE", "SF_STATUS_CD", "SF_STATUS", "SF_PRIORITY", "SF_PROBLEM_CODE", "HEIGHT_FT", "VOLUME_CY"))
#amlsubset

#Header of the shiny dashboard 
header <- dashboardHeader(title = "Siting Grid-Scale Solar in Pennsylvania")

#Sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    #Pages in the sidebar 
    menuItem("Active Underground Permits", icon = icon("globe"), tabName = "mines"),
    menuItem("Dataset", icon = icon("database"),tabName = "minetable"),
    menuItem("Map", tabName = "permit"),
    
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
                choices = swcounty,
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
            ),
    tabItem("permit",
            fluidPage(
              leafletOutput("permitmap")
            ))
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
    
    if (length(input$counties) > 0 ) {
      global <- subset(pa_swcounty, NAME %in% input$counties)
    }
    
    return(global)
  })
  
  output$permitmap <- renderLeaflet({
    permits <- permits()
    leaflet() %>% 
      addPolygons(data = pa_swcounty,
                  weight = 2,
                  color = "black") %>%
      addPolygons(data = permits,
                  weight = 1.5,
                  color = "blue") %>%
      addMarkers(data = goodact) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Gray Canvas", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "Dark Matter", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(noWrap = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Gray Canvas", "Dark Matter", "Topography"),
        options = layersControlOptions(collapsed = TRUE)
      ) 
  })
  
  output$amlbar <- renderPlotly({
    global <- globalInput()
    ggplot(data = aml, aes(x = SF_PRIORITY, fill = SF_STATUS)) +
      geom_bar(stat = "count") +
      labs(title = "Abandoned Mine Lands (AML) in Pennsylvania", 
           x= "Priority of AML", 
           y= "Count of AMLs", fill = "Status"
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
  
  permits <- reactive({
    filter <- ifelse(length(input$counties) > 0, 
                           paste0("COUNTY+IN%+(%27", paste(input$counties, collapse = "%27,%27"),"%27)"),
                           "")
    #url <- paste0("http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/48/query?where=", filter, "&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=pjson")
    
    url <- paste0("http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/48/query?where=COUNTY+IN+%28%27Armstrong%27%2C+%27Beaver%27%2C+%27Cambria%27%2C+%27Greene%27%2C+%27Indiana%27%2C+%27Somerset%27%2C+%27Washington%27%2C+%27Westmoreland%27%29",filter, "&text=&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=pjson")
    permits <- readOGR(url)
  })
  
  output$amltable <- DT::renderDataTable({
    subset(globalInput(), select = c("SF_TYPE", "SF_STATUS_CD", "SF_STATUS", "SF_PRIORITY", "SF_PROBLEM_CODE", "HEIGHT_FT", "VOLUME_CY"))
    })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "amltype", selected = c("Coal Deep Mine", "Coal Surface Mine"))
    updateSliderInput(session, "height", value = c(min(aml$HEIGHT_FT, na.rm = T), max(aml$HEIGHT_FT, na.rm = T)))
    updateSelectInput(session, "counties", selected = c("Somerset"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}
  

#Runs the application 
shinyApp(ui = ui, server = server)