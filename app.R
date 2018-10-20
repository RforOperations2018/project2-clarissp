#Project 2 

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

#Transofrming projection of counties to match the following two layers 
proj4string(pa_swcounty) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
pa_swcounty <- spTransform(pa_swcounty, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#API for the Permit data 
permits <- readOGR("http://data-padep-1.opendata.arcgis.com/datasets/cea4b401782a4178b139c6b5c6a929f2_48.geojson")
permitcounty <- c("Armstrong", "Beaver", "Cambria", "Greene", "Indiana", "Somerset", "Washington", "Westmoreland")
sw_permits <- permits[permits$COUNTY %in% permitcounty,]

#CSV for Permit data 
permitdata <- read.csv("Active_Underground_Permit_Boundaries.csv") 
sw_permitdata <- filter(permitdata, COUNTY == "Armstrong" | COUNTY == "Beaver" | COUNTY == "Cambria" | COUNTY == "Greene" | COUNTY ==  "Indiana" | COUNTY == "Somerset" | COUNTY ==  "Washington" | COUNTY == "Westmoreland")

#API for Environmental Good Samaritan Act points 
surfacemine <- readOGR("http://data-padep-1.opendata.arcgis.com/datasets/67ed627a525548d5900c1b6964b8e619_25.geojson")

getEsri <- function(url) {
  # Make Call
  g <- GET(URLencode(url))
  c <- content(g)
  readOGR(c)
}

#Creating county column for Environmental Good Samaritan Act points (goodact)
surfacemine$county <- over(surfacemine, pa_swcounty, fn = NULL)

#Header of the shiny dashboard 
header <- dashboardHeader(title = "Permits in Pennsylvania")

#Sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    #Pages in the sidebar 
    menuItem("Graphs", icon = icon("pie-chart"), tabName = "activepermit"),
    menuItem("Dataset", icon = icon("database"),tabName = "permittable"),
    menuItem("Map", icon= icon("map-o"), tabName = "permit"),
    
    #Select input for Type of Permits
    selectInput("type",
                "Permit Type(s):",
                choices = sort(unique(sw_permitdata$TYPE)),
                multiple = TRUE,
                selected = c("Room and Pillar")
                ),
    
    #Select input for Counties of Permits 
    selectInput("counties",
                "Select a County:",
                choices = sort(unique(sw_permitdata$COUNTY)),
                multiple = TRUE,
                selected = c("Cambria","Somerset")),
    #Select input for Status of Permits 
    selectInput("operator",
                "Operator(s) of Permit:",
                choices = sort(unique(sw_permitdata$OPERATOR)),
                multiple = TRUE,
                selected = "Rosebud Mining"),
    
    #Reset button for filters 
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
  )
)

#Body of the shiny dashboard 
body <- dashboardBody(
  tabItems(
    tabItem("activepermit",
            fluidPage(
              box(tabPanel("Bar Plot", plotlyOutput("permitbar")), width = 12),
              box(tabPanel("Pie Chart", plotlyOutput("permitpie")), width = 12)
              )
            
            ),
    tabItem("permittable",
            fluidPage(
              inputPanel(
                downloadButton("downloadData", "Download Active Permit Data")
              ),
              box(title = "Abandoned Mine Land Dataset", DT::dataTableOutput("permittable"), width = 12)
              )
            ),
    tabItem("permit",
            fluidRow(
              box(
                selectInput("facility",
                            "Type of Facility for Markers:",
                            choices = sort(unique(surfacemine$features$attributes$PRIMARY_FACILITY_KIND)),
                            multiple = TRUE,
                            selected = "GROWING GREENER")
              ),
              box(title = "Active Permits in Southwest PA", leafletOutput("permitmap"), width = 12)
            ))
    )
  )

ui <- dashboardPage(header, sidebar, body, skin = "black")

#Defines server logic
server <- function(input, output, session = session){
  #Reactive function for permit types 
  permitInput <- reactive({
      if(length(input$type) > 0 ){
        sw_permitdata <- subset(sw_permitdata, TYPE %in% input$type)
      }
      if(length(input$counties) > 0 ){
        sw_permitdata <- subset(sw_permitdata, COUNTY %in% input$counties)
      }
      if(length(input$operator) > 0 ){
      sw_permitdata <- subset(sw_permitdata, OPERATOR %in% input$operator)
      }
    return(sw_permitdata)
  })
  
  output$permitmap <- renderLeaflet({
    facilitymarker <- facilityInput()
    leaflet() %>% 
      addPolygons(data = pa_swcounty,
                  weight = 2,
                  color = "black") %>%
      addPolygons(data = permits,
                  weight = 1.5,
                  color = "blue") %>%
      addMarkers(data = facilitymarker) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Gray Canvas", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "Dark Matter", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(noWrap = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Gray Canvas", "Dark Matter", "Topography"),
        options = layersControlOptions(collapsed = TRUE)
      ) 
  })
  
  output$permitpie <- renderPlotly({
    permit <- permitInput()
    plot_ly(data = permit, labels = permit$COUNTY, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent', showlegend = TRUE)
  })
  
  output$permitbar <- renderPlotly({
    permit <- permitInput() 
    ggplot(data = permit, mapping = aes(x = COUNTY, fill = STATUS)) +
      geom_bar(stat = "count") +
      labs(title = "Active Underground Permits in Pennsylvania", 
           x= "County", 
           y= "Count of Permits", fill = "Status"
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
                angle = 45, 
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
  
  output$permittable <- DT::renderDataTable({
    subset(permitInput(), select = c("MINE", "OPERATOR", "TYPE", "STATUS", "COAL_SEAM", "COUNTY"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sw_permitdata", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(permitInput(), file) 
    }
  )
  
  observeEvent(input$reset, {
    updateSelectInput(session, "type", selected = c("Room and Pillar"))
    updateSelectInput(session, "counties", selected = c("Cambria","Somerset"))
    updateSelectInput(session, "coal", selected = c("Pittsburgh"))
    showNotification("You have successfully reset the filters", type = "message")
  })
  
  facilityInput <- reactive({
    #filter <- ifelse(length(input$facility) > 0, 
                           #paste0("%20IN%20(%27", paste(input$facility, collapse = "%27,%27"),"%27"),"")
    url <- URLencode(paste0('http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/25/query?where=PRIMARY_FACILITY_KIND', gsub(" ", "+", input$facility), "&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=pjson"))
    marker <- getEsri(url)
    return(marker)
    
    
  
     # url <- paste0('http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/25/query?where=PRIMARY_FACILITY_KIND',filter, '&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=geojson')
    #print(url)
    #facilityfilter <- readOGR(url)
    #return(facilityfilter)
  })
}

#Runs the application 
shinyApp(ui = ui, server = server)