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

#Didn't end up using the subset of the counties because I couldn't get the over function to only display markers in my subsetted counties 
#Subsetting counties to Southwest counties 
#swcounty <- c("Armstrong", "Allegheny", "Beaver", "Cambria", "Fayette", "Greene", "Indiana", "Somerset", "Washington", "Westmoreland")
#pa_swcounty <- pacounty[pacounty$NAME %in% swcounty,]

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

#Another attempt to get the map API reactive function to work 
#getEsri <- function(url) {
  # Make Call
  #g <- GET(URLencode(url))
  #c <- content(g)
  #readOGR(c)
#}

#Creating county column for Environmental Good Samaritan Act points (goodact)
surfacemine$county <- over(surfacemine, pa_swcounty, fn = NULL)

#Header of the shiny dashboard 
header <- dashboardHeader(title = "Permits in PA")

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
    #Content for graphs page 
    tabItem("activepermit",
            fluidPage(
              box(tabPanel("Bar Plot", plotlyOutput("permitbar")), width = 12),
              box(tabPanel("Pie Chart", plotlyOutput("permitpie")), width = 12)
              )
            
            ),
    #Contents for dataset page 
    tabItem("permittable",
            fluidPage(
              inputPanel(
                downloadButton("downloadData", "Download Active Permit Data")
              ),
              box(title = "Abandoned Mine Land Dataset", DT::dataTableOutput("permittable"), width = 12)
              )
            ),
    #Content for map page 
    tabItem("permit",
            fluidRow(
              box(
                selectInput("facility",
                            "Type of Facility for Markers:",
                            choices = sort(unique(surfacemine$PRIMARY_FACILITY_KIND)),
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
  
  #Icons for the markers 
  icons <- awesomeIconList(
    makeAwesomeIcon(icon = "leaf", library = "fa", markerColor = "green")
  )
  
  #Map for permits and reclamation sites 
  output$permitmap <- renderLeaflet({
    #facilitymarker <- facilityInput()
    leaflet() %>% 
      addPolygons(data = pacounty,
                  weight = 2,
                  color = "black") %>%
      addPolygons(data = permits,
                  weight = 1.5,
                  color = "red") %>%
      #Data for the markers should be facilitymarker however I wasn't able to get the reactive function to work so I changed the data source so that at least you can see my map in the dashboard 
      addAwesomeMarkers(data = surfacemine, icon = ~icons, popup = ~SITE_NAME) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Gray Canvas", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "Dark Matter", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topography", options = providerTileOptions(noWrap = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Gray Canvas", "Dark Matter", "Topography"),
        options = layersControlOptions(collapsed = TRUE)
      ) 
  })
  
  #Pie chart for active permit data 
  output$permitpie <- renderPlotly({
    permit <- permitInput()
    plot_ly(data = permit, labels = permit$COUNTY, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent', showlegend = TRUE)
  })
  
  #Bar plot for Active Permits 
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
  
  #Data table for permit table 
  output$permittable <- DT::renderDataTable({
    subset(permitInput(), select = c("MINE", "OPERATOR", "TYPE", "STATUS", "COAL_SEAM", "COUNTY"))
  })
  
  #Download button for the data table 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sw_permitdata", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(permitInput(), file) 
    }
  )
  
  #Allows for the reset button to work 
  observeEvent(input$reset, {
    updateSelectInput(session, "type", selected = c("Room and Pillar"))
    updateSelectInput(session, "counties", selected = c("Cambria","Somerset"))
    updateSelectInput(session, "coal", selected = c("Pittsburgh"))
    showNotification("You have successfully reset the filters", type = "message")
  })
  
  #Attempt at reactive function for the map. All the commented out urls are my attempts to fix it. I just left them there so that you can see that I tried a bunch of different things 
  facilityInput <- reactive({
    filter <- ifelse(length(input$facility) > 0, 
                           paste0("%20IN%20(%27", paste(input$facility, collapse = "%27AND%27"),"%27"),"")
    #url <- URLencode(paste0('http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/25/query?where=PRIMARY_FACILITY_KIND', gsub(" ", "+", input$facility), "&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=json"))
    #marker <- getEsri(url) %>%
      #spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    #return(marker)
  
     url <- paste0('http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/25/query?where=PRIMARY_FACILITY_KIND',filter, '&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=geojson')
    #url <- paste0('http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/25/query?where=1%', filter,'&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=geojson')
    print(url)
    facilityfilter <- readOGR(url)
    return(facilityfilter)
  })
}
#http://www.depgis.state.pa.us/arcgis/rest/services/emappa/eMapPA_External_Extraction/MapServer/25/query?where=1%3D1&text=&objectIds=&time=&geometry=%7B%22xmin%22%3A-10315563.459876563%2C%22ymin%22%3A4644636.53163017%2C%22xmax%22%3A-6971902.094570702%2C%22ymax%22%3A5378432.0031676665%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=geojson
#Runs the application 
shinyApp(ui = ui, server = server)
