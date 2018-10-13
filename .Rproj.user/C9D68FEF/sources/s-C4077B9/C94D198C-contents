#Project 1 Update to attempt to do HW 4
#Using Shinydashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinythemes)
library(RSocrata)
library(httr)

pdf(NULL)

#Imports the token 
token <- jsonlite::fromJSON("token.json")$token

borosubset <- read.socrata("https://data.cityofnewyork.us/resource/ki38-k49c.json?$select=comm_dist_boro, comm_district, ipv_rape, ipv_fel_assault, ipv_dir", app_token = token)

boro1 <- as.character(unique(borosubset$comm_dist_boro))
district <- as.numeric(unique(borosubset$comm_district))
assault <- as.numeric(unique(borosubset$ipv_fel_assault))
rape <- as.numeric(unique(borosubset$ipv_rape))
  
remove(borosubset)

#Creates the header of the shiny dashboard 
header <- dashboardHeader(title = "NYC Crime Dashboard",
                          #Creates the content for the notifications menu
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Data updated on September 6, 2018",
                                                        icon = icon("calendar")
                                                        ),
                                       notificationItem(text = "Joint Interest Areas removed from data",
                                                        icon = icon("scissors")
                                       )
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 70, color = "green",
                                                "Downloading Data")
                          ),
                          #Creates the content for the messages menu
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "NYC Open Data",
                                         message = HTML("This data is collected by The Mayor's <br> Office to Combat Domestic Violence"),
                                         icon = icon("institution"),
                                         time = "3:55"
                                         ),
                                       messageItem(
                                         from = "New User",
                                         message = HTML("Is it possible to get notified when <br> the data is updated?"),
                                         icon = icon("question"),
                                         time = "4:00"
                                       )
                          )
)

#Creates the sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    #Creates pages in the sidebar
    menuItem("Felony Assault Incidents", icon = icon("balance-scale"), tabName = "assault"),
    menuItem("Rape Incidents", icon = icon("balance-scale"), tabName = "rape"),
    menuItem("Complete Dataset", icon = icon("database"), tabName = "table"),
    #Creates borough select input
    selectInput("boro",
                "Boroughs:",
                choices = sort(boro1),
                multiple = TRUE,
                selected = TRUE),
    #Creates Community District slider input
    sliderInput("district",
                "Community Districts:",
                min = min(district, na.rm = T),
                max = max(district, na.rm = T),
                value = c(min(district, na.rm = T),
                          max(district, na.rm = T)),
                step = 1),
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
  )
)

#Creates the body of the shiny dashboard
body <- dashboardBody(tabItems(
  #Creates the felony assault page
  tabItem("assault",
          fluidRow(
            valueBoxOutput("incidentsBox"),
            valueBoxOutput("assaultBox")
          ),
          fluidRow(
            box(
              sliderInput("assaultcount",
                          HTML("Change the Range of <br> Felony Assault Incidents <br> in the Box Plot:"),
                          min = min(assault, na.rm = T),
                          max = max(assault, na.rm = T),
                          value = c(min(assault, na.rm = T), max(assault, na.rm = T)),
                          step = 10) 
            ),
              tabBox(width = 12,
                     tabPanel("Bar Plot", plotlyOutput("assaultbarplot")),
                     tabPanel("Box Plot", plotlyOutput("assaultboxplot"))
              )
            )
  ),
  tabItem("rape",
          fluidRow(
            valueBoxOutput("rapeincidentsBox"),
            valueBoxOutput("rapeBox")
          ),
          fluidRow(
            box(
              sliderInput("rapecount",
                          HTML("Change the Range of <br> Rape Incidents in the Box Plot:"),
                          min = min(rape, na.rm = T),
                          max = max(rape, na.rm = T),
                          value = c(min(rape, na.rm = T), max(rape, na.rm = T)),
                          step = 1)
            ),
            tabBox(width = 12,
                 tabPanel("Bar Plot", plotlyOutput("rapebarplot")),
                 tabPanel("Box Plot", plotlyOutput("rapeboxplot"))
            )
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Complete Intimate Partner Violence Dataset", DT::dataTableOutput("table"), width = 12
                )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

# Define server logic
server <- function(input, output, session = session) {
  #Reactive function for all pages (global inputs)
  partnerInput <- reactive({
    # Allows for the slider input to be reactive
    if (length(input$district) > 0 ) {
      #I think this is line is the reason my code is not working but I have tried various attempts to fix it but still have had no luck fixing it
      partner <- read.socrata(paste0("https://data.cityofnewyork.us/resource/ki38-k49c.json?$select=comm_district >= '", input$district[1],
                             comm_district, input$district[2]), app_token = token)
    }
    
    # Allows for the select input to be reactive
    if (length(input$boro) > 0 ) {
      #Same comment about the reason my code is not working 
      partner <- read.socrata(paste0("https://data.cityofnewyork.us/resource/ki38-k49c.json?$select=comm_dist_boro >= '", 
                                     input$boro, comm_dist_boro), app_token = token)
    }
    
    #Attempt to subset my data, not sure if this worked either 
    partner <- select(partner,c("comm_district_boro", "comm_district", "ipv_fel_assault", "ipv_rape"))
  
    return(partner)
  })
  
  #Reactive Function for Assaults Page (local input)
  partnerAssaults <- reactive({
    if (length(input$assaultcount) > 0 ) {
      #Same comment about the reason my code is not working 
      partner <- read.socrata(paste0("https://data.cityofnewyork.us/resource/ki38-k49c.json?$select=ipv_fel_assault >= '", 
                                     input$assaultcount[1], ipv_fel_assault, input$assaultcount[2]), app_token = token)
    }
    
    #Allows for the slider input of felony assaults to be reactive for the boxplot
    return(partner)
  })
  
  #Reactive Function for Rape Page (local input)
  partnerRapes <- reactive({
    if (length(input$rapecount) > 0 ) {
      partner <- read.socrata(paste0("https://data.cityofnewyork.us/resource/ki38-k49c.json?$select=ipv_rape >= '", 
                                     input$rapecount[1], "ipv_rape<='", input$rapecount[2]), app_token = token)
    }
    #Allows for the slider input of felony assaults to be reactive for the boxplot
      return(partner)
  })
  
  #Bar plot for felony assault that involved a family member by borough 
  output$assaultbarplot <- renderPlotly({
    partner <- partnerInput() # Then this needs to be partnerAssaults
    ggplot(data = partner, aes(x = comm_dist_boro, y =ipv_fel_assault)) +
      geom_bar(stat="identity", fill = "#bcbddc") + theme_bw()
  })
  
  #Box plot for felony assault that involved a family member by borough 
  output$assaultboxplot <- renderPlotly({
    partnerAssaults <- partnerAssaults() # Same as above
    ggplot(data = partnerAssaults) +
      geom_boxplot(mapping = aes(x= comm_dist_boro, y = ipv_fel_assault)) + theme_bw()
  })
  
  #Bar plot for rape that involved a family member by borough 
  output$rapebarplot <- renderPlotly({
    partner <- partnerInput() # Change to other function
    ggplot(data = partner, aes(x = comm_dist_boro, y =ipv_rape)) +
      geom_bar(stat="identity", fill = "#ffeda0") + theme_bw()
  })
  
  #Box plot for rape that involved a family member by borough 
  output$rapeboxplot <- renderPlotly({
    partnerRapes <- partnerRapes() # See above
    ggplot(data = partnerRapes) +
      geom_boxplot(mapping = aes(x= comm_dist_boro, y = ipv_rape)) + theme_bw()
  })
  
  #Data table of whole dataset
  output$table <- DT::renderDataTable({
    subset(partnerInput(), select = c(comm_dist_boro, comm_district, ipv_fel_assault, ipv_rape, ipv_dir))
  })
  
  #Value box of average incidents 
  output$incidentsBox <- renderValueBox({
    partner <- partnerInput()
    incidents <- round(mean(partner$ipv_dir, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Incidents Reported", value = incidents , icon = icon("gavel"), color = "teal")
  })
  
  #Value box of average felony assaults
  output$assaultBox <- renderValueBox({
    partner <- partnerInput()
    assault <- round(mean(partner$ipv_fel_assault, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Felony Assault Incidents Reported", value = assault, icon = icon("gavel"), color = "purple")
  })
  
  #Value box of average incidents box, this is the same number as my first value box, the reason I have two is so that users can have a reference
  #of the average number of incidents on each page to compare to rape incidents to 
  output$rapeincidentsBox <- renderValueBox({
    partner <- partnerInput()
    incidents <- round(mean(partner$ipv_dir, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Incidents Reported", value = incidents , icon = icon("gavel"), color = "teal")
  })
  
  #Value box of average rapes
  output$rapeBox <- renderValueBox({
    partner <- partnerInput()
    rape <- round(mean(partner$ipv_rape, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Rape Incidents Reported", value = rape, icon = icon("gavel"), color = "yellow")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "boro", selected = c(""))
    updateSliderInput(session, "district", value = c(min(district, na.rm = T), max(district, na.rm = T)))
    updateSliderInput(session, "assaultcount", value = c(min(assault, na.rm = T), max(assault, na.rm = T)))
    updateSliderInput(session, "rapecount", value = c(min(rape, na.rm = T), max(rape, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
