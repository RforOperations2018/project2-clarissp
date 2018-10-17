#Project 1 
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

#token <- jsonlite::fromJSON("token.json")$token

#boro1 <- as.character(unique(borosubset$comm_dist_boro))
#district <- as.numeric(unique(borosubset$comm_district))
#assault <- as.numeric(unique(borosubset$ipv_fel_assault))
#rape <- as.numeric(unique(borosubset$ipv_rape))
  

#remove(borosubset)

pdf(NULL)

#Create variable for intimate partner violence data downloaded from NYC Open Data 
partner.load <- read.csv("IntimatePartnerViolence.csv") %>%
  mutate(boro = trimws(as.character(Comm_Dist_.Boro),"both"),
         assaultincidents = trimws(as.numeric(IPV_Fel_Assault),"both")
         )

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
                choices = sort(unique(partner.load$boro)),
                multiple = TRUE,
                selected = c("Queens", "Manhattan")),
    #Creates Community District slider input
    sliderInput("district",
                "Community Districts:",
                min = min(partner.load$Comm_District, na.rm = T),
                max = max(partner.load$Comm_District, na.rm = T),
                value = c(min(partner.load$Comm_District, na.rm = T),
                          max(partner.load$Comm_District, na.rm = T)),
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
                          min = min(partner.load$IPV_Fel_Assault, na.rm = T),
                          max = max(partner.load$IPV_Fel_Assault, na.rm = T),
                          value = c(min(partner.load$IPV_Fel_Assault, na.rm = T), 
                                    max(partner.load$IPV_Fel_Assault, na.rm = T)),
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
                          min = min(partner.load$IPV_Rape, na.rm = T),
                          max = max(partner.load$IPV_Rape, na.rm = T),
                          value = c(min(partner.load$IPV_Rape, na.rm = T), 
                                    max(partner.load$IPV_Rape, na.rm = T)),
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
    partner <- partner.load %>%
    # Allows for the slider input to be reactive
    filter(Comm_District >= input$district[1] & Comm_District <= input$district[2]) 
  
    # Allows for the select input to be reactive
    if (length(input$boro) > 0 ) {
      partner <- subset(partner, boro %in% input$boro)
    }
    return(partner)
  })
  
  #Reactive Function for Assaults Page (local input)
  partnerAssaults <- reactive({
      #Allows for the slider input of felony assaults to be reactive for the boxplot
      partner <- partnerInput() %>%
        filter(IPV_Fel_Assault >= input$assaultcount[1] & IPV_Fel_Assault <= input$assaultcount[2]) 
      return(partner)
    })
  
  #Reactive Function for Rape Page (local input)
  partnerRapes <- reactive({
    #Allows for the slider input of felony assaults to be reactive for the boxplot
    partner <- partnerInput() %>%
      filter(IPV_Rape >= input$rapecount[1] & IPV_Rape <= input$rapecount[2]) 
      return(partner)
  })
  
  #Bar plot for felony assault that involved a family member by borough 
  output$assaultbarplot <- renderPlotly({
    partner <- partnerInput() 
    ggplot(data = partner, aes(x = boro, y = IPV_Fel_Assault)) +
      geom_bar(stat="identity", fill = "#bcbddc") + theme_bw()
  })
  
  #Box plot for felony assault that involved a family member by borough 
  output$assaultboxplot <- renderPlotly({
    partnerAssaults <- partnerAssaults() 
    ggplot(data = partnerAssaults) +
      geom_boxplot(mapping = aes(x= boro, y = IPV_Fel_Assault)) + theme_bw()

  })
  
  #Bar plot for rape that involved a family member by borough 
  output$rapebarplot <- renderPlotly({
    partner <- partnerInput() # Change to other function
    ggplot(data = partner, aes(x = boro, y =IPV_Rape)) +
      geom_bar(stat="identity", fill = "#ffeda0") + theme_bw()
  })
  
  #Box plot for rape that involved a family member by borough 
  output$rapeboxplot <- renderPlotly({

    partnerRapes <- partnerRapes() # See above
    ggplot(data = partnerRapes) +
      geom_boxplot(mapping = aes(x= boro, y = IPV_Rape)) + theme_bw()
  })
  
  #Data table of whole dataset
  output$table <- DT::renderDataTable({
    subset(partnerInput(), select = c(boro, Comm_District, IPV_Fel_Assault, DV_Fel_Assault, IPV_Rape, DV_Rape))
  })
  
  #Value box of average incidents 
  output$incidentsBox <- renderValueBox({
    partner <- partnerInput()
    incidents <- round(mean(partner$IPV_DIR, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Incidents Reported", value = incidents , icon = icon("gavel"), color = "teal")
  })
  
  #Value box of average felony assaults
  output$assaultBox <- renderValueBox({
    partner <- partnerInput()
    assault <- round(mean(partner$IPV_Fel_Assault, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Felony Assault Incidents Reported", value = assault, icon = icon("gavel"), color = "purple")
  })
  
  #Value box of average incidents box, this is the same number as my first value box, the reason I have two is so that users can have a reference
  #of the average number of incidents on each page to compare to rape incidents to 
  output$rapeincidentsBox <- renderValueBox({
    partner <- partnerInput()
    incidents <- round(mean(partner$IPV_DIR, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Incidents Reported", value = incidents , icon = icon("gavel"), color = "teal")
  })
  
  #Value box of average rapes
  output$rapeBox <- renderValueBox({
    partner <- partnerInput()
    rape <- round(mean(partner$IPV_Rape, na.rm = T), 0)
    
    valueBox(subtitle = "Average Number of Rape Incidents Reported", value = rape, icon = icon("gavel"), color = "yellow")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "boro", selected = c("Queens", "Manhattan"))
    updateSliderInput(session, "district", value = c(min(partner.load$Comm_District, na.rm = T), max(partner.load$Comm_District, na.rm = T)))
    updateSliderInput(session, "assaultcount", value = c(min(partner.load$IPV_Fel_Asssault, na.rm = T), max(partner.load$IPV_Fel_Asssault, na.rm = T)))
    updateSliderInput(session, "rapecount", value = c(min(partner.load$IPV_Rape, na.rm = T), max(partner.load$IPV_Rape, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
