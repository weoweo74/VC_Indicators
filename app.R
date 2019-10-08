## app.R ##
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(shinyalert)){
  install.packages("shinyalert")
  library(shinyalert)
}

if(!require(shinyBS)){
  install.packages("shinyBS")
  library(shinyBS)
}

if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
  library(shinycssloaders)
}

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs)
}

if(!require(mapview)){
  install.packages("mapview")
  library(mapview)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

recommendation <- read.csv('recommendation.csv',header=T)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Malaria Interventions")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    
    sidebarMenu(id = "sbm",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuSubItem("Country Overview", icon = icon("check-circle"),tabName = "globalattack1")),
                menuItem("Build your Query", tabName = "bycountry", icon = icon("gears"),badgeLabel = "new", badgeColor = "green"),
                conditionalPanel(
                  condition = "input.sbm == 'valueAnalysis' || input.sbm == 'trainModels' || input.sbm == 'compareModels' || input.sbm == 'forecast'",
                  uiOutput("stateUi"),
                  uiOutput("countyUi"),
                  uiOutput("cityUi"),
                  uiOutput("zipUi")
                ),
                #menuItem("Predicting Malaria Incidence", tabName = "predict", icon = icon("dashboard")),
                menuItem("Help", tabName = "help", icon = icon("question-circle"))
    
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Vector_Control per Country"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Vector_ControlbyPrd", height = "300px")
  )
  ,box(
    title = "Vector_Control per Admin1"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Vector_ControlbyAdmin1", height = "300px")
  ) 
)

########################
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')
##############################################################################################################
# Customizing for Malaria Replace 'Product' with 'Country' and 'Region' with 'Admin1'
# Also Replace 'Revenue' with 'Vector_Control'
#######################################SERVER SIDE IF THE SHINY APP###########################################
# create the server functions for the dashboard

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(shinyalert)){
  install.packages("shinyalert")
  library(shinyalert)
}

if(!require(shinyBS)){
  install.packages("shinyBS")
  library(shinyBS)
}

if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
  library(shinycssloaders)
}

if(!require(shinyjs)){
  install.packages("shinyjs")
  library(shinyjs)
}

if(!require(mapview)){
  install.packages("mapview")
  library(mapview)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

server <- function(input, output) { 
  
  total.Vector_Control <- sum(recommendation$Vector_Control)
  sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Vector_Control)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Country) %>% summarise(value = sum(Vector_Control)) %>% filter(value==max(value))
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Top Account:',sales.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total.Vector_Control, format="d", big.mark=',')
      ,'Total Nets (ITN) Distributed'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      # Here we have to replace  paste('Top product:' with 'Highest Malaria Cases:'
      ,paste('Highest Malaria Cases:',prof.prod$Country)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
  #creating the plotOutput content
  output$Vector_ControlbyPrd <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Country, y=Vector_Control, fill=factor(Admin1))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Vector_Control (in Euros)") + 
      xlab("Country") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Vector_Control by Country") + labs(fill = "Admin1")
  })
  output$Vector_ControlbyAdmin1 <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Vector_Control, fill=factor(Admin1))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Vector_Control (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Vector_Control by Admin1") + labs(fill = "Admin1")
  })
}
shinyApp(ui, server)