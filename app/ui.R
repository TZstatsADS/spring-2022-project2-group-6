
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}

shinyUI(dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(title ="Dashboard Header", titleWidth = 300),
  dashboardSidebar(
    width = 270,
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Covid Overview", tabName = "Covid", icon = icon("virus")),
      menuItem("Covid Effects", tabName = "Effects", icon = icon("chart-bar"),
               startExpanded = TRUE,
               menuSubItem("How Covid has affected crimes", tabName = "Crimes", icon = icon("skull-crossbones")),
               menuSubItem("How Covid has affected bike usage", tabName = "Bikes", icon = icon("biking")),
               menuSubItem("How Covid has affected restaurants", tabName = "Restaurants", icon = icon("utensils"))),
      menuItem("Restaurants Map", tabName = "Restaurants_map", icon = icon("map")),
      menuItem("About", tabName="About", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Home",
        fluidPage(
          h1("Project Title",align = "center", style="font-weight: bold"),
          br(),
          
          fluidRow(
            h4("Project descirption")
          )
        )
      ),
      
      tabItem(
        tabName = "Covid",
        fluidPage(

        )
      ),
      
      tabItem(
        tabName = "Effects",
        fluidPage(

        )
      ),
      
      tabItem(
        tabName = "Crimes",
        fluidPage(

        )
      ),
      
      tabItem(
        tabName = "Bikes",
        h4("Discussion about bikes and Covid"),
        
        box(width = 400,
            h4("Plot description"),
            br(),
            
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  
                  selectInput("bike_count_period", 
                              label = "Bike Count Period",
                              choices = c("Per Year" = 1,
                                          "Per Month" = 2), 
                              selected = 1),
                  selectInput("bike_count_borough", 
                              label = "Bike Count Borrow",
                              choices = c("Manhattan" = 1,
                                          "Brooklyn" = 2,
                                          "Queens" = 3,
                                          "Staten Island" = 4,
                                          "All" = 5), 
                              selected = 5)
                ),
                mainPanel(
                   plotlyOutput("bike_count")
                ))
              
            )
            
        )
        
        
        # fluidPage(
        #   wellPanel(
        #     selectInput("select", label = h3("Select box"), 
        #                 choices = list("Per Year" = 1, "Per Month" = 2), selected = 1),
        #     hr(),
        #     
        #   )
        # )
      ),
      
      tabItem(
        tabName = "Restaurants",
        fluidPage(

        )
      ),
      
      tabItem(
        tabName = "Restaurants_map",
        fluidPage(
          
        )
      ),

      tabItem(
        tabName = "About",
        fluidPage(
          h1("Team",align = "center", style="font-weight: bold"),
          br(),
          h1("Data",align = "center", style="font-weight: bold"),
          br(),
          h1("Code",align = "center", style="font-weight: bold")
        )
      )
      
    )
  )
  

))

# # Define UI for application that draws a histogram
# shinyUI(
#     navbarPage(strong("Citi Bike Study",style="color: white;"),
#                theme=shinytheme("cerulean"), # select your themes https://rstudio.github.io/shinythemes/
# #------------------------------- tab panel - Maps ---------------------------------
#                 tabPanel("Maps",
#                          icon = icon("map-marker-alt"), #choose the icon for
#                          div(class = 'outer',
#                         # side by side plots
#                         fluidRow(
#                                 splitLayout(cellWidths = c("50%", "50%"),
#                                              leafletOutput("left_map",width="100%",height=1200),
#                                              leafletOutput("right_map",width="100%",height=1200))),
#                         #control panel on the left
#                         absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
#                                       top = 200, left = 50, right = "auto", bottom = "auto", width = 250, height = "auto",
#                                       tags$h4('Citi Bike Activity Comparison'),
#                                       tags$br(),
#                                       tags$h5('Pre-covid(Left) Right(Right)'),
#                                       prettyRadioButtons(
#                                                       inputId = "adjust_score",
#                                                       label = "Score List:",
#                                                       choices = c("start_cnt",
#                                                                   "end_cnt",
#                                                                   "day_diff_absolute",
#                                                                   "day_diff_percentage"),
#                                                       inline = TRUE,
#                                                       status = "danger",
#                                                       fill = TRUE
#                                                         ),
#                                       awesomeRadio("adjust_time",
#                                                    label="Time",
#                                                     choices =c("Overall",
#                                                                "Weekday",
#                                                                "Weekend"),
#                                                     selected = "Overall",
#                                                     status = "warning"),
#                                       # selectInput('adjust_weather',
#                                       #             label = 'Adjust for Weather',
#                                       #             choices = c('Yes','No'),
#                                       #             selected = 'Yes'
#                                       #             ),
#                                       style = "opacity: 0.80"
# 
#                                 ), #Panel Control - Closing
#                             ) #Maps - Div closing
#                         ) #tabPanel maps closing
# 
# 
# 
#     ) #navbarPage closing
# ) #Shiny UI closing
