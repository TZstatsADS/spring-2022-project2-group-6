
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
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("timetk")) {
  install.packages("timetk")
  library(timetk)
}
if (!require("kableExtra")) {
  install.packages("kableExtra")
  library(kableExtra)
}
if (!require("highcharter")) {
  install.packages("highcharter")
  library(highcharter)
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
      menuItem("Restaurants Map", tabName = "Restaurants_map", icon = icon("map"),
               startExpanded = TRUE,
               menuSubItem("COVID 7-Day Map", tabName = "covid_heat", icon = icon("virus"))),
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
          fluidRow(
            valueBoxOutput("total_case"),
            valueBoxOutput("total_hospital"),
            valueBoxOutput("total_death")),
          fluidRow(
            valueBoxOutput("day7_case"),
            valueBoxOutput("day7_hospital"),
            valueBoxOutput("day7_death"))
        )
      ),
      
      tabItem(
        tabName = "Effects",
        fluidPage(

        )
      ),
      
      tabItem(
        tabName = "Crimes",
        h4("Discussion about crimes and Covid"),
        box(width = 400,
          h4("Number of arrests over time", align = 'center'),
          br(),
          
          fluidPage(
            fluidRow( 
              column( width = 7,
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          div(style = "font-size: 12px;",
                              radioButtons("crimes_arrests", "",
                                           c("Arrests" = "Arrests",
                                             "Shootings" = "Shootings"))
                          )
                        ),
                        mainPanel(
                          highchartOutput("arrests_crimes_month_year")
                        )
                      )
              ),
              column( width = 5,
                      img(src="Crimes_news.png",width = "100%", height = "35%")
              ),
              br()
            ),
            fluidRow( 
              h5("Discuss plots", align = 'center', style="font-weight: bold")
            )
          )
        ),
        box(width = 400, 
          h4("Title", align = 'center'),
          br(),
          fluidRow( 
            column( width = 6,
                    h4("Shootings Sep-Dec 2019", align = 'center'),
                    leafletOutput("shootings_map_2019") 
            ),
            column( width = 6,
                    h4("Shootings Sep-Dec 2021", align = 'center'),
                    leafletOutput("shootings_map_2021") 
            )
          )
        )
      ),
      
      tabItem(
        tabName = "Bikes",
        img(src="bikes_header.png",width = "100%", height = "35%"),
        h1("How has Covid affected bike usage?", align = 'center'),
        
        box(width = 400,
            h4("Number of bikes over time", align = 'center'),
            br(),
            
            fluidPage(
              fluidRow( 
                column( width = 6,
                        highchartOutput("bike_count_year")
                ),
                column( width = 6,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            
                            selectInput("bike_count_borough_month", 
                                        label = "Bike Count Borrow",
                                        choices = c("Manhattan" = 1,
                                                    "Brooklyn" = 2,
                                                    "All" = 3), 
                                        selected = 3)
                          ),
                          mainPanel(
                            highchartOutput("bike_count_month")
                          ))
                ),
                br(),
                br(),
                h5("Discuss plots", align = 'center', style="font-weight: bold")
              )
            )
        ),
        box(width = 400,
            h4("Open Streets", align = 'center'),
            br(),
            
            fluidPage(
              fluidRow( 
                column( width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 2,
                            checkboxInput("per_day", label = "Per Day", value = TRUE),
                            checkboxInput("aggregated", label = "Aggregated", value = FALSE)
                          ),
                          mainPanel(
                            highchartOutput("open_streets_dates")
                          )
                        )
                ),
              fluidRow( 
                column( width = 12,
                        h3("TITLE", align = 'center'),
                        leafletOutput("open_streets_map", width="100%", height=800)
                      )
              ),
                br(),
                br(),
                h5("Discuss plots", align = 'center', style="font-weight: bold")
            )
          )
      )),
      
      tabItem(
        tabName = "Restaurants",
        h4("Discussion about restaurants and Covid"),
        
        # box(width = 400,
        #     #DT::dataTableOutput("mytable")
        # ),
        box(width = 400,
            h4("Title", align = 'center'),
            br(),
            fluidRow( 
              column( width = 6,
                      h4("Open restaurants by borough", align = 'center'),
                      highchartOutput("restaurants_borough")
              ),
              column( width = 6,
                      h4("Open restaurants by additional seating type", align = 'center'),
                      highchartOutput("restaurants_seatings") 
              )
            )
        ),
        
        box(width = 400,
            h4("Open Restaurants Applications", align = 'center'),
            br(),
            
            fluidPage(
              fluidRow( 
                column( width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 2,
                            checkboxInput("per_day_restaurants", label = "Per Day", value = TRUE),
                            checkboxInput("aggregated_restaurants", label = "Aggregated", value = FALSE)
                          ),
                          mainPanel(
                            highchartOutput("open_restaurants_dates")
                          )
                        )
                )
            ),
            fluidRow(
              valueBoxOutput("vbox1", width = 2))
          )
      )),
      
      tabItem(
        tabName = "Restaurants_map",
        fluidPage(
          
        )
      ),
      
      tabItem(
        tabName = "covid_heat",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              selectInput("type", "Type", 
                          choices = c("7-Day", "Total"))),
            mainPanel(leafletOutput("heatmap"))
          )
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
)))
