
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

#Data Loading
app_data <- read.csv('../output/open_restaurants.csv')
zipcode <- sort(unique(app_data$Postcode_x))

shinyUI(dashboardPage(
  
  skin = "black",
  
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
          setBackgroundImage(src = "TimeSquare_adj.jpg", shinydashboard = T),
          h1("Project Title",align = "center", style="font-weight: bold"),
          br(),
          
          fluidRow(
            h4("Project descirption")
          )
        )
      ),
      
      tabItem(
        tabName = "Covid",
        # Six Boxes
        fluidPage(
          fluidRow(
            valueBoxOutput("total_case"),
            valueBoxOutput("total_hospital"),
            valueBoxOutput("total_death")),
          fluidRow(
            valueBoxOutput("day7_case"),
            valueBoxOutput("day7_hospital"),
            valueBoxOutput("day7_death"))
        ),
        # 7 day trend
        box(width = 400,
            h4("Latest News on COVID", align = 'center', style="font-weight: bold"),
            br(),
            
            fluidPage(
              fluidRow(
                column(width = 6, highchartOutput("covid_trend_7day")),
                column(width = 6,p(strong("COVID Omicron News: U.S. looking at possibility of fourth vaccine dose for some",style = "font-size: 27px;"),a(href="https://abc7ny.com/covid-vaccines-fourth-shot-new-york-vaccine-mandate/11583089/","Click for Link",target="_blank", style = "font-size: 13px;"),
                                   br(),br(),
                                   strong("Brooklyn gang members lived large on more than $4 million in stolen COVID unemployment funds", style = "font-size: 27px;"),
                                   a(href="https://www.nydailynews.com/new-york/nyc-crime/ny-brooklyn-gang-members-stole-millions-in-covid-aid-20220218-poe5moloafht3azdzwfticm5xe-story.html","Click for Link",target="_blank",style = "font-size: 13px;"),
                                   br(),br(),
                                   strong("COVID Omicron News: Positivity rate continues to drop in Tri-State", style = "font-size: 27px;"),
                                   a(href="https://abc7ny.com/positivity-rate-cdc-mask-guidance-school-masks-kathy-hochul/11588695/","Click for Link",target="_blank", style = "font-size: 13px;"),
                                   br(),br(),
                                   strong("Nearly 1,500 NYC municipal workers fired for not being vaccinated against COVID", style = "font-size: 27px;"),
                                   a(href="https://abcnews.go.com/Health/1500-nyc-municipal-workers-fired-vaccinated-covid/story?id=82900617","Click for Link",target="_blank", style = "font-size: 13px;")
                )
                )
              ))),
        # Overall view
        box(width = 400,
            h4("Covid Overall View", align = 'center', style="font-weight: bold"),
            br(),

            fluidPage(
        #       # Covid Overall Trend line Row
              fluidRow(
                column( width = 12,
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       checkboxInput("cases", label = "Cases", value = TRUE),
                                       checkboxInput("hospital", label = "Hospitalization", value = TRUE),
                                       checkboxInput("death", label = "Death", value = TRUE)
                          ),
                          mainPanel(highchartOutput("covid_trend_total"))
                        )
                )
              ),
        #       # Vacc info Row
              fluidRow(
                # Get Vaccinated!!!
                column(width = 5,
                       img(src="vaccine_slogan.png",width = "80%", height = "25%"),
                       br(),br()),
                # Vac bar chart
                column(width = 5,highchartOutput("covid_vax_bar"))
              )

            ),
            # Pie charts
            fluidRow(
              # put up three pie chart
              column(width = 4, plotlyOutput("pie_race")),
              column(width = 4, plotlyOutput("pie_age")),
              column(width = 4, plotlyOutput("pie_poverty"))
            )
            
        ),
        
        # Heatmap
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
        tabName = "Effects",
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
        h1("How has Covid affected Crimes?", align = 'center'),
        h3("It is a fact among New Yorkers that the Big Apple saw an increase in almost every category of major crime in 2021 with this trend continuing in 2022.", align = 'center'),
        box(width = 400,
          
          br(),
          fluidPage(
            fluidRow( 
              h4(""),
              br(),
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
                          br(),
                          br(),
                          br(),
                          br(),
                          highchartOutput("arrests_crimes_month_year")
                        )
                      )
              ),
              column(width = 5,p(br(),br(),br(),
                                 strong("More than 1,500 NYC inmates have been released during coronavirus crisis",style = "font-size: 27px;"),a(href="https://nypost.com/2020/04/10/more-than-1500-nyc-inmates-have-been-released-amid-coronavirus-crisis/","Click for Link",target="_blank", style = "font-size: 13px;"),
                                 br(),br(),
                                 strong("Scores of NYC inmates serving time on Rikers set for early release amid surging crime", style = "font-size: 27px;"),
                                 a(href="https://www.dailyadvent.com/news/amp/c4ae64518b811be6052a5957b9e92cb6-Scores-of-NYC-inmates-serving-time-on-Rikers-set-for-early-release-amid-surging-crime","Click for Link",target="_blank",style = "font-size: 13px;"),
                                 br(),br(),
                                 strong("NY governor orders immediate release of 191 inmates from Rikers Island", style = "font-size: 27px;"),
                                 a(href="https://thehill.com/homenews/state-watch/572771-ny-governor-orders-immediate-release-of-191-inmates-from-rikers-island","Click for Link",target="_blank", style = "font-size: 13px;")
                                 
              )
              ),
              br()
            )
          )
        ),
        box(width = 400, 
          h4("Number of shootings per zipcode", align = 'center', style = "font-size:20px;"),
          br(),
          fluidRow( 
            column( width = 6,
                    h4("Shootings in 2019", align = 'center'),
                    leafletOutput("shootings_map_2019") 
            ),
            column( width = 6,
                    h4("Shootings in 2021", align = 'center'),
                    leafletOutput("shootings_map_2021") 
            )
          )
        )
      ),
      
      tabItem(
        tabName = "Bikes",
        h1("How has Covid affected bike usage?", align = 'center'),
        h3("As Covid started spreading and New Yorkers started to socially distance, they started looking at alternative ways to commute.", align = 'center'),
        box(width = 400,
            h4("Bike usage over time", align = 'center', style = "font-size:20px;"),
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
            h4("Open Streets for bikes and pedestrians", align = 'center', style = "font-size:20px;"),
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
                )
              ),
              br(),
              fluidRow( 
                column( width = 12,
                        h3("Map with open streets", align = 'center'),
                        leafletOutput("open_streets_map", width="100%", height=800),
                        br(),
                        h5("The blue lines represent the parts of the streets that are open. By clicking a blue line, the days and times the respective street 
                           is open for cyclists and pedestrains will be displayed.", align = 'center')
                      )
              )
            )
          )
      ),
      
      tabItem(
        tabName = "Restaurants",
        h1("How has Covid affected restaurants?", align = 'center'),
        h3("As Covid started spreading and regulations about social distancings were enforced, the only viable solution for restauransts to continue operating 
           and being profitable was to have outside seating.", align = 'center'),
        box(width = 400,
            h4("Number of restaurants with outside seating", align = 'center', style = "font-size:20px;"),
            br(),
            fluidRow( 
              column( width = 6,
                      highchartOutput("restaurants_borough")
              ),
              column( width = 6,
                      highchartOutput("restaurants_seatings") 
              )
            )
        ),
        
        box(width = 400,
            h4("When restaurants applied for outside seating space", align = 'center'),
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
            )
          )
      )),
      
      tabItem(
        tabName = "Restaurants_map",
        fluidPage(
          sidebarLayout( position = "right",
                         
                         sidebarPanel(
                           
                           h2( helpText("Restaurants Map") ),
                           
                           
                           selectInput( inputId="zip", 
                                        label="Select Zipcode", 
                                        choices= c("all", zipcode),
                                        selected="all",
                           ),
                           
                           selectInput( inputId="cluster1", 
                                        label="Cluster Option", 
                                        choices= c("ENABLE","DISABLE"),
                                        selected="yes"
                           ),
                           
                           sliderInput("range",
                                       "Rating:",
                                       min = 0,
                                       max = 10,
                                       value = c(5,8)
                           )
                           
                         ),
                         
                         mainPanel(leafletOutput("restaurant_map", width="150%", height=800)
                         )
                         #mainPanel(fluidRow( column(12, leafletOutput( "mapPlot1" ) ) ))
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
