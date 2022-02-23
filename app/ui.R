
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
      menuItem("HOME", tabName = "Home", icon = icon("home")),
      menuItem("RESTAURANTS SAFETY MAP", tabName = "Restaurants_map", icon = icon("map")),
      menuItem("EVERYDAY COVID NEWS", tabName = "Covid", icon = icon("virus")),
      menuItem("INTERESTING INFO", tabName = "Effects", icon = icon("chart-bar"),
               startExpanded = TRUE,
               menuSubItem("COVID & CRIMES", tabName = "Crimes", icon = icon("skull-crossbones")),
               menuSubItem("COVID & BIKES", tabName = "Bikes", icon = icon("biking")),
               menuSubItem("COVID & RESTAURANTS", tabName = "Restaurants", icon = icon("utensils"))),
      menuItem("ABOUT", tabName="About", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Home",
        fluidPage(
          setBackgroundImage(src = "TimeSquare_adj.jpg", shinydashboard = T),
          absolutePanel(id = "text", class = "foreground-content",
                        top = "25%", left = "25%", width = "70%", fixed = F,
                        draggable = F, height = 200,
                        fluidRow(
                          style = "padding: 6%; background-color: white",
                          h1("It is time to get outside!", style = "color: #7b6f38; font-weight: bold; font-size: 50px;"),
                          h1("Find A Safe Restaurant Here!", style = "color: #5da1ab; font-weight: bold; font-size: 70px"),
                          br(),
                          p("As Covid started spreading in the Big Apple a lot has changed. Health safety regulations have 
                            been imposed by the government and health awareness has increased among New Yorkers who are 
                            trying to protect themselves and their loved ones by social distancing. As a measure against 
                            Covid, the Department of Transportation has opened streets exclusively for cyclists and pedestrians 
                            and has allowed restaurants to have outside seating space. However, these pasts months New York has 
                            also seen a great increase in crimes. Our goal is to help New Yorkers find restaurants that have outside 
                            space, are near streets open for cyclists and pedestrians, and are in safe neighbourhoods, in an 
                            effort help everyone stay healthy and safe!", style = "color: #666666; font-weight: italian; font-size: 15px"),
                        ),
                        style = "opacity: 0.75")
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
                       img(src="vaccine_slogan_adj.png", width = "80%", height = "25%"),
                       br(),br()),
                # Vac bar char
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
          h1("TEAM", align = 'center', style = "color: #7b6f38; font-weight: bold; font-size: 40px;"),
          br(),
          fluidRow( 
            column( width = 3,
                    img(src="ani_yinan.png",width = "70%", height = "35%", style="display: block; margin-left: auto; margin-right: auto;"),
                    h4(strong("Yinan Shi"), style = "color: #4b4b4b; font-size:22px;", align = 'center'),
                    h4("ys3387@columbia.edu", style = "font-size:20px; color:blue;", align = 'center'),
                    h4("LinkedIn | Twitter", style = "font-size:18px; color:blue;", align = 'center')
            ),
            column( width = 3,
                      img(src="ani_silvia.png",width = "70%", height = "35%", style="display: block; margin-left: auto; margin-right: auto;"),
                    h4(strong("Silvia Vlachou"), style = "color: #4b4b4b; font-size:22px;", align = 'center'),
                    h4("vv2340@columbia.edu", style = "font-size:20px; color:blue;", align = 'center'),
                    h4("LinkedIn | Twitter", style = "font-size:18px; color:blue;", align = 'center')
            ),
            column( width = 3,
                    img(src="ani_xinran.png",width = "70%", height = "35%", style="display: block; margin-left: auto; margin-right: auto;"),
                    h4(strong("Xinran Wang"), style = "color: #4b4b4b; font-size:22px;", align = 'center'),
                    h4("xw2809@columbia.edu", style = "font-size:20px; color:blue;", align = 'center'),
                    h4("LinkedIn | Twitter", style = "font-size:18px; color:blue;", align = 'center')
            ),
            column( width = 3,
                    img(src="ani_jiachen.png",width = "70%", height = "35%", style="display: block; margin-left: auto; margin-right: auto;"),
                    h4(strong("Jiachen Liu"), style = "color: #4b4b4b; font-size:22px;", align = 'center'),
                    h4("jl5991@columbia.edu", style = "font-size:20px; color:blue;", align = 'center'),
                    h4("LinkedIn | Twitter", style = "font-size:18px; color:blue;", align = 'center')
            )
          ),
          fluidRow(
            br(),
            br(),
            br()
          ),
          fluidRow(
            style = "padding 8%; background-color: white; opacity: 0.65",
            h1("RESOURCES", align = 'center', style = "color: #7b6f38; font-weight: bold; font-size: 40px;"),
            br(),
            h4("Covid", style = "font-size: 24px;", align = 'center'),
            h4(a(href="https://github.com/nychealth/coronavirus-data/","NYC Coronavirus Disease 2019 (COVID-19) Data",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4("Crimes", style = "font-size: 24px;", align = 'center'),
            h4(a(href="https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc","NYC Open Data - NYPD Arrest Data (Year to Date)",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4(a(href="https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8","NYC Open Data - NYPD Shooting Incident Data (Year To Date)",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4("Bikes and Open Streets", style = "font-size: 24px;", align = 'center'),
            h4(a(href="https://data.cityofnewyork.us/NYC-BigApps/Citi-Bike-System-Data/vsnr-94wk","Citi Bike System Data",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4(a(href="https://www1.nyc.gov/html/dot/html/bicyclists/bike-counts.shtml","NYC Bicycle Counts",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4(a(href="https://data.cityofnewyork.us/Health/Open-Streets-Locations/uiay-nctu","NYC Open Data - Open Streets Locations",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4("Restaurants", style = "font-size: 24px;", align = 'center'),
            h4(a(href="https://data.cityofnewyork.us/Transportation/Open-Restaurant-Applications/pitm-atqc","NYC Open Data - Open Restaurant Applications",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4(a(href="https://www1.nyc.gov/html/dot/html/pedestrians/openrestaurants.shtml","NYC Open Restaurants",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4(a(href="https://data.cityofnewyork.us/Business/Sidewalk-Caf-Licenses-and-Applications/qcdj-rwhu","NYC Open Data - Sidewalk Café Licenses and Applications",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4("Images", style = "font-size: 24px;", align = 'center'),
            h4(a(href="https://www.freepik.com/vectors/animal-set","freepik",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            h4(a(href="https://www.whcawical.org/getvaccinated/","WHCA-wiCAL",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center'),
            br(),
            br()
          ),
          fluidRow(
            br(),
          ),
          fluidRow(
            style = "padding 8%; background-color: white; opacity: 0.65",
            h1("CODE", align = 'center', style = "color: #7b6f38; font-weight: bold; font-size: 40px;"),
            br(),
            h4(a(href="https://github.com/TZstatsADS/spring-2022-project2-group-6","https://github.com/TZstatsADS/spring-2022-project2-group-6",target="_blank", style = "font-size: 20px; color:blue;"), align = 'center')
          )
        )
      )
    
  )
)))
