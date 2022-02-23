#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("leaflet")) {
    install.packages("leaflet")
    library(leaflet)
}
if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
if (!require("mapview")) {
    install.packages("mapview")
    library(mapview)
}
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
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
if (!require("geojsonio")) { 
  install.packages("geojsonio")
  library(geojsonio)
}
if (!require("tigris")) { 
  install.packages("tigris")
  library(tigris)
}
if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)
}
if (!require("padr")) {
  install.packages("padr")
  library(padr)
}
if (!require("forecast")) {
  install.packages("forecast")
  library(forecast)
}
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")


# import data
bike_count <- read.csv('../output/Processed-bikecount-month.csv')
bike_count_boroughs <- read.csv('../output/Processed-bikecount-month-boroughs.csv')
open_streets_geo <- geojsonio::geojson_read("../data/Open Streets Locations.geojson", what ="sp")
open_streets <- read.csv('../output/Processed_Open_Streets_Locations.csv')
arrests <- read.csv('../output/NYPD_Arrests_Data_Recent.csv')
#shootings <- read.csv('../output/NYPD_Shooting_Data_Recent.csv')
covid7day <- read.csv('../output/covid_last7day.csv')
covid_total <- read.csv('../output/covid_total_case.csv')
covid_trend_week <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv",
                             stringsAsFactors = F)
covid_vax <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/breakthrough-summary.csv",
                      stringsAsFactors = F)
shootings_zipcodes <- read.csv('../output/Processed-shootings-zipcodes.csv')
open_restaurants <- read.csv('../output/open_restaurants.csv')

covid_data <- read.csv("../data/last7days-by-modzcta.csv")
crime_data <- read.csv('../output/NYPD_Shooting_Data_Recent.csv')
app_data <- read.csv("../output/Open_Restaurants.csv" )

by_age <- read.csv(url("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-age.csv"))
by_age <- as.data.frame(by_age)
by_age <- by_age[-c(4,12),]
by_poverty <- read.csv(url("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-poverty.csv"))
by_race <- read.csv(url("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-race.csv"))

#Data processing
vax_sum <- covid_vax$vax_count +covid_vax$unvax_count
covid_vax$vax_prob <- covid_vax$vax_count/(vax_sum)
covid_vax$unvax_prob <- covid_vax$unvax_count/(vax_sum)

##############################################################################
# Algorithm for Recommendation Rating System
##############################################################################
#Crime Evaluation (Shooting cases: exponential smoothing)
rating2_group <- unique(crime_data$BORO)
crime_data$Date <- as.Date(crime_data$OCCUR_DATE, format="%m/%d/%Y")
crime_frequency <- crime_data %>% group_by(BORO) %>% count(Date)
crime_paddate <- pad(crime_frequency, interval = "day",start_val = min(crime_data$Date),
                     end_val = max(crime_data$Date)) %>% fill_by_value(n)

rating2_pre <- rep(0,length(rating2_group))
i=1
for (groupname in unique(crime_paddate$BORO)){
  crime_smooth <- ses(crime_paddate %>% filter(BORO == groupname) %>% ungroup()%>% select('n'), h=1, alpha=0.8, initial="simple")
  rating2_pre[i] <- crime_smooth$mean[1]
  i <- i+1
}
rating2_min <- min(rating2_pre)

#Covid (Recent 7 days average percentage case)
rating3_group<- covid_data$label
rating3_pre <- covid_data$percentpositivity_7day
rating3_quantile <- quantile(rating3_pre, probs = c(.25, .75))
rating3_min <- min(rating3_pre)

#Rating Start
len <- dim(app_data)[1]
rating1 <- rep(0,len)
rating2 <- rep(0,len)
rating3 <- rep(0,len)
for (i in 1:len){
  #Five points for the open area
  data <- app_data[i,]
  if (data$SkippedReason=='No Seating'){
    rate <- 1
  }else if (data$SeatingChoice=='both') {
    rate <- 5
  }else if(data$SeatingChoice=='roadway'){
    rate <- 4
  }else if(data$SeatingChoice=='sidewalk'){
    rate <- 3
  }else if(data$SeatingChoice=='openstreets'){
    rate <- 2
  }
  
  if (data$IsRoadwayCompliant=='Non-Compliant'){
    rate <- rate-0.5
  }
  rating1[i] <- rate
  
  #Two points for safety issue
  boro <- toupper(data$Borough)
  rating2[i] <- 2*(rating2_min/rating2_pre[rating2_group==boro])
  
  #Three points for covid status 
  zipcode <- data$Postcode_x
  positive <- rating3_pre[grepl(zipcode,rating3_group)]
  try(
    if (positive<=rating3_quantile[1]){
      rating3[i] <- 3
    } else if (positive>rating3_quantile[2]) {
      rating3[i] <- 1
    } else {
      rating3[i] <- 2
    },
    silent = TRUE
  )
}

app_data$rating <- round(rating1+rating2+rating3, digits = 1)
##############################################################################
# End of the Rating
##############################################################################


shinyServer(function(input, output) {
  
  ##############################################################################
  # COVID tab - Graphs
  ##############################################################################
  
  output$covid_trend_total <- renderHighchart({
    c <- list(name = "Cases", data = covid_trend_week$CASE_COUNT,
              color='#6581BF', marker = list(symbol = 'circle'))
    h <- list(name = "Hospitalization", data = covid_trend_week$HOSPITALIZED_COUNT,
              color='#02216f', marker = list(symbol = 'circle'))
    d <- list(name = "Death", data = covid_trend_week$DEATH_COUNT,
              color='#2F57AB', marker = list(symbol = 'circle'))
    if (!input$cases & !input$hospital & input$death){
      hc_chart(hc_series(highchart(),d), type = "line") %>% hc_xAxis(categories = unique(covid_trend_week$date_of_interest))}
    else {
      if(input$cases){
        if(input$hospital){
          if(input$death){hc_chart(hc_series(highchart(),c, h, d), type = "line") %>% hc_xAxis(categories = unique(covid_trend_week$date_of_interest))}
          else {hc_chart(hc_series(highchart(),c, h), type = "line") %>% hc_xAxis(categories = unique(covid_trend_week$date_of_interest))}
        } else {hc_chart(hc_series(highchart(),c), type = "line") %>% hc_xAxis(categories = unique(covid_trend_week$date_of_interest))}
      } else if(input$hospital) {
        if(input$death){hc_chart(hc_series(highchart(),h, d), type = "line") %>% hc_xAxis(categories = unique(covid_trend_week$date_of_interest))}
        else {hc_chart(hc_series(highchart(), h), type = "line") %>% hc_xAxis(categories = unique(covid_trend_week$date_of_interest))}
      } else {} %>% 
        
        hc_exporting(enabled = T, formAttributes = list(target = "_blank")) %>% 
        hc_xAxis(categories = unique(covid_trend_week$date_of_interest)) %>% 
        hc_yAxis(title = list(text = "Count")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = F), enableMouseTracking = T))%>%
        hc_tooltip(table = TRUE, sort = TRUE,
                   # pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                   #                       " {series.name}: {point.y}"),
                   # headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
        ) %>%
        hc_title(text = "COVID Total Trend") %>%
        hc_legend(layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
        hc_caption(align = 'center', style = list(color = "black"), text = 'To be changed')}
  })
  
  output$covid_trend_7day <- renderHighchart({
    highchart() %>%
      hc_exporting(enabled = T, formAttributes = list(target = "_blank")) %>% 
      hc_chart(type = "line") %>% 
      hc_series(list(name = 'Case', data = covid_trend_week$CASE_COUNT %>% tail(7),
                     color = "#0099ae", marker = list(symbol = 'circle')),
                list(name = "Hospitalization", data = covid_trend_week$HOSPITALIZED_COUNT %>% tail(7),
                     color = '#02216f', marker = list(symbol = 'circle')),
                list(name = "Death", data = covid_trend_week$DEATH_COUNT %>% tail(7),
                     color = '#6581BF', marker = list(symbol = 'circle'))) %>% 
      hc_xAxis(categories = unique(covid_trend_week$date_of_interest %>% tail(7))) %>% 
      hc_yAxis(title = list(text = "Count")) %>% 
      hc_plotOptions(column = list(dataLabels = list(enabled = F), enableMouseTracking = T)) %>% 
      hc_tooltip(table = T, sort = T
                 # pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                 #                       " {series.name}: {point.y}"),
                 # headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_title(text = "COVID 7-Day Trend") %>% 
      hc_legend(layout = "vertical", align = "left", verticalAlign = "top", 
                floating = T, x = 50, y = 40) %>% 
      hc_caption(align = 'center', style = list(color = "black"), text = 'Past Week')
  })
  
  
  output$covid_vax_bar <- renderHighchart({
    covid_vax %>% select(measure, "Vaccinated" = vax_prob, "Unvaccinated" = unvax_prob) %>% 
      pivot_longer(cols = c(Vaccinated, Unvaccinated), names_to = "vax_type", values_to = "count") %>% 
      hchart("column", hcaes(x = measure, y = count, group = vax_type)) %>% 
      hc_chart(zoomType = "x") %>% 
      hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
      hc_yAxis(title = list(text = "Percentage"),
               max = covid_vax$unvax_prob %>% max()) %>%
      hc_title(text = "Percentage Number of Vaccinated and Unvaccinated People") %>%
      hc_caption( align = 'center', style = list(color = "black"), 
                  text = ' Unvaccinated people are getting more covid!')  %>%
      hc_exporting(enabled = TRUE) %>%
      hc_colors(c("#D7DCEA", "#A1B3D7", "#6581BF")) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE
                 # pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                 #                       " {series.name}: {point.y}"),
                 # headerFormat = '<span style="font-size: 13px">Trimester {point.key}</span>'
      )
  })
  
  output$pie_age <- renderPlotly({
    colors=RColorBrewer::brewer.pal(nrow(by_age),'Blues')
    
    fig <- plot_ly(by_age, labels = ~AGE_GROUP,
                   values = ~(CONFIRMED_CASE_COUNT/sum(CONFIRMED_CASE_COUNT)), type = 'pie',
                   marker = list(colors = colors))
    fig %>% layout(title = 'Age Group',
                   legend = list(x = -1, y = 1))
    
  })
  
  
  output$pie_poverty <- renderPlotly({
    
    
    colors=RColorBrewer::brewer.pal(nrow(by_poverty),'Oranges')
    
    fig <- plot_ly(by_poverty, labels = c('Low_poverty', 'Medium_poverty', 'High_poverty', 'Very_high_poverty'),  
                   values = ~(CONFIRMED_CASE_COUNT/sum(CONFIRMED_CASE_COUNT)), type = 'pie',
                   marker = list(colors = colors))
    fig %>% layout(title = 'Poverty Group',
                   legend = list(x = -1, y = 1))
    
  })
  
  
  output$pie_race <- renderPlotly({
    colors=RColorBrewer::brewer.pal(nrow(by_race),'Greens')    
    
    fig <- plot_ly(by_race, labels = c('Asian/Pacific-Islander', 
                                       'Black/African-American	', 
                                       'Hispanic/Latino', 'White'),
                   values = ~(CONFIRMED_CASE_COUNT/sum(CONFIRMED_CASE_COUNT)), type = 'pie',
                   marker = list(colors = colors))
    fig %>% layout(title = 'Race Group',
                   legend = list(x = -1, y = 1))
    
  })
  
  ##############################################################################
  # COVID Valuebox tab
  ##############################################################################
  
  output$total_case <- renderValueBox({
    
    valueBox(
      h4("Total Comfirmed Case", style="font-size: 24px"),
      h3(sum(covid_trend_week$CASE_COUNT)),
      icon = icon("virus", lib = "font-awesome"),
      color = "red"
    )
  })
  
  output$total_hospital <- renderValueBox({
    valueBox(
      h4("Total Hospitalization", style="font-size: 24px"),
      h3(sum(covid_trend_week$HOSPITALIZED)),
      icon = icon("hospital", lib = "font-awesome"),
      color = "teal"
    )
  })
  
  output$total_death <- renderValueBox({
    valueBox(
      h4("Total Death", style="font-size: 24px"),
      h3(sum(covid_trend_week$DEATH_COUNT)),
      icon = icon("skull", lib = "font-awesome"),
      color = "black"
    )
  })

  output$day7_case <- renderValueBox({
    valueBox(
      h4("7-Day Average Comfirmed Case", style="font-size: 24px"),
      h3(covid_trend_week$CASE_COUNT %>% tail(7) %>% mean() %>% round()),
      icon = icon("virus", lib = "font-awesome"),
      color = "red"
    )
  })

  output$day7_hospital <- renderValueBox({
    valueBox(
      h4("7-Day Average Hospitalization", style="font-size: 24px"),
      h3(covid_trend_week$HOSPITALIZED %>% tail(7) %>% mean() %>% round()),
      icon = icon("hospital", lib = "font-awesome"),
      color = "teal"
    )
  })

  output$day7_death <- renderValueBox({
    valueBox(
      h4("7-Day Average Death", style="font-size: 24px"),
      h3(covid_trend_week$DEATH_COUNT %>% tail(7) %>% mean() %>% round()),
      icon = icon("skull", lib = "font-awesome"),
      color = "black"
    )
  })
  
  ##############################################################################
  # Crimes tab
  ##############################################################################
  
  arrests = arrests %>% dplyr::rowwise() %>% dplyr::mutate(Month = strsplit(ARREST_DATE, split="/")[[1]][1])
  arrests = arrests %>% dplyr::rowwise() %>% dplyr::mutate(Year = strsplit(ARREST_DATE, split="/")[[1]][3])
  arrests = arrests %>% dplyr::rowwise() %>% dplyr::mutate(Year_Month = paste(Year, "/", Month, sep = ""))
  
  shootings_zipcodes = shootings_zipcodes %>% dplyr::rowwise() %>% dplyr::mutate(Month = strsplit(OCCUR_DATE, split="/")[[1]][1])
  shootings_zipcodes = shootings_zipcodes %>% dplyr::rowwise() %>% dplyr::mutate(Year = strsplit(OCCUR_DATE, split="/")[[1]][3])
  shootings_zipcodes = shootings_zipcodes %>% dplyr::rowwise() %>% dplyr::mutate(Year_Month = paste(Year, "/", Month, sep = ""))
  
  arrests_by_month_year <- data.frame(arrests$Year_Month) 
  names(arrests_by_month_year)[names(arrests_by_month_year) == 'arrests.Year_Month'] <- 'Year_Month'
  arrests_by_month_year$count <- 1
  arrests_by_month_year <- arrests_by_month_year %>% 
    group_by(Year_Month) %>% 
    summarise(Total_count = sum(count))
  arrests_by_month_year <- arrests_by_month_year[order(arrests_by_month_year$Year_Month),]
  arrests_by_month_year = arrests_by_month_year %>% dplyr::rowwise() %>% dplyr::mutate(Month_Year = paste(strsplit(Year_Month, split="/")[[1]][2], "/", strsplit(Year_Month, split="/")[[1]][1], sep = ""))
  
  shootings_by_month_year <- data.frame(shootings_zipcodes$Year_Month) 
  names(shootings_by_month_year)[names(shootings_by_month_year) == 'shootings_zipcodes.Year_Month'] <- 'Year_Month'
  shootings_by_month_year$count <- 1
  shootings_by_month_year <- shootings_by_month_year %>% 
    group_by(Year_Month) %>% 
    summarise(Total_count = sum(count))
  shootings_by_month_year <- shootings_by_month_year[order(shootings_by_month_year$Year_Month),]
  shootings_by_month_year = shootings_by_month_year %>% dplyr::rowwise() %>% dplyr::mutate(Month_Year = paste(strsplit(Year_Month, split="/")[[1]][2], "/", strsplit(Year_Month, split="/")[[1]][1], sep = ""))
  
  shootings_by_zipcode_total <- data.frame(shootings_zipcodes$zipcode, shootings_zipcodes$Year, shootings_zipcodes$Month) 
  names(shootings_by_zipcode_total)[names(shootings_by_zipcode_total) == 'shootings_zipcodes.zipcode'] <- 'zipcode'
  names(shootings_by_zipcode_total)[names(shootings_by_zipcode_total) == 'shootings_zipcodes.Year'] <- 'Year'
  names(shootings_by_zipcode_total)[names(shootings_by_zipcode_total) == 'shootings_zipcodes.Month'] <- 'Month'
  shootings_by_zipcode_total$count <- 1
  shootings_by_zipcode_total <- shootings_by_zipcode_total %>% 
    group_by(zipcode, Year, Month) %>% 
    summarise(Total_count = sum(count))
  shootings_by_zipcode_total <- subset(shootings_by_zipcode_total, zipcode!="7020")
  
  output$arrests_crimes_month_year <- renderHighchart({
    if (input$crimes_arrests == 'Arrests'){
      use_name <- 'Arrests'
      use_data <- arrests_by_month_year$Total_count
      use_color <- '#a07272'
    }
    if (input$crimes_arrests == 'Shootings'){
      use_name <- 'Shootings'
      use_data <- shootings_by_month_year$Total_count
      use_color <- '#640404'
    }
  
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_chart(type = 'line') %>%
      hc_series( list(name = use_name, data =use_data, color=use_color, marker = list(symbol = 'circle') )
      )%>%
      hc_xAxis( categories = unique(arrests_by_month_year$Month_Year),
                plotLines = list(
                  list(label = list(text = "Covid start"),
                       color = "#d25b5b",
                       width = 2,
                       value = 19.5)
                )
      ) %>%
      hc_yAxis( title = list(text = "Number of Arrests")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Month/Year {point.key}</span>'
      ) %>%
      hc_title(text = paste('How has the Number of ', use_name, ' Changed Over Time')) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
      hc_caption( align = 'center', style = list(color = "black"), text = "We observe that there are significantly less arrests after the onset of Covid than in the pre-Covid era. 
                  However, if we look at the shootings plot, it is apparent that the number of shooting has largely increased. This trend could potentially be explained as follows: 
                  as Covid started spreading within prisons, a large number of inmates were released due to fear that a wide spread of Covid among inmates would jeopardize their health.")
    
    
  })
  
  
  
  
  
  
  
  options(tigris_use_cache = TRUE)

  zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York") %>%
    mutate("ZCTA5CE10" = as.numeric(ZCTA5CE10))
  
  shootings_2021_3 <- subset(shootings_by_zipcode_total, Year=="2021")
  shootings_2021_3 <- shootings_2021_3 %>%
                    group_by(zipcode) %>%
                    summarise(Total_c = sum(Total_count))
  # output$mytable = DT::renderDataTable({
  #   shootings_2021_3
  # })
  
  shootings_2019_3 <- subset(shootings_by_zipcode_total, Year=="2019")
  shootings_2019_3 <- shootings_2019_3 %>%
    group_by(zipcode) %>%
    summarise(Total_c = sum(Total_count))
  
  output$shootings_map_2021 <- renderLeaflet({
    zcta <- geo_join(zcta, shootings_2021_3 %>% select(zipcode, "case" = Total_c), by_sp = "ZCTA5CE10",
                     by_df = "zipcode", how = "left") %>% drop_na()
  
    pal <- colorNumeric(
      palette = "Reds", domain = zcta$case
    )

    labels <- paste0("Zip code: ", zcta$ZCTA5CE10,"<br/>",
                     "Number of shootings: ", zcta$case) %>%
      lapply(htmltools::HTML)

    zcta %>% ungroup() %>% leaflet %>%
      addProviderTiles("CartoDB") %>%
      setView(lng=-73.985428, lat=40.748817, zoom = 10) %>%
      addPolygons(fillColor = ~pal(case), weight = 1, opacity = 1,
                  color = "white", dashArray = "2", fillOpacity = 0.7,
                  label = labels) %>%
      addLegend(pal = pal,
                values = ~case,
                title = "Shootings",
                position = "topright")
  })
  
  output$shootings_map_2019 <- renderLeaflet({
    zcta <- geo_join(zcta, shootings_2019_3 %>% select(zipcode, "case" = Total_c), by_sp = "ZCTA5CE10",
                     by_df = "zipcode", how = "left") %>% drop_na()
    
    pal <- colorNumeric(
      palette = "Reds", domain = zcta$case
    )
    
    labels <- paste0("Zip code: ", zcta$ZCTA5CE10,"<br/>",
                     "Number of shootings: ", zcta$case) %>%
      lapply(htmltools::HTML)
    
    zcta %>% ungroup() %>% leaflet %>%
      addProviderTiles("CartoDB") %>%
      setView(lng=-73.985428, lat=40.748817, zoom = 10) %>%
      addPolygons(fillColor = ~pal(case), weight = 1, opacity = 1,
                  color = "white", dashArray = "2", fillOpacity = 0.7,
                  label = labels) %>%
      addLegend(pal = pal,
                values = ~case,
                title = "Shootings",
                position = "topright")
  })
  
  
  
  ##############################################################################
  # Bikes tab
  ##############################################################################
  
  ##############################################################################
  # Bikes
  ##############################################################################
  
  bike_count_boroughs <- subset(bike_count_boroughs, borough!="Queens" & borough!="Staten Island")
  
  bike_count_per_year <- bike_count_boroughs %>% 
                         group_by(Year) %>% 
                         summarise(Total_count = sum(total))
  
  bike_count_per_year_per_borough <- bike_count_boroughs %>% 
                                      group_by(borough, Year) %>% 
                                      summarise(Total_count = sum(total))
  
  bike_count_per_year_per_borough <- subset(bike_count_per_year_per_borough, borough!="Queens" & borough!="Staten Island")
  
  bike_count_per_month <- bike_count_boroughs %>% 
    group_by(Month, Year, borough) %>% 
    summarise(Total_count = sum(total))
  
  bike_count_per_month_from_2017 <- subset(bike_count_per_month, Year>=2017)
  
  bike_count_per_quarter_from_2017 <- bike_count_per_month_from_2017
  bike_count_per_quarter_from_2017$Month <- replace(bike_count_per_quarter_from_2017$Month, 
                                                    bike_count_per_quarter_from_2017$Month==2 | 
                                                      bike_count_per_quarter_from_2017$Month==3 |
                                                      bike_count_per_quarter_from_2017$Month==4, 1)
  bike_count_per_quarter_from_2017$Month <- replace(bike_count_per_quarter_from_2017$Month, 
                                                    bike_count_per_quarter_from_2017$Month==5 | 
                                                      bike_count_per_quarter_from_2017$Month==6 |
                                                      bike_count_per_quarter_from_2017$Month==7 |
                                                      bike_count_per_quarter_from_2017$Month==8, 2)
  bike_count_per_quarter_from_2017$Month <- replace(bike_count_per_quarter_from_2017$Month, 
                                                    bike_count_per_quarter_from_2017$Month==9 | 
                                                      bike_count_per_quarter_from_2017$Month==10 |
                                                      bike_count_per_quarter_from_2017$Month==11 |
                                                      bike_count_per_quarter_from_2017$Month==12, 3)
 
  bike_count_per_quarter_from_2017_per_borough <- bike_count_per_quarter_from_2017 %>% 
    group_by(Month, Year, borough) %>% 
    summarise(Total_count = sum(Total_count))
  
  bike_count_per_quarter_from_2017 <- bike_count_per_quarter_from_2017 %>% 
    group_by(Month, Year) %>% 
    summarise(Total_count = sum(Total_count))
  
  output$bike_count_year <- renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_chart(type = 'line') %>%
      hc_series( list(name = 'Total', data =bike_count_per_year$Total_count, color='#0099ae', marker = list(symbol = 'circle') ),
                 list(name = 'Manhattan', data =bike_count_per_year_per_borough$Total_count[bike_count_per_year_per_borough$borough=='Manhattan'], color = '#02216f', marker = list(symbol = 'circle') ),
                 list(name = 'Brooklyn', data =bike_count_per_year_per_borough$Total_count[bike_count_per_year_per_borough$borough=='Brooklyn'], color = '#6581BF', marker = list(symbol = 'circle') )
      )%>%
      hc_xAxis(categories = unique(bike_count_per_year_per_borough$Year) ) %>%
      hc_yAxis(title = list(text = "Number of bikes")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_title(text = "Bike Usage per Year") %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
      hc_caption( align = 'center', style = list(color = "black"), text = 'We observe that the number of bikes increased largely after Covid.
                  Bikes where counted on some specific main points of Manhattan and Brooklyn. For 2021, we have data till the end of August.')
  })
  
  output$bike_count_month <- renderHighchart({
    if (input$bike_count_borough_month == 3) {
      use_data <- bike_count_per_quarter_from_2017
    }
    else if (input$bike_count_borough_month == 1){
      use_data <- subset(bike_count_per_quarter_from_2017_per_borough, borough=="Manhattan")
    }
    else if (input$bike_count_borough_month == 2){
      use_data <- subset(bike_count_per_quarter_from_2017_per_borough, borough=="Brooklyn")
    }
    hchart(use_data, "column",
           hcaes(x = Month, y = Total_count, group = Year)) %>%
      hc_chart(zoomType = "x") %>%
      hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
      hc_xAxis(title = list(text = "Trimester")) %>%
      hc_yAxis(title = list(text = "Number of bikes"),
               max = max(bike_count_per_quarter_from_2017$Total_count)) %>%
      hc_title(text = "Bike Usage per Trimester per Year") %>%
      hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')  %>%
      hc_exporting(enabled = TRUE) %>%
      hc_colors(c("#D7DCEA", "#A1B3D7", "#6581BF", "#2F57AB", "#0B389D")) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Trimester {point.key}</span>'
      ) %>%
      hc_caption( align = 'center', style = list(color = "black"), text = 'We observe that for every trimester more bikes were used in 2020 and 2021 than before Covid. 
                  Bikes where counted on some specific main points of Manhattan and Brooklyn. For 2021, we have data till the end of August, so we do not have data for the 3rd trimester of 2021.')
    
  })
  
  ##############################################################################
  # Bikes Open Streets
  ##############################################################################
  
  output$open_streets_map <- renderLeaflet({
    leaflet() %>%
      clearShapes() %>%
      clearMarkers() %>%
      addTiles() %>%
      setView(lng=-73.985428, lat=40.748817, zoom = 12) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolylines(data=open_streets_geo, color="#0B389D", weight=3, opacity=1, label = lapply(open_streets$Label, htmltools::HTML), popup = open_streets$Hours, 
                   highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 5, sendToBack = FALSE, color = "white"))
  })
  
  open_streets_dates <- data.frame(open_streets$StartDate) 
  names(open_streets_dates)[names(open_streets_dates) == 'open_streets.StartDate'] <- 'StartDate'
  open_streets_dates$count <- 1
  open_streets_dates <- open_streets_dates %>% 
    group_by(StartDate) %>% 
    summarise(Total_count = sum(count))
  open_streets_dates <- open_streets_dates[order(open_streets_dates$StartDate),]
  open_streets_dates_aggregate <- open_streets_dates
  
  open_streets_dates_aggregate$Aggregate <- 0
  counter <- 0
  for (i in 1:length(open_streets_dates_aggregate$StartDate)) {
    if(i==1) {
      counter <- counter + open_streets_dates_aggregate[i,2]
      open_streets_dates_aggregate[i,3] <- counter
    } else {
      counter <- counter + open_streets_dates_aggregate[i,2]
      open_streets_dates_aggregate[i,3] <- counter
    }
  }
  
  output$open_streets_dates <- renderHighchart({
    if (input$per_day & input$aggregated){
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'line') %>%
        hc_series( list(name = 'Per Day', data = open_streets_dates$Total_count, color='#6581BF', marker = list(symbol = 'circle') ),
                   list(name = 'Aggregate', data =open_streets_dates_aggregate$Aggregate, color = '#02216f', marker = list(symbol = 'circle') )
        )%>%
        hc_xAxis( categories = unique(open_streets_dates$StartDate) ) %>%
        hc_yAxis( title = list(text = "Number of approvals")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          #stacking = "normal",
          enableMouseTracking = T ) 
        )%>%
        hc_tooltip(table = TRUE,
                   sort = TRUE,
                   pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                         " {series.name}: {point.y}"),
                   headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
        ) %>%
        hc_title(text = "Open Streets Approvals") %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
        hc_caption( align = 'center', style = list(color = "black"), text = 'As more and more New Yorkers started using bikes for their commute, the Department of 
                    Transportation proceeded to opening some streets (for specific days and times each week) only for cyclists and pedestrians.')
    }
    else if (!input$per_day & !input$aggregated) {
    }
    else{
      if (input$per_day){
        use_name <- 'Per Day'
        name <- 'per day'
        use_data <- open_streets_dates$Total_count
        use_color <- '#6581BF'
      }
      if (input$aggregated){
        use_name <- 'Aggregated'
        name <- 'in total'
        use_data <- open_streets_dates_aggregate$Aggregate
        use_color <- '#02216f'
      }
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'line') %>%
        hc_series( list(name = use_name, data = use_data, color=use_color, marker = list(symbol = 'circle') )
        )%>%
        hc_xAxis( categories = unique(open_streets_dates$StartDate) ) %>%
        hc_yAxis( title = list(text = "Number of approvals")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          #stacking = "normal",
          enableMouseTracking = T ) 
        )%>%
        hc_tooltip(table = TRUE,
                   sort = TRUE,
                   pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                         " {series.name}: {point.y}"),
                   headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
        ) %>%
        hc_title(text = paste("Open Streets Approvals ", name)) %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
        hc_caption( align = 'center', style = list(color = "black"), text = 'Caption') %>%
        hc_caption( align = 'center', style = list(color = "black"), text = 'As more and more New Yorkers started using bikes for their commute, the Department of 
                    Transportation proceeded to opening some streets (for specific days and times each week) only for cyclists and pedestrians.')
    }
  })
  
  ##############################################################################
  # Restaurants tab
  ##############################################################################
  
  open_restaurants_borough <- open_restaurants
  open_restaurants_borough$count <- 1
  open_restaurants_borough <- open_restaurants_borough %>%
    group_by(Borough) %>%
    summarise(Total_count = sum(count))
  
  open_restaurants_seating_type <- open_restaurants
  open_restaurants_seating_type$count <- 1
  open_restaurants_seating_type <- open_restaurants_seating_type %>%
    group_by(Borough, Seating.Interest..Sidewalk.Roadway.Both.) %>%
    summarise(Total_count = sum(count))
  
  open_restaurants_seating_type <- rename(open_restaurants_seating_type, Seating=Seating.Interest..Sidewalk.Roadway.Both.)
  
  # output$mytable = DT::renderDataTable({
  #   open_restaurants_seating_type
  # })
  
  output$restaurants_borough <- renderHighchart({
    hchart(open_restaurants_borough, "column",
           hcaes(x = Borough, y = Total_count, color = c("#c1cbb4", "#789f52", "#2c5d37", "#d6db90", "#9cc43c"))) %>%
      hc_chart(zoomType = "x") %>%
      hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
      hc_xAxis(title = list(text = "Borough")) %>%
      hc_yAxis(title = list(text = "Number of restaurants"),
               max = max(open_restaurants_borough$Total_count)) %>%
      hc_title(text = "Number of Restaurants that Have Outside Space per Borough") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       "Open restaurants: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Borough: {point.key}</span>'
      )
  })
  
  output$restaurants_seatings <- renderHighchart({
    hchart(open_restaurants_seating_type, "column",
           hcaes(x = Borough, y = Total_count, group = Seating),
           stacking = "normal") %>%
      hc_chart(zoomType = "x") %>%
      hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
      hc_xAxis(title = list(text = "Borough")) %>%
      hc_yAxis(title = list(text = "Number of open resturants"),
               max = max(open_restaurants_seating_type$Total_count)) %>%
      hc_title(text = "Number of Restaurants per Type of Outside Space per Borough") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_colors(c("#c1cbb4", "#d6db90", "#789f52", "#2c5d37")) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">{point.key}</span>'
      )
  })
  
  open_restaurants_dates <- data.frame(open_restaurants$Time_of_Submission) 
  names(open_restaurants_dates)[names(open_restaurants_dates) == 'open_restaurants.Time_of_Submission'] <- 'Time_of_Submission'
  
  open_restaurants_dates = open_restaurants_dates %>% dplyr::rowwise() %>% dplyr::mutate(date = strsplit(Time_of_Submission, split=" ")[[1]][1])
  open_restaurants_dates = open_restaurants_dates %>% dplyr::rowwise() %>% dplyr::mutate(Year = strsplit(date, split="/")[[1]][3])
  open_restaurants_dates = open_restaurants_dates %>% dplyr::rowwise() %>% dplyr::mutate(Month = strsplit(date, split="/")[[1]][2])
  open_restaurants_dates = open_restaurants_dates %>% dplyr::rowwise() %>% dplyr::mutate(Day = strsplit(date, split="/")[[1]][1])
  open_restaurants_dates = open_restaurants_dates %>% dplyr::rowwise() %>% dplyr::mutate(Final_Date = paste(Year, "/", Month, "/", Day, sep = ""))
  
  open_restaurants_dates <- data.frame(open_restaurants_dates$Final_Date)
  names(open_restaurants_dates)[names(open_restaurants_dates) == 'open_restaurants_dates.Final_Date'] <- 'Final_Date'
  
  open_restaurants_dates$count <- 1
  open_restaurants_dates <- open_restaurants_dates %>%
    group_by(Final_Date) %>%
    summarise(Total_count = sum(count))
  open_restaurants_dates <- open_restaurants_dates[order(open_restaurants_dates$Final_Date),]
  open_restaurants_dates_aggregate <- open_restaurants_dates
  
  open_restaurants_dates_aggregate$Aggregate <- 0
  counter <- 0
  for (i in 1:length(open_restaurants_dates_aggregate$Final_Date)) {
    if(i==1) {
      counter <- counter + open_restaurants_dates_aggregate[i,2]
      open_restaurants_dates_aggregate[i,3] <- counter
    } else {
      counter <- counter + open_restaurants_dates_aggregate[i,2]
      open_restaurants_dates_aggregate[i,3] <- counter
    }
  }
  
  # output$mytable = DT::renderDataTable({
  #   open_restaurants_dates_aggregate
  # })
  
  
  output$open_restaurants_dates <- renderHighchart({
    if (input$per_day_restaurants & input$aggregated_restaurants){
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'line') %>%
        hc_series( list(name = 'Per Day', data = open_restaurants_dates$Total_count, color='#789f52', marker = list(symbol = 'circle') ),
                   list(name = 'Aggregate', data =open_restaurants_dates_aggregate$Aggregate, color = '#2c5d37', marker = list(symbol = 'circle') )
        )%>%
        hc_xAxis( categories = unique(open_restaurants_dates$Final_Date) ) %>%
        hc_yAxis( title = list(text = "Number of Applications")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          #stacking = "normal",
          enableMouseTracking = T )
        )%>%
        hc_tooltip(table = TRUE,
                   sort = TRUE,
                   pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                         " {series.name}: {point.y}"),
                   headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
        ) %>%
        hc_title(text = "Applications for Outside Seating Space") %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
        hc_caption( align = 'center', style = list(color = "black"), text = 'As Covid started spreading and regulations about social distancing were enforced, 
                    the only viable solution for restauransts to continue operating and being profitable was to have outside seating. To facilitate them, the Department 
                    of Transportation decided to approve restaurants for sidewalk and/or roadway seating.')
    }
    else if (!input$per_day_restaurants & !input$aggregated_restaurants) {
    }
    else{
      if (input$per_day_restaurants){
        use_name <- 'Per Day'
        use_data <- open_restaurants_dates$Total_count
        use_color <- '#789f52'
      }
      if (input$aggregated_restaurants){
        use_name <- 'Aggregated'
        use_data <- open_restaurants_dates_aggregate$Aggregate
        use_color <- '#2c5d37'
      }
      highchart() %>%
        hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
        hc_chart(type = 'line') %>%
        hc_series( list(name = use_name, data = use_data, color=use_color, marker = list(symbol = 'circle') )
        )%>%
        hc_xAxis( categories = unique(open_restaurants_dates$Final_Date) ) %>%
        hc_yAxis( title = list(text = "Number of Applications")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          #stacking = "normal",
          enableMouseTracking = T )
        )%>%
        hc_tooltip(table = TRUE,
                   sort = TRUE,
                   pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                         " {series.name}: {point.y}"),
                   headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
        ) %>%
        hc_title(text = "Applications for outside seating space") %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
        hc_caption( align = 'center', style = list(color = "black"), text = 'As Covid started spreading and regulations about social distancings were enforced, 
                    the only viable solution for restauransts to continue operating and being profitable was to have outside seating. To facilitate them the Department 
                    of Transportation decided to approve restaurants for sidewalk and/or roadway seating.')
    }
  })
  
  
  
  
  
  
  ##############################################################################
  # COVID HEAT tab
  ##############################################################################
  
  options(tigris_use_cache = TRUE)
  
  zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York") %>%
    mutate("ZCTA5CE10" = as.numeric(ZCTA5CE10))
  
  type <- reactive({
    if(input$type == "7-Day"){
      return(covid7day %>% select(modzcta, "case" = people_positive))
    }
    
    if(input$type == 'Total'){
      return(covid_total %>% select(modzcta, "case" = COVID_CASE_COUNT))
    }
  })
  
  output$heatmap <- renderLeaflet({
    zcta <- geo_join(zcta, type(), by_sp = "ZCTA5CE10",
                     by_df = "modzcta", how = "left") %>% drop_na()
    
    if (input$type == "7-Day"){
      pal <- colorNumeric(
        palette = "Reds", domain = zcta$case
      )
    }
    
    if (input$type == "Total"){
      pal <- colorNumeric(
        palette = "Purples", domain = zcta$case
      )
    }
    
    labels <- paste0("zip code: ", zcta$ZCTA5CE10,"<br/>",
                     "case number: ", zcta$case) %>%
      lapply(htmltools::HTML)
    
    zcta %>% ungroup() %>% leaflet %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(fillColor = ~pal(case), weight = 1, opacity = 1,
                  color = "white", dashArray = "2", fillOpacity = 0.7,
                  label = labels) %>%
      addLegend(pal = pal,
                values = ~case,
                title = "COVID MAP",
                position = "topright")
    
  })
  
  output$vbox1 <- renderValueBox({
    counter <- 10
    valueBox( "Coss", counter, color = "navy")
  })
    
    
  ##############################################################################
  # COVID HEAT tab
  ##############################################################################
  
  options(tigris_use_cache = TRUE)
  
  zcta <- zctas(starts_with = c(10, 11), year = 2010, state = "New York") %>%
    mutate("ZCTA5CE10" = as.numeric(ZCTA5CE10))
  
  type <- reactive({
    if(input$type == "7-Day"){
      return(covid7day %>% select(modzcta, "case" = people_positive))
    }
    
    if(input$type == 'Total'){
      return(covid_total %>% select("modzcta" = MODIFIED_ZCTA, "case" = COVID_CASE_COUNT))
    }
  })
  
  output$heatmap <- renderLeaflet({
    zcta <- geo_join(zcta, type(), by_sp = "ZCTA5CE10",
                     by_df = "modzcta", how = "left") %>% drop_na()
    
    if (input$type == "7-Day"){
      pal <- colorNumeric(
        palette = "Reds", domain = zcta$case
      )
    }
    
    if (input$type == "Total"){
      pal <- colorNumeric(
        palette = "Purples", domain = zcta$case
      )
    }
    
    labels <- paste0("zip code: ", zcta$ZCTA5CE10,"<br/>",
                     "case number: ", zcta$case) %>%
      lapply(htmltools::HTML)
    
    zcta %>% ungroup() %>% leaflet %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(fillColor = ~pal(case), weight = 1, opacity = 1,
                  color = "white", dashArray = "2", fillOpacity = 0.7,
                  label = labels) %>%
      addLegend(pal = pal,
                values = ~case,
                title = "COVID MAP",
                position = "topright")
    
  })
  
  ##############################################################################
  # Restaurant Map Tab
  ##############################################################################
  observeEvent( input$range, {
    
    output$restaurant_map<-renderLeaflet({  
      app_data.sub <- app_data[ (app_data$rating>=input$range[1])&(app_data$rating<=input$range[2]),]
      app_data.sub1 <- app_data.sub[ app_data.sub$Postcode_x == input$zip,  ]
      
      if (input$zip != "all"){
        if (input$cluster1 == "ENABLE")
        {
          map1 <- leaflet(app_data.sub1) %>%
            addProviderTiles("CartoDB.Voyager") %>%
            setView(-74.00, 40.71, zoom = 11)%>%
            addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, 
                              icon = awesomeIcons(markerColor= "purple",
                                                  text = fa("utensils")), 
                              popup = paste(
                                "<b>Restaurant Name:</b>", app_data.sub1$Restaurant.Name,"<br>",
                                "<b>Address:</b>", app_data.sub1$Business.Address, 
                                app_data.sub1$Postcode_x,  "<br>",
                                "<b>Seating Choice:</b>", app_data.sub1$SeatingChoice, "<br>",
                                "<b> Recommendation Rate:</b>", app_data.sub1$rating , "<br>"
                              ),
                              clusterOptions = markerClusterOptions())
        }
        else
        {map1 <- leaflet(app_data.sub1) %>%
          addProviderTiles("CartoDB.Voyager") %>%
          setView(-74.00, 40.71, zoom = 11)%>%
          addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, 
                            icon = awesomeIcons(markerColor= "purple",
                                                text = fa("utensils")), 
                            popup = paste(
                              "<b>Restaurant Name:</b>", app_data.sub1$Restaurant.Name, "<br>", 
                              "<b>Address:</b>", app_data.sub1$Business.Address,
                              app_data.sub1$Postcode_x,  "<br>",
                              "<b>Seating Choice:</b>", app_data.sub$SeatingChoice, "<br>",
                              "<b>Recommendation Rate:</b>", app_data.sub1$rating , "<br>"))
        }
      }
      else if(input$zip == "all"){
        if (input$cluster1 == "ENABLE")
        {
          map1 <- leaflet(app_data.sub) %>%
            addProviderTiles("CartoDB.Voyager") %>%
            setView(-74.00, 40.71, zoom = 11)%>%
            addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, 
                              icon = awesomeIcons(markerColor= "purple",
                                                  text = fa("utensils")), 
                              popup = paste(
                                "<b>Restaurant Name:</b>", app_data.sub$Restaurant.Name, "<br>",
                                "<b>Address:</b>", app_data.sub$Business.Address,
                                app_data.sub$Postcode_x,  "<br>",
                                "<b>Seating Choice:</b>", app_data.sub$SeatingChoice, "<br>",
                                "<b>Recommendation Rate:</b>", app_data.sub$rating , "<br>"),
                              clusterOptions = markerClusterOptions())
        }
        else
        {map1 <- leaflet(app_data.sub) %>%
          addProviderTiles("CartoDB.Voyager") %>%
          setView(-74.00, 40.71, zoom = 11)%>%
          addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, 
                            icon = awesomeIcons(markerColor= "purple",
                                                text = fa("utensils")), 
                            popup = paste(
                              "<b>Restaurant Name:</b>", app_data.sub$Restaurant.Name,"<br>",
                              "<b>Address:</b>", app_data.sub$Business.Address, 
                              app_data.sub$Postcode_x,  "<br>",
                              "<b>Seating Choice:</b>", app_data.sub$SeatingChoice, "<br>",
                              "<b>Recommendation Rate:</b>", app_data.sub$rating , "<br>"))
        }
      }
    })
  })
  
  
})
