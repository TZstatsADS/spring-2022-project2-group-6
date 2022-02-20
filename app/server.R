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

# import data
bike_count <- read.csv('../output/Processed-bikecount-month.csv')
bike_count_boroughs <- read.csv('../output/Processed-bikecount-month-boroughs.csv')
open_streets_geo <- geojsonio::geojson_read("../data/Open Streets Locations.geojson", what ="sp")
open_streets <- read.csv('../output/Processed_Open_Streets_Locations.csv')
arrests <- read.csv('../output/NYPD_Arrests_Data_Recent.csv')
#shootings <- read.csv('../output/NYPD_Shooting_Data_Recent.csv')
covid7day <- read.csv('../output/covid_last7day.csv')
covid_total <- read.csv('../output/covid_total_case.csv')
shootings_zipcodes <- read.csv('../output/Processed-shootings-zipcodes.csv')
open_restaurants <- read.csv('../output/open_restaurants.csv')

# Get Data
shinyServer(function(input, output) {
  
  
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
      hc_title(text = "TITLE") %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
      hc_caption( align = 'center', style = list(color = "black"), text = 'Caption')
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
      hc_xAxis( categories = unique(bike_count_per_year_per_borough$Year) ) %>%
      hc_yAxis( title = list(text = "Number of bikes")) %>%
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
      hc_title(text = "TITLE") %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
      hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')
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
      hc_title(text = "TITLE") %>%
      hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')  %>%
      hc_exporting(enabled = TRUE) %>%
      hc_colors(c("#D7DCEA", "#A1B3D7", "#6581BF", "#2F57AB", "#0B389D")) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">Trimester {point.key}</span>'
      )
    
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
        hc_caption( align = 'center', style = list(color = "black"), text = 'Caption')
    }
    else if (!input$per_day & !input$aggregated) {
    }
    else{
      if (input$per_day){
        use_name <- 'Per Day'
        use_data <- open_streets_dates$Total_count
        use_color <- '#6581BF'
      }
      if (input$aggregated){
        use_name <- 'Aggregated'
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
        hc_title(text = "Open Streets Approvals") %>%
        hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 50, y = 40 ) %>%
        hc_caption( align = 'center', style = list(color = "black"), text = 'Caption')
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
      hc_yAxis(title = list(text = "Number of open restaurants"),
               max = max(open_restaurants_borough$Total_count)) %>%
      hc_title(text = "TITLE") %>%
      hc_caption( align = 'center', style = list(color = "black"), text = 'caption')  %>%
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
      hc_title(text = "TITLE") %>%
      hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')  %>%
      hc_exporting(enabled = TRUE) %>%
      hc_colors(c("#c1cbb4", "#d6db90", "#789f52", "#2c5d37")) %>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">{point.key}</span>'
      )
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
  
  
})
