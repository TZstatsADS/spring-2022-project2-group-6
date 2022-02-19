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

# Get Data
shinyServer(function(input, output) {
  
  # import data
  bike_count = read.csv('../output/Processed-bikecount-month.csv')
  bike_count_boroughs = read.csv('../output/Processed-bikecount-month-boroughs.csv')
  open_streets_geo <- geojsonio::geojson_read("../data/Open Streets Locations.geojson", what ="sp")
  open_streets <- read.csv('../output/Processed_Open_Streets_Locations.csv')
  
  
  
  ##############################################################################
  # Bikes tab
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
  
  # output$mytable = DT::renderDataTable({
  #   bike_count_per_quarter_from_2017_per_borough
  # })
  
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
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 ) %>%
      hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')
  })
  
  output$bike_count_month <- renderHighchart({
     if (input$bike_count_borough_month == 3) {
        hchart(bike_count_per_quarter_from_2017, "column",
               hcaes(x = Month, y = Total_count, group = Year)) %>%
          hc_chart(zoomType = "x") %>%
          hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
          hc_xAxis(title = list(text = "Trimester")) %>%
          hc_yAxis(title = list(text = "Number or bikes"),
                   max = max(bike_count_per_quarter_from_2017$Total_count)) %>%
          hc_title(text = "TITLE") %>%
          hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')  %>%
          hc_exporting(enabled = TRUE) %>%
          hc_colors(c("#D7DCEA", "#A1B3D7", "#6581BF", "#2F57AB", "#0B389D"))
    }
    
    else if (input$bike_count_borough_month == 1) {
      hchart(subset(bike_count_per_quarter_from_2017_per_borough, borough=="Manhattan"), "column",
             hcaes(x = Month, y = Total_count, group = Year)) %>%
        hc_chart(zoomType = "x") %>%
        hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
        hc_xAxis(title = list(text = "Trimester")) %>%
        hc_yAxis(title = list(text = "Number or bikes"),
                 max = max(bike_count_per_quarter_from_2017_per_borough$Total_count)) %>%
        hc_title(text = "TITLE") %>%
        hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')  %>%
        hc_exporting(enabled = TRUE) %>%
        hc_colors(c("#D7DCEA", "#A1B3D7", "#6581BF", "#2F57AB", "#0B389D"))
    }
    
    else if (input$bike_count_borough_month == 2) {
      hchart(subset(bike_count_per_quarter_from_2017_per_borough, borough=="Brooklyn"), "column",
             hcaes(x = Month, y = Total_count, group = Year)) %>%
        hc_chart(zoomType = "x") %>%
        hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
        hc_xAxis(title = list(text = "Trimester")) %>%
        hc_yAxis(title = list(text = "Number or bikes"),
                 max = max(bike_count_per_quarter_from_2017_per_borough$Total_count)) %>%
        hc_title(text = "TITLE") %>%
        hc_caption( align = 'center', style = list(color = "black"), text = '2021 had data only till August')  %>%
        hc_exporting(enabled = TRUE) %>%
        hc_colors(c("#D7DCEA", "#A1B3D7", "#6581BF", "#2F57AB", "#0B389D"))
    }
  })
  
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
  
})
