library(shiny)
library(sp)
library(sf)
library(mapview)
library(tidyverse)
library(plotly)
library(png)
library(stringi)

load("finaldata.Rdata")

shinyServer(function(input, output) {
  
  pop <- finaldata %>%
    mutate('School Size' = School_Total,
           'School Name' = school_name_2,
           Address = ADDRESS) %>%
    select('School Name', Address, 'School Size')
  
  coordinates(finaldata) <- ~ X + Y
  proj4string(finaldata) <- "+init=epsg:4326"
  mp <- mapview(finaldata, zcol="SCH_TYPE", legend=TRUE, cex=8,
           popup=popupTable(pop)) 
  
  output$mapview <- renderMapview(mp) 
  
  clicked_mapview <- reactiveValues(clickedMarker=NULL)
  observeEvent(input$mapview_marker_click,{
    clicked_mapview$clickedMarker <- input$mapview_marker_click
  })
  
  selected_coordinates= reactive(({
    c(clicked_mapview$clickedMarker$lng,clicked_mapview$clickedMarker$lat)
  }))
  
  selected_data <- reactive(({
    if(is.null(clicked_mapview$clickedMarker))
      return(NULL)
    
    filter(finaldata, X == as.numeric(as.character(selected_coordinates()[1])), Y==as.numeric(as.character(selected_coordinates()[2])))
  }))
  
  output$school_info=renderPlotly({
    sch=selected_data()
    if(is.null(sch))
      return(NULL)
    plot_ly() %>%
      add_bars(y = sch$Science, name = "MCAS Science Score") %>%
      add_bars(y = sch$Math, name = "Standardized MCAS Math Score")%>%
      add_bars(y = sch$ELA, name = "Standardized MCAS ELA Score")%>%
      layout(title = '',
             legend = list(orientation = 'h'),
             xaxis = list(title = ""))
  })
  
  output$max_min_temperature_plotly_3days=renderPlotly({
    temp=selected_data()
    if(is.null(temp))
      return(NULL)
    
    temp=temp[1:3,]
    plot_ly() %>%
      add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
      add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
      add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
      add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
      add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
      add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
      layout(title = '',
             legend = list(orientation = 'h'),
             xaxis = list(title = ""))
  })
  
  output$max_min_temperature_plotly_5days=renderPlotly({
    temp=selected_data()
    if(is.null(temp))
      return(NULL)
    temp=temp[1:5,]
    plot_ly() %>%
      add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
      add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
      add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
      add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
      add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
      add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
      layout(title = '',
             legend = list(orientation = 'h'),
             xaxis = list(title = ""))  
  })
  
  
  output$humidty_rain_cloudness_16days=renderPlotly({
    temp=selected_data()
    if(is.null(temp))
      return(NULL)
    
    plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
      add_trace(y = ~humidty, name = 'Humidity') %>%
      layout(yaxis = list(title = '%'), barmode = 'group')%>%
      add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(rain, '°F')) %>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))
  })
  
  
  output$humidty_rain_cloudness_5days=renderPlotly({
    temp=selected_data()
    if(is.null(temp))
      return(NULL)
    temp=temp[1:5,]
    plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
      add_trace(y = ~humidty, name = 'Humidity') %>%
      layout(yaxis = list(title = '%'), barmode = 'group')%>%
      add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(rain, '°F')) %>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))      
  })
  
  
  output$humidty_rain_cloudness_3days=renderPlotly({
    temp=selected_data()
    if(is.null(temp))
      return(NULL)
    temp=temp[1:3,]
    plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
      add_trace(y = ~humidty, name = 'Humidity') %>%
      layout(yaxis = list(title = '%'), barmode = 'group')%>%
      add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(rain, '°F')) %>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))     
  })
  
  condition1<-reactive({
    if(is.null(selected_data())){
      result=0
    }else{
      result=1
    }
    result
  })
  
  output$condition1 <- renderText({
    condition1()
  })
  
  outputOptions(output, 'condition1', suspendWhenHidden=FALSE)
  
})