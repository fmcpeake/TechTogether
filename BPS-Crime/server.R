library(shiny)
library(sp)
library(sf)
library(mapview)
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
    c(clicked_mapview$clickedMarker$lng, clicked_mapview$clickedMarker$lat)
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
      add_bars(x = "Science", y = sch$Science, name = "MCAS Science Score", width = .3) %>%
      add_bars(x = "Math", y = sch$Math, name = "Standardized MCAS Math Score", width = .3)%>%
      add_bars(x = "English", y = sch$ELA, name = "Standardized MCAS ELA Score", width = .3)%>%
      layout(title = 'Standardized MCAS Scores',
             showlegend = F,
             xaxis = list(title = ""),
             yaxis = list(range = c(0,100)))
  })
  
  output$crime_around_schools=renderPlotly({
    sch=selected_data()
    if(is.null(sch))
      return(NULL)
    
    plot_ly() %>%
      add_bars(x = "Domestic", y = sch$Domestic, width = .5) %>%
      add_bars(x = "Drugs", y = sch$Drugs, width = .5)%>%
      add_bars(x = "Missing Person", y = sch$`Missing Person`, width = .5)%>%
      add_bars(x = "Violent", y = sch$Violent, width = .5)%>%
      layout(title = 'Number of Crimes',
             showlegend = F,
             xaxis = list(title = ""))
  })
  
  output$science_and_crime=renderPlotly({
    sch=selected_data()
    if(is.null(sch))
      return(NULL)
    
    plot_ly(finaldata, x = ~totalcrime, y = ~Science, mode = "markers", type = "scatter",
            marker = list(size = 10,
            color = 'rgba(145,191,219, .9)',
            width = 2)) %>%
      add_trace(x = sch$totalcrime, y = sch$Science, mode = "markers",
                name = sch$school_name_2,
                marker = list(color='rgba(252,141,89, .8)')) %>%
      layout(title = 'Science Score and Crime',
             showlegend = F,
             yaxis = list(zeroline = FALSE),
             xaxis = list(zeroline = FALSE, title = "Total Crime"))
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