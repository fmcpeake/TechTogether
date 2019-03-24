library(leaflet)
library(plotly)
shinyUI(fluidPage(
  fluidRow(
    column(width=6,align="left",
           br(),
           tags$h4(tags$strong("Boston Schools and Crime",
                               style="text-align:center;color: #0000ff")),
           
           leafletOutput("mapview",height = 500)
    ),
    
    column(width=6,align="center",
           #conditionalPanel(
             
             # condition = "output.condition1 == 0",
             # br(),
             # tags$h4("About",style='color:blue'),
             # tags$p('...',span(tags$a(href="https://openweathermap.org/forecast16", "OpenWeatherMap.")) ,style="text-align:left;color:black;font-size:140%")
             # ),
             # 
             tabsetPanel(
               tabPanel(tags$em("Academic Performance",style="font-size:120%"),
                        tags$hr(style="border-color: #ffc266;"),
                        plotlyOutput("school_info")
                          
                        ),
               tabPanel(tags$em("Crime Around Schools",style="font-size:120%"),
                        tags$hr(style="border-color:  #d27979;"),
                        plotlyOutput("crime_around_schools")),
               
               tabPanel(tags$em("Science MCAS and Crime",style="font-size:120%"),
                        tags$hr(style="border-color:  #d27979;"),
                        plotlyOutput("science_and_crime"))
                        )
             ))
))
