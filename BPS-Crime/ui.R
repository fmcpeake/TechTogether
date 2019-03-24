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
           conditionalPanel(
              condition = "output.condition1 == 0",
              br(),
              tags$h4("About",style='color:blue'),
              tags$p('Some Boston public schools have more underperforming students and dropout incidents than others.These outcomes can be perceived to be caused by factors such as physical ability, cognitive ability, motivation, family background, and race. However, there is an increasing number of articles indicating association between academic performance and crime rate. In order to gain understanding based on evidence, we would like to investigate external factors, specifically crime rates in the surrounding neighborhood and the corresponding academic performances.' ,style="text-align:left;color:black;font-size:100%")
              ),
           conditionalPanel(
             condition = "output.condition1 == 1",
             br(),
             tabsetPanel(
               tabPanel(tags$em("Academic Performance",style="font-size:100%"),
                        tags$hr(style="border-color: #d27979;"),
                        plotlyOutput("school_info")),
               
               tabPanel(tags$em("Crime Around Schools",style="font-size:100%"),
                        tags$hr(style="border-color:  #d27979;"),
                        plotlyOutput("crime_around_schools")),
               
               tabPanel(tags$em("Science MCAS and Crime",style="font-size:100%"),
                        tags$hr(style="border-color:  #d27979;"),
                        plotlyOutput("science_and_crime"))
                        ))
             ))
))
