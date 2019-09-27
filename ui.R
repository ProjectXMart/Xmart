library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(DT)
library(tidyverse)
library(googleway)


ui <- fluidPage(
    title ="Retail Intelligence",
    
    tags$div(id="outer",
             h2("X-MART"),
             tags$head(
                 # Include our custom CSS
                 includeCSS("C:/Users/hkumar062/Documents/New folder/X-MART/www/styles.css"),
                 includeCSS("https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/css/bootstrap.min.css"),
                 tags$head(includeScript("C:/Users/hkumar062/Documents/New folder/X-MART/www/mapsP.js"))
                 
                 
             ),
             sidebarLayout(
                 
                 
                 sidebarPanel(
                     h4(class ='Location-Input',"Location Input"),
                     tags$div(class="line", br()),
                     setSliderColor("#d55404",c(1)),
                     selectInput("inputId", "Location Type",   list('Multiple','Single'), selected = NULL, multiple = FALSE,
                                 selectize = TRUE, width = NULL, size = NULL),
                     conditionalPanel("input.inputId == 'Multiple'",
                                      # Only prompt for threshold when coloring or sizing by superzip
                                      fileInput('datafile', 'Choose CSV file',
                                                accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                                      
                     ),
                     conditionalPanel("input.inputId == 'Single'",
                                      # Only prompt for threshold when coloring or sizing by superzip
                                      textInput("Place","Enter the complete address of your retail property")),
                     # chooseSliderSkin("Orange"),
                     tags$div(sliderInput("Radius","Radius",min=0,max=20,step=0.5,value=4)),
                     actionButton("button",tags$div(id ="Button-Background",tags$div(id="White", tags$em(id ="Values","Find Location")),color="White"))
                     
                 ),
                 
                 mainPanel(
                     #tags$div(id="map",
                    
                    # df <- data.frame(lat = -79.347015,
                     #                 lon = 43.651070,
                     #                 info = '<div id="bodyContent"><iframe width="640" height="390" src="//www.youtube.com/embed/a8UOzBUZxCw" frameborder="0" allowfullscreen></iframe></div>'),
                     
                   
                      #   add_markers(data = df, info_window = "info")
                   google_mapOutput(outputId = "map")
                     
                     
                  
                                    
                 )),
             # hr(),
             fluidRow( column=4,
                       tabsetPanel(
                           tabPanel("Segment",tags$div(id ="Locations-broken-dow",tags$h5(tags$em("Location broken down by segmentation"))),
                                    tags$div(id="outer",
                                             # A static valueBox
                                             plotOutput("M1",height = "120px"),
                                             tags$div (class="expand_more_24px",
                                                       actionButton("dropb",class="expand_more_24px", type="button" ,
                                                                    tags$span(class="caret"))),
                                             tags$div( class ="dt",DT::dataTableOutput("mytable"))),
                                    tags$div(id="outer",
                                             plotOutput("M2",height = "120px"),
                                             tags$div (class="expand_more_24px",
                                                       tags$button (class="expand_more_24px", type="button" ,
                                                                    tags$span(class="caret")))
                                             # plotOutput("M3",height = "400px")
                                    )
                           ), 
                           tabPanel("Location", verbatimTextOutput("summary"),fluidRow(style = "border-bottom: 1px solid rgba(0, 0, 0, .2);",
                                                                                       column(3, offset = 1, br(), br(),br(),
                                                                                              textOutput("m1_1"),tags$head(tags$style("#m1_1{font-weight: bold; font-size: 120%; text-align: left; vertical-align: middle;}")),
                                                                                              textOutput("m1_2"),tags$head(tags$style("#m1_2{font-size: 100%; opacity: 0.75; text-align: left; vertical-align: top;}"))),
                                                                                       column(8,tags$h4("Top Retail Category Opportunity",align = "left"), plotOutput(outputId = "Mall1", height = "200px"))),
                                    fluidRow(style = "border_bottom: 1px solid rgba(0, 0, 0, .2);",
                                             column(3, offset = 1, br(),
                                                    textOutput("m2_1"),tags$head(tags$style("#m2_1{font-weight: bold;font-size: 120%; text-align: left; vertical-align: middle;}")),
                                                    textOutput("m2_2"),tags$head(tags$style("#m2_2{font-size: 100%; opacity: 0.75; text-align: left; vertical-align: top;}"))),
                                             column(8, plotOutput(outputId = "Mall2", height = "200px")))), 
                           
                           tabPanel("Category", tableOutput("table"),fluidRow(column(1),style = "border-bottom: 1px solid rgba(0, 0, 0, .2);",
                                                                              column(8,tags$h4("Brand Index",align = "left",opacity = 0.4), plotOutput(outputId = "b1", height = "200px"))))
                       )
                       
             )))
