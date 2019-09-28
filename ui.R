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
            hidden(div(id = "panel", tabsetPanel(
                        
                         tabPanel("Malls", uiOutput("Categories"))
                       
                       )))
                       
             
    
    )
    
    )
