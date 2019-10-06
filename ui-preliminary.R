library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(shinyBS)
library(RColorBrewer)
library(geosphere)
library(googleway)
library(ggmap)

dataset <- diamonds

ui <- fluidPage(
  
    title ="Retail Intelligence",
    
    
    
    tags$div(id="outer",
             h2("X-MART"),
             tags$head(
                 # Include our custom CSS
                 includeCSS("C:/Users/ali518/Desktop/X-Mart/Trial1/style.css")
                 
             ),
             sidebarLayout(
                 
                 
                 sidebarPanel(id = "side",
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
                     tags$div(sliderInput("Radius","Radius",min=0,max=20,step=0.5,value=4)),useShinyjs(),
                     actionButton("button",tags$div(id ="Button-Background",tags$div(id="White", tags$em(id ="Values","Find Location")),color="White"))
                     
                 ),
                 
               mainPanel(
                 
                 google_mapOutput(outputId = "map"),
                     uiOutput("Details"),
                     uiOutput("Allocation")
                 )),
             # hr(),
             
             
                 hidden(div(id = "panel",
                        
                         tabPanel("Malls", uiOutput("Categories"))
                       
                       ))
                       
             
    
    )
    
    )
