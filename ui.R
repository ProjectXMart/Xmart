library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(leaflet)

dataset <- diamonds

ui <- fluidPage(
    title ="Retail Intelligence",
   
    tags$div(id="outer",
             h2("X-MART"),
        tags$head(
            # Include our custom CSS
            includeCSS("C:/Users/hkumar062/Documents/New folder/X-MART/www/styles.css")
        
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
                leaflet()
            )),
       # hr(),
    fluidRow( column=4,
        tabsetPanel(
            tabPanel("Segment",tags$div(id ="Locations-broken-dow",tags$h5(tags$em("Location broken down by segmentation"))),
                    tags$div(id="outer",
                         # A static valueBox
                        plotOutput("M1",height = "120px"),
                        plotOutput("M2",height = "120px")
                       # plotOutput("M3",height = "400px")
                     )
                     ), 
            tabPanel("Location", verbatimTextOutput("summary")), 
            tabPanel("Category", tableOutput("table"))
        )
)))
