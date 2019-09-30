library(googleway)
library(geosphere)
library(ggmap)
server <- function(input, output,session) {
  observeEvent(input$button,{
  
  ######Single Option Trade Area Calculation####
    register_google(key="")
    
    jio<-geocode("18 york st, toronto, on m5j 0b2, canada",output ="latlona", source ="google")
    
    lati<-jio$lat
    longi<-jio$lon
    
    a<-c(longi,lati)
    
    fsacentroids <- read.csv("C:/Users/hkumar062/Desktop/X_MART/www/FSACentroids.csv",stringsAsFactors = FALSE)
      for (i in 1:nrow(fsacentroids)){
        
        b<-c(fsacentroids$Lng[i],fsacentroids$Lat[i])
        fsacentroids$geodis[i]<-distm(a,b, fun = distHaversine)
        
      }
    
    fsacentroids$geodiskm<-fsacentroids$geodis/1000
    
    tafsa<-subset(fsacentroids,geodiskm<=input$Radius)
    
    pccentroids<-read.csv("C:/Users/hkumar062/Downloads/Lat Long Canada.csv",stringsAsFactors = FALSE)
    
    tapccentroids<-merge(tafsa,pccentroids,by="FSA")

    #names(tapccentroids)    
    # write.csv(jio,"C:/Users/hkumar062/Downloads/jio1.csv")
   # str(tapccentroids)
    
    for (i in 1:nrow(tapccentroids)){
      
      b<-c(as.numeric(tapccentroids$Lng.x[i]),as.numeric(tapccentroids$Lat.x[i]))
            tapccentroids$geodis[i]<-distm(a,b, fun = distHaversine)
      
    }
    
    tapccentroids$geodiskm<-tapccentroids$geodis/1000
    
    tradearea<-subset(tapccentroids,geodiskm<=input$Radius)
 write.csv(tradearea,"C:/Users/hkumar062/Downloads/tradearea_new.csv")
  
    output$map <-renderGoogle_map({
    map_key <- ""
    set_key(key =map_key)
    google_map(data = tradearea, key = map_key) %>%
      add_markers(lat = "Lat.x", lon = "Lng.y", info_window = "FSA")
    
    })
  }
  )
  
 tradearea = read.csv("C:/Users/ali518/Desktop/X-Mart/Trial1/tradearea_new.csv")
     tradearea = tradearea$PostCode

    

        p = read.csv("C:/Users/ali518/Desktop/X-Mart/Trial1/persona.csv")
        persona = p %>% filter(PostCode %in% tradearea)
        persona = count(persona, Segmentation)
        persona$n = round(persona$n*100/sum(persona$n),0)
        persona = persona[persona$n != 0, ]
        persona <- persona %>%
          mutate(item = "Persona")
        
        
        test= readRDS("C:/Users/ali518/Desktop/X-Mart/table.rds")
        test = test %>% filter(PostCode %in% tradearea)
        test = test %>% filter(Segmentation %in% persona$Segmentation)
        test = test %>% group_by(Segmentation) %>% summarise_if(is.numeric,mean)
        test = t(test)
       
        


     

      affluence = read.csv("C:/Users/ali518/Desktop/X-Mart/Trial1/affluence.csv")
      affluence = affluence %>% filter(PostCode %in% tradearea)
      affluence = count(affluence, Segmentation)
      affluence$n = round(affluence$n*100/sum(affluence$n),0)
      affluence = affluence[affluence$n != 0, ]
      affluence <- affluence %>% 
        mutate(item = "Affluence")
        
      affluence <- affluence[order(affluence$n),]
    
    
    
  
      behaviour = read.csv("C:/Users/ali518/Desktop/X-Mart/Trial1/behaviour.csv")
      behaviour = behaviour %>% filter(PostCode %in% tradearea)
      behaviour = count(behaviour, Segmentation)
      behaviour$n = round(behaviour$n*100/sum(behaviour$n),0)
      behaviour = behaviour[behaviour$n != 0, ]
      behaviour <- behaviour %>% 
        mutate(item = "Behaviour")
      
      behaviour <- behaviour[order(behaviour$n),]



      category <- readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/category.rds")
      category = category %>% filter(PostCode %in% tradearea)
      category = category %>% group_by(Category) %>% summarise(spend = sum(High.Street.Spend))
      category = as.data.frame(category)
      category = category[order(-category$spend),]
      rank = as.character(category$Category)
      category = category %>% mutate(Category = paste("#",1:nrow(category),"-",Category))
    
    
  
    
        
      b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/FURN.rds")
      b1 = b1 %>% filter(PostCode %in% tradearea)
      b1 = apply(b1[,3:ncol(b1)], 2, mean)
      b1 = as.matrix(b1)
      b1 = as.data.frame(b1)
      b1 <- b1 %>%
        mutate(Brand = rownames(b1))
      b1 = b1[order(-b1$V1),]
      Furniture = b1
      
      
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Book.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Books = b1
      # 
      b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Cloth.rds")
      b1 = b1 %>% filter(PostCode %in% tradearea)
      b1 = apply(b1[,3:ncol(b1)], 2, mean)
      b1 = as.matrix(b1)
      b1 = as.data.frame(b1)
      b1 <- b1 %>%
        mutate(Brand = rownames(b1))
      b1 = b1[order(-b1$V1),]
      Clothing = b1
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/HI.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # HomeImprovement = b1
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Jewel.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Jewellery = b1
      # 
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Optic.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Optical = b1
      # 
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Pet.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Pet = b1
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/REST.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Restaurants = b1
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Shoe.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Shoes = b1
      # 
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Sport.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Sports = b1
      # 
      # b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Toy.rds")
      # b1 = b1 %>% filter(PostCode %in% tradearea)
      # b1 = apply(b1[,3:ncol(b1)], 2, mean)
      # b1 = as.matrix(b1)
      # b1 = as.data.frame(b1)
      # b1 <- b1 %>%
      #   mutate(Brand = rownames(b1))
      # b1 = b1[order(-b1$V1),]
      # Toys = b1
      # 
  
    
    
    output$Details <- renderUI({
      bsModal("modalExample", "Details", trigger = "", size = "large",
              
              fluidRow(column(1,br(),renderText("Persona")),
                       
                       column(1,align = "center",offset = 0,useShinyjs(),div(style = "margin-top: 12px; margin-right: 0px",actionButton("p",class="expand_more_10px", type="button" ,
                tags$span(class="caret")))),column(10,offset = 0,align = "left",style='padding:0px;',
                      
              renderPlot(ggplot(persona, aes(x = item, y = n, fill = forcats::fct_rev(Segmentation),order = n)) +
                           geom_col() +
                           geom_text(aes(label = paste(n, "%")),
                                     position = position_stack(vjust = 0.5)) +coord_flip()+
                           theme_minimal(base_size = 16) +
                           ylab(NULL) +
                           xlab(NULL) + 
                           theme(legend.position = "bottom",panel.grid = element_blank(), 
                                 axis.title = element_blank(),axis.ticks.x = element_blank(),axis.text=element_blank(),legend.title = element_blank())+
                           scale_fill_manual(values= c("#ffd9c7","#f3ab89","#e27c4f","#cc4b14"))+
                           guides(fill = guide_legend(reverse = TRUE)),height = 120),hidden(tableOutput("table_persona")))),
              
              fluidRow(column(1,br(),renderText("Affluence")),
                       
                       column(1,align = "center",offset = 0,useShinyjs(),div(style = "margin-top: 12px; margin-right: 0px",actionButton("a",class="expand_more_10px", type="button" ,
                                                                                                                                        tags$span(class="caret")))),column(10,offset = 0,align = "left",style='padding:0px;',
                                                                                                                                                                           
                                                                                                                                                                           renderPlot(ggplot(affluence, aes(x = item, y = n, fill = forcats::fct_rev(Segmentation),order = n)) +
                                                                                                                                                                                        geom_col() +
                                                                                                                                                                                        geom_text(aes(label = paste(n, "%")),
                                                                                                                                                                                                  position = position_stack(vjust = 0.5)) +coord_flip()+
                                                                                                                                                                                        theme_minimal(base_size = 16) +
                                                                                                                                                                                        ylab(NULL) +
                                                                                                                                                                                        xlab(NULL) + 
                                                                                                                                                                                        theme(legend.position = "bottom",panel.grid = element_blank(), 
                                                                                                                                                                                              axis.title = element_blank(),axis.ticks.x = element_blank(),axis.text=element_blank(),legend.title = element_blank())+
                                                                                                                                                                                        scale_fill_manual(values= c("#ffd9c7","#f3ab89","#e27c4f","#cc4b14"))+
                                                                                                                                                                                        guides(fill = guide_legend(reverse = TRUE)),height = 120),hidden(tableOutput("table_affluence")))),
              fluidRow(column(1,br(),renderText("Behaviour")),
                       
                       column(1,align = "center",offset = 0,useShinyjs(),div(style = "margin-top: 12px; margin-right: 0px",actionButton("b",class="expand_more_10px", type="button" ,
                                                                                                                                        tags$span(class="caret")))),column(10,offset = 0,align = "left",style='padding:0px;',
                                                                                                                                                                           
                                                                                                                                                                           renderPlot(ggplot(behaviour, aes(x = item, y = n, fill = forcats::fct_rev(Segmentation),order = n)) +
                                                                                                                                                                                        geom_col() +
                                                                                                                                                                                        geom_text(aes(label = paste(n, "%")),
                                                                                                                                                                                                  position = position_stack(vjust = 0.5)) +coord_flip()+
                                                                                                                                                                                        theme_minimal(base_size = 16) +
                                                                                                                                                                                        ylab(NULL) +
                                                                                                                                                                                        xlab(NULL) + 
                                                                                                                                                                                        theme(legend.position = "bottom",panel.grid = element_blank(), 
                                                                                                                                                                                              axis.title = element_blank(),axis.ticks.x = element_blank(),axis.text=element_blank(),legend.title = element_blank())+
                                                                                                                                                                                        scale_fill_manual(values= c("#ffd9c7","#f3ab89","#e27c4f","#cc4b14"))+
                                                                                                                                                                                        guides(fill = guide_legend(reverse = TRUE)),height = 120),hidden(tableOutput("table_behaviour"))))
                       
                       )
                  
              
              
      
    })
    
    output$Allocation <- renderUI({
      bsModal("sq_all", "Optimal Space Allocation", trigger = "", size = "large")
    })
    
    
    output$table_persona <- renderTable({
      test
    }, rownames = TRUE, colnames = FALSE)
    
    
    observeEvent(input$seg, {
      toggleModal(session, "modalExample", "open")
    })
    
    observeEvent(input$p, {
      toggle("table_persona")
    })
    
    observeEvent(input$alloc, {
      toggleModal(session,"sq_all","open")
    })

    
    output$m1_1 <- renderText({
        "East York Mall"
    })
    
    output$m1_2 <- renderUI({
        HTML(paste("385 Noah Place Suit 878","Toronto, ON M6K 3P0",sep = "<br/>"))
    })
    
    #Render Mall category Breakdown
    output$Categories <- renderUI({
      lapply(1:1, function(i){
        
        fluidRow(style = "border-bottom: 1px solid rgba(0, 0, 0, .2);",
                 column(3, offset = 1, br(), br(),br(),
                        textOutput("m1_1"),tags$head(tags$style("#m1_1{font-weight: bold; font-size: 180%; text-align: left; vertical-align: middle;}")),
                        htmlOutput("m1_2"),tags$head(tags$style("#m1_2{font-size: 150%; opacity: 0.75; text-align: left; vertical-align: top;}")),
                        actionButton("seg","Segmentation",style = "background-color: white; color: chocolate; text-decoration: underline; border-style: none;font-size: 120%;"),
                        actionButton("alloc","Recommended Space Allocation",style = "background-color: white; color: chocolate; text-decoration: underline; border-style: none;font-size: 120%;")),
                 
                 column(8,tags$h4("Top Retail Category Opportunity",align = "left"), uiOutput("c1")))
        
      })
    })
    
    
    #fill color
    color = rev(c(brewer.pal(9,"YlOrRd"),brewer.pal(9,"Oranges")))
    
    brand_color = c(brewer.pal(11,"PRGn"),brewer.pal(9,"Purples"),brewer.pal(9,"Blues"))
    
    #mall 1 should be pre-ordered
    output$c1 <- renderUI({
       lapply(1:nrow(category), function(i){
           b_name = paste("hideshow",i,sep="")
           brand = rank[i]
          fluidRow(column(1,br(),renderText(as.character(category[i,1]))),column(1,align = "center",offset = 0,useShinyjs(),div(style = "margin-top: 12px; margin-right: 0px",actionButton(b_name,class="expand_more_10px", type="button" ,
                                                                                          tags$span(class="caret")))),column(10,offset = 0,align = "left",style='padding:0px;',
          renderPlot(ggplot(category[i,], aes(x = Category, y = spend)) +
                         geom_bar(stat = "identity",width = 0.9,fill = color[i]) +coord_flip()+
                         theme_minimal() +
                         ylab(NULL) +
                         xlab(NULL) + 
                         theme(legend.position = "none", panel.grid = element_blank(), 
                              axis.title = element_blank(),axis.text = element_blank()) +
                         ylim(0,max(category$spend)),height = 60),hidden(plotOutput(brand,height = "300px"))))
           
                         
       })
      
    })
    
  
    
    
    lapply(1:nrow(category), function(i){
      
      observeEvent(input[[paste("hideshow",i,sep="")]], {
        # every time the button is pressed, alternate between hiding and showing the plot
        toggle(rank[i])
      })
      
    })
    
    
    
    
    
    observeEvent(input$button, {
      show("panel")
    })
    
    
    output$Furniture<- renderPlot({ 
        
        ggplot(Furniture, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
            geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
            theme_minimal(base_size = 16) +
            ylab(NULL) +
            xlab(NULL) + 
            theme(legend.position = "none", panel.grid = element_blank(), 
                  axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Furniture$V1))

    })  
    
    
    output$Clothing<- renderPlot({ 
      
      ggplot(Clothing, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Clothing$V1))
      
    })  
}
