server = function(input, output) {
  p <- reactive({
    
    persona <- data.frame(Segmentation = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), 
                          Proportion = c(57, 21, 15, 7))
    
    persona <- persona %>% 
      mutate(item = "Persona")
    
    
  })
  
  a <- reactive({
    
    affluence <- data.frame(Segmentation = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), 
                            Proportion = c(34, 30, 21, 15))
    
    affluence <- affluence %>% 
      mutate(item = "Affluence")
    
    affluence <- affluence[order(affluence$Proportion),]
  })
  
  mall <- reactive({
    
    mall <- data.frame(section = c("#1 Retail", "#2 Services", "#3 Restaurant", "#4 Foods"), 
                       Proportion = c(200, 175, 150, 50))
    
    
    
  })
  
  mall1 <- reactive({
    
    mall1 <- data.frame(section = c("#1 Retail", "#2 Services", "#3 Restaurant", "#4 Foods"), 
                        Proportion = c(200, 175, 150, 50))
    
    
    
  })
  
  mall2 <- reactive({
    
    mall2 <- data.frame(section = c("#2 Retail", "#1 Services", "#4 Restaurant", "#3 Foods"), 
                        Proportion = c(275, 300, 50, 150))
    
    
    
  })
  
  b1 <- reactive({
    
    b1 <- data.frame(section = c("#1 - Zara", "#2 - H&M", "#3 - J.Crew", "#4 - Forever 21","#5 - Urban Outfitter"), 
                     Proportion = c(500, 300, 275, 150,50))
    
    
    
  })
  
  tags$div(id ="Rectangle",output$M1 <- renderPlot({ 
    
    ggplot(p(), aes(x = item, y = Proportion, fill = forcats::fct_rev(Segmentation),order = Proportion)) +
      geom_col() +
      geom_text(aes(label = paste(Proportion, "%")),
                position = position_stack(vjust = 0.5)) +coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
      theme(legend.position = "bottom",panel.grid = element_blank(), 
            axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),legend.title = element_blank())+
      scale_fill_manual(values= c("#ffd9c7","#f3ab89","#e27c4f","#cc4b14"))+guides(fill = guide_legend(reverse = TRUE))
    
    
    
  })  )
  
  output$M2 <- renderPlot({ 
    
    ggplot(a(), aes(x = item, y = Proportion, fill = forcats::fct_rev(Segmentation),order = Proportion)) +
      geom_col() +
      geom_text(aes(label = paste(Proportion, "%")),
                position = position_stack(vjust = 0.5)) +coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
      theme(legend.position = "bottom",panel.grid = element_blank(), 
            axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),legend.title = element_blank())+
      scale_fill_manual(values= c("#ffdbc2","#fdba89","#f59951","#e97809")) + guides(fill = guide_legend(reverse = TRUE))
    
  })  
  
  
  output$M3 <- renderPlot({ 
    
    ggplot(mall(), aes(x = reorder(section, Proportion), y = Proportion,fill =section)) +
      geom_bar(stat = "identity",width = 0.9) +coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
      theme(legend.position = "none", panel.grid = element_blank(), 
            axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +
      scale_fill_manual(values= c("#cc4b14","#e27c4f","#f3ab89", "#ffd9c7"))
  })  
  observeEvent(input$dropb,
               output$mytable <- DT::renderDataTable({a()}))
  
  output$Mall1 <- renderPlot({ 
    
    ggplot(mall1(), aes(x = reorder(section, Proportion), y = Proportion,fill =section)) +
      geom_bar(stat = "identity",width = 0.9) +coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
      theme(legend.position = "none", panel.grid = element_blank(), 
            axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +
      scale_fill_manual(values= c("#ab1818","#cc4b14","#e97809", "#ffa600")) +ylim(0,300)
  })  
  
  output$Mall2<- renderPlot({ 
    
    ggplot(mall2(), aes(x = reorder(section, Proportion), y = Proportion,fill =section)) +
      geom_bar(stat = "identity",width = 0.9) +coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
      theme(legend.position = "none", panel.grid = element_blank(), 
            axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +
      scale_fill_manual(values= c("#ab1818","#cc4b14","#e97809", "#ffa600"))+ylim(0,300)
  })  
  
  output$m1_1 <- renderText({
    "East York Mall"
  })
  
  output$m1_2 <- renderText({
    paste("385 Noah Place Suit 878","Toronto, ON M6K 3P0",sep = "\n")
  })
  
  output$m2_1 <- renderText({
    "Cloverdale Mall"
  })
  
  output$m2_2 <- renderText({
    paste("250 The East Mall","Etobicoke, ON M9B 3Y80",sep = "\n")
  })
  
  
  
  output$b1<- renderPlot({ 
    
    ggplot(b1(), aes(x = reorder(section, Proportion), y = Proportion,fill =section)) +
      geom_bar(stat = "identity",width = 0.9) +coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
      theme(legend.position = "none", panel.grid = element_blank(), 
            axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +
      scale_fill_manual(values= c(alpha(rgb(146/255,120/255,194/255),0.48),alpha(rgb(191/255,114/255,189/255),0.48),alpha(rgb(232/255,107/255,171/255),0.48),
                                  alpha(rgb(255/255,105/255,142/255),0.48),alpha(rgb(255/255,115/255,106/255),0.48)))+ylim(0,500)
  })  
}