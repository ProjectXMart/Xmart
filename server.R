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
}