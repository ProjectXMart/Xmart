server = function(input, output,session) {
  output$map1 <-renderGoogle_map({
    map_key <- "AIzaSyD6AfICWEZMcI40_Alta3JTLlHD-im2LRk"
    set_key(key =map_key)
    google_map(key = map_key)

  })
 

  observeEvent(input$button, {
    
    
    register_google(key="AIzaSyD6AfICWEZMcI40_Alta3JTLlHD-im2LRk")

    if (input$inputId == "Single") {

      jio<-geocode(input$Place,output ="latlona", source ="google")

      lati<-jio$lat
      longi<-jio$lon
      x = merge(jio,input$Radius*1000)
      x$ico <-"https://img.icons8.com/dusk/64/000000/building.png"

      a<-c(longi,lati)

      fsacentroids <- read.csv("C:/Users/ali518/Desktop/X-Mart/FSACentroids.csv",stringsAsFactors = FALSE)
      for (i in 1:nrow(fsacentroids)){

        b<-c(fsacentroids$Lng[i],fsacentroids$Lat[i])
        fsacentroids$geodis[i]<-distm(a,b, fun = distHaversine)

      }

      fsacentroids$geodiskm<-fsacentroids$geodis/1000

      tafsa<-subset(fsacentroids,geodiskm<=input$Radius)

      pccentroids<-read.csv("C:/Users/ali518/Desktop/X-Mart/Lat Long Canada.csv",stringsAsFactors = FALSE)

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
      #write.csv(tradearea,"tradearea_new.csv")

      tafsa1 <-merge(tafsa,x)
      output$map <-renderGoogle_map({
        map_key <- "AIzaSyD6AfICWEZMcI40_Alta3JTLlHD-im2LRk"
        set_key(key =map_key)
        google_map(data = tafsa1, key = map_key) %>%
          add_markers(lat = "Lat", lon = "Lng", mouse_over = "FSA" ) %>%
          add_circles(lon ="lon",lat="lat",radius="y",fill_colour = "#ffd9c7",info_window = "address",fill_opacity =0.05)%>%
          add_markers(lon="lon",lat="lat",mouse_over="address",marker_icon="ico")


      })
    
    
      
    }
    # if (input$inputId == "Multiple") {
    #   ############################Multiple Option###################################################
    #   
    #   ####Reading the input file from user into another dataframe
    #   
    #   
    #   proplist<-read.csv("C:/Users/ali518/Desktop/X-Mart/testmall.csv",stringsAsFactors =FALSE)
    #   
    #   
    #   for(i in 1:nrow(proplist)){
    #     
    #     jio<-geocode(proplist$Address[i],output ="latlona", source ="google")
    #     
    #     proplist$lati[i]<-jio$lat
    #     proplist$longi[i]<-jio$lon
    #     
    #   }
    #   
    #   fsacentroids <- read.csv("C:/Users/ali518/Desktop/X-Mart/FSACentroids.csv",stringsAsFactors = FALSE)
    #   pccentroids<-read.csv("C:/Users/ali518/Desktop/X-Mart/Lat Long Canada.csv",stringsAsFactors = FALSE)
    #   
    #   for(i in 1:1)
    #   {
    #     a<-c(proplist$longi[i],proplist$lati[i])
    #     
    #     for (i in 1:nrow(fsacentroids)){
    #       
    #       b<-c(fsacentroids$Lng[i],fsacentroids$Lat[i])
    #       fsacentroids$geodis[i]<-distm(a,b, fun = distHaversine)
    #     }
    #     
    #     fsacentroids$geodiskm<-fsacentroids$geodis/1000
    #     
    #     fsacentroids$Address<-proplist$Address[i]
    #     
    #     tafsa<-subset(fsacentroids,geodiskm<=4000)
    #     
    #     tapccentroids<-merge(tafsa,pccentroids,by="FSA")
    #     
    #     for (i in 1:nrow(tapccentroids)){
    #       
    #       b<-c(as.numeric(tapccentroids$Lng.x[i]),as.numeric(tapccentroids$Lat.x[i]))
    #       tapccentroids$geodis[i]<-distm(a,b, fun = distHaversine)
    #       
    #     }
    #     
    #     tapccentroids$geodiskm<-tapccentroids$geodis/1000
    #     
    #     tradeareapc<-subset(tapccentroids,geodiskm<=4000)
    #     
    #   }
    #   
    #   tradearea<-tradeareapc
    #   
    #   for(i in 2:nrow(proplist))
    #   {
    #     a<-c(proplist$longi[i],proplist$lati[i])
    #     
    #     for (i in 1:nrow(fsacentroids)){
    #       
    #       b<-c(fsacentroids$Lng[i],fsacentroids$Lat[i])
    #       fsacentroids$geodis[i]<-distm(a,b, fun = distHaversine)
    #     }
    #     
    #     fsacentroids$geodiskm<-fsacentroids$geodis/1000
    #     
    #     fsacentroids$Address<-proplist$Address[i]
    #     
    #     tafsa<-subset(fsacentroids,geodiskm<=input$Radius)
    #     
    #     tapccentroids<-merge(tafsa,pccentroids,by="FSA")
    #     
    #     for (i in 1:nrow(tapccentroids)){
    #       
    #       b<-c(as.numeric(tapccentroids$Lng.x[i]),as.numeric(tapccentroids$Lat.x[i]))
    #       tapccentroids$geodis[i]<-distm(a,b, fun = distHaversine)
    #       
    #     }
    #     
    #     tapccentroids$geodiskm<-tapccentroids$geodis/1000
    #     
    #     tradeareapc<-subset(tapccentroids,geodiskm<=input$Radius)
    #     
    #     tradearea<-rbind(tradeareapc,tradeareapc)
    #   }
    #   
    #   write.csv(tradearea,"tradearea_new.csv")
    #   
    #   output$map <-renderGoogle_map({
    #     map_key <- "AIzaSyD6AfICWEZMcI40_Alta3JTLlHD-im2LRk"
    #     set_key(key =map_key)
    #     google_map(data = tradearea, key = map_key) %>%
    #       add_markers(lat = "Lat.x", lon = "Lng.y", info_window = "FSA")
    #     
    #   })
    # }
    
    
    #tradearea = read.csv("C:/Users/ali518/Desktop/X-Mart/Trial1/tradearea_new.csv")
    
    #tradearea = tradearea[,-1]
    
    malls<-readRDS(file="C:/Users/ali518/Desktop/X-Mart/bestmalls.rds")
    pceuc<-readRDS(file="C:/Users/ali518/Desktop/X-Mart/pceuclidean.rds")
    
    names(malls)
    
    trade_euc<-merge(tradearea,pceuc)
    names(trade_euc)
    train<-trade_euc[c(12:41)]
    train[is.na(train)] <- 0
    
    train$Tot<-train$A+train$B+train$C+train$D+train$E+train$F+train$G+train$H+train$I+train$J+train$K+train$L+train$M+train$N+train$O+train$P+train$Q+train$R
    
    trainsum<-train%>%group_by(Province)%>%summarise(Books=sum(Books),Clothing=sum(Clothing),Furniture=sum(Furniture),Home_Improvement=sum(Home_Improvement),Jewellery=sum(Jewellery),Optical=sum(Optical),Pet=sum(Pet),Restaurants=sum(Restaurants),Shoes=sum(Shoes),Sports=sum(Sports),Toys=sum(Toys),A=sum(A),B=sum(B),C=sum(C),D=sum(D),E=sum(E),Fi=sum(F),G=sum(G),H=sum(H),I=sum(I),J=sum(J),K=sum(K),L=sum(L),M=sum(M),N=sum(N),O=sum(O),P=sum(P),Q=sum(Q),R=sum(R))
  
    names(train)
  
   a<-as.numeric(trainsum[c(2:20,24:29)])
  
  for(i in 1:nrow(malls)){
    
    b<-as.numeric(malls[i,2:26])
    malls$distance<-dist(rbind(a,b))
    
  }
  
  
  allocation = t(malls[1,27:39])
  allocation= cbind(rownames(allocation),allocation)
  colnames(allocation) = c("Segmentation","n")
  allocation = as.data.frame(allocation)
  allocation <- allocation %>% 
    mutate(item = "allocation")
  allocation$n = as.character(allocation$n)
  allocation$n = as.numeric(allocation$n)*100
  allocation$n = round(allocation$n,0)
  allocation <- allocation[order(-allocation$n),]
    
    
    
    
    
    
    
   tradearea = tradearea$PostCode
    
    
  
    p = readRDS("C:/Users/ali518/Desktop/X-Mart/persona.rds")
    persona = p %>% filter(PostCode %in% tradearea)
    persona = count(persona, Segmentation)
    persona$n = round(persona$n*100/sum(persona$n),0)
    persona = persona[persona$n != 0, ]
    persona <- persona %>%
      mutate(item = "Persona")
    
    
    p_table = p[,c(1,3,12,6,2,ncol(p))]
    p_table[,2:5] = round(p_table[,2:5]*100,2)
    colnames(p_table)[2] = "Percentage of management Occupations"
    colnames(p_table)[3] = "Percentage of university certificate or diploma below bachelor level"
    colnames(p_table)[4] = "Percentage of occupations in art, culture, recreation and sport"
    colnames(p_table)[5] = "Percentage of number of familities with 5 or more persons"
    p_table = p_table %>% filter(PostCode %in% tradearea)
    p_table = p_table %>% filter(Segmentation %in% persona$Segmentation)
    p_table = p_table %>% group_by(Segmentation) %>% summarise_if(is.numeric,mean)
    p_table = t(p_table)
    p_table = p_table[-2,]
    
    
    
    
    a = readRDS("C:/Users/ali518/Desktop/X-Mart/affluence.rds")
    affluence = a %>% filter(PostCode %in% tradearea)
    affluence = count(affluence, Segmentation)
    affluence$n = round(affluence$n*100/sum(affluence$n),0)
    affluence = affluence[affluence$n != 0, ]
    affluence <- affluence %>% 
      mutate(item = "Affluence")
    
    affluence <- affluence[order(affluence$n),]
    
    a_table = a[,c(1,4,2,13,17,10,3,ncol(a))]
    colnames(a_table)[2] = "Average Household Income"
    colnames(a_table)[3] = "Total Expenditure"
    colnames(a_table)[4] = "Mortgage: Other real estate in Canada & foreign"
    colnames(a_table)[5] = "Credit Card and Installment debt"
    colnames(a_table)[6] = "Percentage House Hold with income range $100,000-$124,999"
    colnames(a_table)[7] = "Average Value of Dewelling"
    a_table = a_table %>% filter(PostCode %in% tradearea)
    a_table = a_table %>% filter(Segmentation %in% affluence$Segmentation)
    a_table = a_table %>% group_by(Segmentation) %>% summarise_if(is.numeric,mean)
    a_table = t(a_table)
    a_table = a_table[-2,]
    
    
    
    
    b = readRDS("C:/Users/ali518/Desktop/X-Mart/behaviour.rds")
    behaviour = b %>% filter(PostCode %in% tradearea)
    behaviour = count(behaviour, Segmentation)
    behaviour$n = round(behaviour$n*100/sum(behaviour$n),0)
    behaviour = behaviour[behaviour$n != 0, ]
    behaviour <- behaviour %>% 
      mutate(item = "Behaviour")
    
    behaviour <- behaviour[order(behaviour$n),]
    
    
    b_table = b
    b_table[,2:6] = round(b_table[,2:6],2)
    colnames(b_table)[2] = "Attitude about Advertising: 'New and improved' on packages is just an advertising gimmick"
    colnames(b_table)[3] = "Health Consciousness: I would like to eat healthy foods more often"
    colnames(b_table)[4] = "Opinion about New Products: I have tried a product/service based on a personal recommendation"
    colnames(b_table)[5] = "Brand Loyalty: I value companies who give back to the community"
    colnames(b_table)[6] = "Cost Sensitivity:I prefer to postpone a purchase than buy on credit"
    b_table = b_table %>% filter(PostCode %in% tradearea)
    b_table = b_table %>% filter(Segmentation %in% affluence$Segmentation)
    b_table = b_table %>% group_by(Segmentation) %>% summarise_if(is.numeric,mean)
    b_table = t(b_table)
    b_table = b_table[-2,]
    
    
    
    
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
    
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Book.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Books = b1
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Cloth.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Clothing = b1
    
    # t1 = substr(tradearea,1,3)
    # b1 = read.csv("C:/Users/ali518/Desktop/X-Mart/Trial1/IndexClothing.csv")
    # b1 = b1 %>% filter(FSA %in% t1)
    # b1 = b1 %>% group_by(Name) %>% summarise(V1 = weighted.mean(Value,Sum_PP15_))
    # colnames(b1)[1] = c("Brand")
    # Clothing = b1
    
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/HI.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    HomeImprovement = b1
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Jewel.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Jewellery = b1
    
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Optic.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Optical = b1
    
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Pet.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Pet = b1
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/REST.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Restaurants = b1
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Shoe.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Shoes = b1
    
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Sport.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Sports = b1
    
    b1 = readRDS("C:/Users/ali518/Desktop/X-Mart/Trial1/Toy.rds")
    b1 = b1 %>% filter(PostCode %in% tradearea)
    b1 = apply(b1[,3:ncol(b1)], 2, mean)
    b1 = as.matrix(b1)
    b1 = as.data.frame(b1)
    b1 <- b1 %>%
      mutate(Brand = rownames(b1))
    b1 = b1[order(-b1$V1),]
    Toys = b1
    
    
    
    
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
    
    
    #Space for Euclidean distance
    output$Allocation <- renderUI({
      bsModal("sq_all", "Recommended Space Allocation", trigger = "", size = "large",
      renderPlot(ggplot(allocation, aes(x = item, y = n, fill = forcats::fct_rev(Segmentation),order = n)) +
      geom_col() + scale_fill_brewer(palette = "Set3")+
      geom_text(aes(label = paste(n, "%")),
      position = position_stack(vjust = 0.5)) + coord_flip() +
     theme_minimal(base_size = 16) +
      ylab(NULL) +
      xlab(NULL) + 
     theme(legend.position = "bottom",panel.grid = element_blank(), 
     axis.title = element_blank(),axis.ticks.x = element_blank(),axis.text=element_blank(),legend.title = element_blank())+
     guides(fill = guide_legend(reverse = TRUE)),height = 300))
     
    })
    
    
    output$table_persona <- renderTable({
      p_table
    }, rownames = TRUE, colnames = FALSE)
    
    
    output$table_affluence <- renderTable({
      a_table
    }, rownames = TRUE, colnames = FALSE)
    
    
    output$table_behaviour <- renderTable({
      b_table
    }, rownames = TRUE, colnames = FALSE)
    
    
    observeEvent(input$seg, {
      toggleModal(session, "modalExample", "open")
    })
    
    observeEvent(input$p, {
      toggle("table_persona")
    })
    
    observeEvent(input$a, {
      toggle("table_affluence")
    })
    
    observeEvent(input$b, {
      toggle("table_behaviour")
    })
    
    
    
    
    observeEvent(input$alloc, {
      toggleModal(session,"sq_all","open")
    })
    
    
    output$m1_1 <- renderText({
      "Property"
    })
    
    output$m1_2 <- renderUI({
      HTML(input$Place)
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
    
    
    output$Books<- renderPlot({ 
      
      ggplot(Books, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Books$V1))
      
    })  
    
    
    output$HomeImprovement<- renderPlot({ 
      
      ggplot(HomeImprovement, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(HomeImprovement$V1))
      
    })  
    
    output$Jewellery<- renderPlot({ 
      
      ggplot(Jewellery, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Jewellery$V1))
      
    })  
    
    output$Optical<- renderPlot({ 
      
      ggplot(Optical, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Optical$V1))
      
    })  
    
    output$Pet<- renderPlot({ 
      
      ggplot(Pet, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Pet$V1))
      
    })  
    
    output$Restaurants<- renderPlot({ 
      
      ggplot(Restaurants, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Restaurants$V1))
      
    })  
    
    
    output$Shoes<- renderPlot({ 
      
      ggplot(Shoes, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Shoes$V1))
      
    })  
    
    
    output$Sports<- renderPlot({ 
      
      ggplot(Sports, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Sports$V1))
      
    })  
    
    output$Toys<- renderPlot({ 
      
      ggplot(Toys, aes(x = reorder(Brand,V1), y = V1,fill =Brand)) +
        geom_bar(stat = "identity",width = 0.9) +coord_flip()+
        scale_fill_manual(values = alpha(brand_color,alpha = 0.48)) +
        theme_minimal(base_size = 16) +
        ylab(NULL) +
        xlab(NULL) + 
        theme(legend.position = "none", panel.grid = element_blank(), 
              axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust = 0)) +ylim(0,max(Toys$V1))
      
    })  
    
    
    show("panel")
    
    
    
  })
  
  
  
  

  
     
}