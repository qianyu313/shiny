devtools::install_github('cbw1243/usdampr')




test1a <- mpr_request(slugIDs_legacy ='LM_CT152', report_time = '06/26/2020')
str(test1a)
data(slugInfo)
View(slugInfo)






setwd("/Users/qianyudong/Documents/R/shiny/shiny05")

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(reshape2)
library(scales)
library(stringr)
library(maps)
library(usmap)
library(ggpmisc)
library(gridExtra)
library(shinycssloaders)
library(mapproj)
library(grid)
library(sf)
library(tidyverse)
library(urbnmapr)
library(shinyWidgets)
#library(numform)


tweaks <-
  list({tags$head(tags$style(HTML("
                                  .multicol {
                                  # height: 150px;
                                  # -webkit-column-count: 3; /* Chrome, Safari, Opera */
                                  # -moz-column-count: 3;  /* Firefox */
                                  column-count: 2;
                                  # -moz-column-fill: auto;
                                  # -column-fill: auto;
                                  }
                                  "))
  )})


#load data 

spacial<-readRDS("/Users/qianyudong/Documents/R/shiny/shiny04/SpacialInventory17.rds")
data<-readRDS("/Users/qianyudong/Documents/R/shiny/shiny04/SpacialInventory17.rds")
data1<-readRDS("/Users/qianyudong/Documents/R/shiny/shiny04/RegionalInventory17.rds")


shinyServer(
  
  function(input, output,session) {
    
    #############################
    ##Map at state level ########
    ############################# 
    output$State_ui <- renderUI({
      # if (is.null(input$State_Year_Type))
      #   return()
      switch(
        input$State_Year_Type,
        "Single year"=selectInput(inputId = "state_year",
                                  label = "Year",
                                  choices = seq(min(MapData1()$year),max(MapData1()$year)),
                                  selected =  max(MapData1()$year)),
        
        "Nominal change between two years"=sliderInput(inputId="state_year_range",label= "Nominal change between two years",
                                                       min = min(MapData1()$year),
                                                       max = max(MapData1()$year),
                                                       value = c(2009,2019),sep=""),
        
        "Percent change between two years" =sliderInput(inputId="state_year_percent",label= "Percent change between two years", 
                                                        min = min(MapData1()$year),
                                                        max = max(MapData1()$year),
                                                        value = c(2009,2019),sep="")
        
        
      )
      
      
    })
    
    #MapData3()[1,]$category%in%cate
    
    MapData1<-reactive({dplyr::filter(spacial,spacial$category%in%input$State_category)})
    MapData2_1 <-reactive({dplyr::filter(MapData1(),year%in%input$state_year_range[2])})
    MapData2_2 <-reactive({dplyr::filter(MapData1(),year%in%input$state_year_range[1])})
    MapData2_3 <- reactive({left_join(MapData2_1(), MapData2_2(),by = c("state" = "state", "category" = "category","state_fips"="state_fips"))})
    value2<-reactive({round(MapData2_3()$value.x-MapData2_3()$value.y,digits = 4)})
    
    MapData3_1 <-reactive({dplyr::filter(MapData1(),year%in%input$state_year_percent[2])})
    MapData3_2 <-reactive({dplyr::filter(MapData1(),year%in%input$state_year_percent[1])})
    MapData3_3 <- reactive({left_join(MapData3_1(), MapData3_2(),by = c("state" = "state",  "category" = "category","state_fips"="state_fips"))})
    value3<-reactive({round((((MapData3_3()$value.x-MapData3_3()$value.y)/MapData3_3()$value.y))*100,digits = 2)})
    
    Type<-reactive({input$State_Year_Type})
    MapData2<-  reactive({
      if (Type()==c("Single year"))
      {
        
        MapData2=filter(MapData1(),year%in%input$state_year)
        
      }
      
      else if(Type()==c("Nominal change between two years"))
      {
        
        MapData2=as.data.frame(cbind(state=MapData2_3()$state,year=paste(MapData2_3()$year.y, MapData2_3()$year.x,sep=","),
                                     state_fips= MapData2_3()$state_fips,
                                     value=value2(),
                                     category=MapData2_3()$category),stringsAsFactors=FALSE)
        
        
      }
      else
      {
        
        MapData2=as.data.frame(cbind(state=MapData3_3()$state,year=paste(MapData3_3()$year.y, MapData3_3()$year.x,sep=",") ,
                                     state_fips= MapData3_3()$state_fips
                                     ,value=as.numeric(value3()),
                                     category=MapData3_3()$category),stringsAsFactors=FALSE)
        
        
      }
      
    })
    
    ##Prepare map
    
    spatial_data <-reactive({left_join(get_urbn_map(map = "states", sf = TRUE),
                                       MapData2(),
                                       by=c("state_fips"="state_fips"))})
    
    ###centroids 
    centroids<-reactive({as.data.frame(cbind(state_fips=spatial_data()$state_fips,state=spatial_data()$state_abbv,value=spatial_data()$value,
                                             st_coordinates(st_centroid(spatial_data()$geometry))),stringsAsFactors=FALSE )})
    
    centroids2<-reactive({as.data.frame(cbind(value=spatial_data()$value,
                                              st_coordinates(st_centroid(spatial_data()$geometry)),state_fips=spatial_data()$state_fips),stringsAsFactors=FALSE)})
    centroids_text <-reactive({centroids()[centroids()$state%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),1:3]})
    centroids_ot<-reactive({ centroids2()[!centroids2()$state_fips%in% c(centroids_text()$state_fips),1:3]})
    
    output$State_Plot<-renderPlot({
      
      if (Type()==c("Single year")&&input$State_category%in%c("CALF CROP","FEEDER SUPPLY"))
      {
        Northeastern <-reactive({centroids()[centroids()[,2]%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),2:3] %>% drop_na()})
        Northeastern_Table <- reactive({as.data.frame(cbind(as.numeric(Northeastern()$value)/1000,c("")),row.names = Northeastern()$state)})
        color<-reactive({geom_sf(spatial_data(), mapping= aes(fill = value/1000,geometry = geometry),color = "#ffffff", size = 0.25)})
        color2<-reactive({scale_fill_gradient(low = "grey90", high ="royalblue3",na.value = "grey80")}) 
        title<-reactive({ ggtitle(paste(input$State_category ,"(1,000 Head)",sep = " "),subtitle = paste("January",MapData2()[1,2],sep = " "))})
        numbers_on_map<-reactive({with(centroids_ot(),
                                       annotate(geom="text", x=as.numeric(X), y= as.numeric(Y), label= as.numeric(value)/1000,na.rm = T,
                                                size =3,color="black"))})
        legend_name<-reactive({labs(fill = "Number")})
        
        t<- reactive({round(sum(as.numeric(MapData2()$value/1000),na.rm=T))})
        b<-reactive({paste("U.S. Total", t())})
        
        
      }
      else if (Type()==c("Single year")&&input$State_category%in%c("STOCKER INDEX","HEIFER RETENTION"))
      {
        
        Northeastern <-reactive({centroids()[centroids()[,2]%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),2:3] %>% drop_na()})
        Northeastern_Table <- reactive({as.data.frame(cbind(Northeastern()$value,c("")),row.names = Northeastern()$state)})
        color<-reactive({geom_sf(spatial_data(), mapping= aes(fill =  as.numeric(value),geometry = geometry), size = 0.25)})
        color2<-reactive({ scale_fill_gradient2(low = "red4", high ="royalblue3" ,mid="grey90", na.value = "grey80")})
        title<-reactive({ ggtitle(paste(input$State_category,sep = " "),subtitle = paste("January",MapData2()[1,2],sep = " "))})
        numbers_on_map<-reactive({with(centroids_ot(),
                                       annotate(geom="text", x=as.numeric(X), y= as.numeric(Y), label=as.numeric(value),na.rm = T,
                                                size =3,color="black"))})
        legend_name<-reactive({labs(fill = "Value")})
        
        t<- reactive({round(mean(as.numeric(MapData2()$value),na.rm=T),digits = 2)})
        b<-reactive({paste("U.S. Average", t())})
        
        
      }
      else if (Type()==c("Nominal change between two years")&&input$State_category%in%c("CALF CROP","FEEDER SUPPLY"))
      {
        Northeastern <-reactive({centroids()[centroids()[,2]%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),2:3] %>% drop_na()})
        Northeastern_Table <- reactive({as.data.frame(cbind(as.numeric(Northeastern()$value)/1000,c("")),row.names = Northeastern()$state)})
        color<-reactive({geom_sf(spatial_data(), mapping= aes(fill =  as.numeric(value)/1000,geometry = geometry), size = 0.25)})
        color2<-reactive({scale_fill_gradient2(low = "red4", high ="royalblue3" ,mid="grey90",midpoint = 0, na.value = "grey80")})
        title<-reactive({ ggtitle(paste("CHANGE IN",input$State_category,"(1,000 Head)",sep = " "),subtitle = paste("January",MapData2()[1,2],sep = " "))})
        numbers_on_map<-reactive({with(centroids_ot(),
                                       annotate(geom="text", x=as.numeric(X), y= as.numeric(Y), label=as.numeric(value)/1000,na.rm = T,
                                                size =3,color="black"))})
        legend_name<-reactive({labs(fill = "Nominal change")})
        
        t<-reactive({round(sum(as.numeric(MapData2()$value)/1000,na.rm=T))})
        b<-reactive({paste("U.S. Total", t())})
        
        
        
      }
      else if (Type()==c("Nominal change between two years")&&input$State_category%in%c("STOCKER INDEX","HEIFER RETENTION"))
      {
        Northeastern <-reactive({centroids()[centroids()[,2]%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),2:3] %>% drop_na()})
        Northeastern_Table <- reactive({as.data.frame(cbind(Northeastern()$value,c("")),row.names = Northeastern()$state)})
        color<-reactive({geom_sf(spatial_data(), mapping= aes(fill =  as.numeric(value),geometry = geometry), size = 0.25)})
        color2<-reactive({scale_fill_gradient2(low = "red4", high ="royalblue3" ,mid="grey90",midpoint = 0, na.value = "grey80")})
        title<-reactive({ ggtitle(paste("CHANGE IN",input$State_category),subtitle = paste("January",MapData2()[1,2],sep = " "))})
        numbers_on_map<-reactive({with(centroids_ot(),
                                       annotate(geom="text", x=as.numeric(X), y= as.numeric(Y), label=as.numeric(value),na.rm = T,
                                                size =3,color="black"))})
        legend_name<-reactive({labs(fill = "Nominal change")})
        
        t<- reactive({round(mean(as.numeric(MapData2()$value),na.rm=T),digits = 2)})
        b<-reactive({paste("U.S. Average", t())})
        
        
        
        
        
      }
      else if (Type()==c("Percent change between two years")&&input$State_category%in%c("CALF CROP","FEEDER SUPPLY"))
      {
        Northeastern <-reactive({centroids()[centroids()[,2]%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),2:3] %>% drop_na()})
        Northeastern_Table <- reactive({as.data.frame(cbind(Northeastern()$value,c("")),row.names = Northeastern()$state)})
        color<-reactive({geom_sf(spatial_data(), mapping= aes(fill =  as.numeric(value),geometry = geometry), size = 0.25)})
        color2<-reactive({scale_fill_gradient2(low = "red4", high ="royalblue3" ,mid="grey90",midpoint = 0, na.value = "grey80")})
        title<-reactive({ ggtitle(paste("PERCENT CHANGE IN",input$State_category,sep = " "),subtitle = paste("January",MapData2()[1,2],sep = " "))})
        numbers_on_map<-reactive({with(centroids_ot(),
                                       annotate(geom="text", x=as.numeric(X), y= as.numeric(Y), label=round(as.numeric(value),digits = 2),na.rm = T,
                                                size =3,color="black"))})
        legend_name<-reactive({labs(fill = "Percent change")})
        
        t<-reactive({round((((sum(as.numeric(MapData3_3()$value.x),na.rm=T)-sum(as.numeric(MapData3_3()$value.y),na.rm=T))/sum(as.numeric(MapData3_3()$value.y),na.rm=T)))*100, digits = 2)})
        b<-reactive({paste("U.S. Total", t())})
      }
      else
      {
        Northeastern <-reactive({centroids()[centroids()[,2]%in%c("CT","DE","MD","MA","NH","NJ","RI","VT"),2:3] %>% drop_na()})
        Northeastern_Table <- reactive({as.data.frame(cbind(Northeastern()$value,c("")),row.names = Northeastern()$state)})
        color<-reactive({geom_sf(spatial_data(), mapping= aes(fill =  as.numeric(value),geometry = geometry), size = 0.25)})
        color2<-reactive({scale_fill_gradient2(low = "red4", high ="royalblue3" ,mid="grey90",midpoint = 0, na.value = "grey80")})
        title<-reactive({ ggtitle(paste("PERCENT CHANGE IN",input$State_category,sep = " "),subtitle = paste("January",MapData2()[1,2],sep = " "))})
        numbers_on_map<-reactive({with(centroids_ot(),
                                       annotate(geom="text", x=as.numeric(X), y= as.numeric(Y), label=round(as.numeric(value),digits = 2),na.rm = T,
                                                size =3,color="black"))})
        legend_name<-reactive({labs(fill = "Percent change")})
        
        t<-reactive({round((((mean(as.numeric(MapData3_3()$value.x),na.rm=T)-mean(as.numeric(MapData3_3()$value.y),na.rm=T))/mean(as.numeric(MapData3_3()$value.y),na.rm=T)))*100, digits = 2)})
        b<-reactive({paste("U.S. Average", t())})
        
      }
      
      
      ggplot() +
        color()+
        color2()+
        title()+
        numbers_on_map()+
        legend_name()+
        # scale_fill_gradient2(low = "red4", high ="royalblue3" ,mid="grey90",midpoint = 0, na.value = "grey80")+
        coord_sf(datum = NA)+
        annotation_custom(tableGrob(Northeastern_Table(), cols = NULL,
                                    theme=ttheme_minimal(base_size = 10,padding = unit(c(2, 2), "mm"))),
                          xmin=2000000, xmax=4000000, ymin=150000, ymax=400000)+
        geom_text(aes(x=1000000, y=-2000000, label=b()))+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y= element_blank(),
              axis.text.x= element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              legend.position = c(1.09,0.35),
              plot.title = element_text(hjust=0.5),
              plot.subtitle = element_text(hjust=0.5),
              plot.margin = unit(c(1.2, 3, 1.2, 0.2),"cm")
        )
      
      
    })
    
    output$State_Data<-renderTable({
      StateMapData<-MapData2()
      names(StateMapData)<-c("state","year","fips","value","category")
      StateMapData$year<-as.character(StateMapData$year)
      StateMapData
    })
    
    output$download_State_Plot = downloadHandler(paste(input$State_category,"AT STATE LEVEL",".png", sep=" "), content =function(file)    { 
      ggsave(file,plot = last_plot(), scale = 1,
             units = c("in"),
             dpi = 300)})
    
    output$download_State_Data= downloadHandler(paste(input$State_category,"AT STATE LEVEL",".csv", sep=" "), content =function(file) {
      StateMapData<-MapData2()
      names(StateMapData)<-c("state","year","value","fips","category")
      StateMapData$year<-as.character(StateMapData$year)
      StateMapData
      write.table(StateMapData, file  ,sep=",",row.names =FALSE,quote = FALSE) })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###############
    ## Bar Chart###
    ############### 
    output$Bar_ui <- renderUI({
      
      # data control: the checkboxes will control the data values plotted
      controls <-{
        list(
          #h4("States"),
          tags$div(
            # align = 'left', 
            class = 'multicol',
            checkboxGroupInput(inputId  = "b_location", 
                               label= "States",
                               choices = c("Alabama"="01", "Alaska"="02", "Arizona"="04", "Arkansas"="05",
                                           "California"="06", "Colorado"="08", "Connecticut"="09\\", 
                                           "Delaware"="10", "Florida"="12","Georgia"="13",
                                           "Hawaii"="15", "Idaho"="16", "Illinois"="17", "Indiana"="18",
                                           "Iowa"="19","Kansas"="20", "Kentucky"="21", "Louisiana"="22",
                                           "Maine"="23", "Maryland"="24", "Massachusetts"="25",
                                           "Michigan"="26", "Minnesota"="27", "Mississippi"="28", 
                                           "Missouri"="29", "Montana"="30", "Nebraska"="31", "Nevada"="32",
                                           "New Hampshire"="33", "New Jersey"="34", "New Mexico"="35",
                                           "New York"="36", "North Carolina"="37", "North Dakota"="38",
                                           "Ohio"="39", "Oklahoma"="40","Oregon"="41", "Pennsylvania"="42",
                                           "Rhode Island"="44", "South Carolina"="45", "South Dakota"="46",
                                           "Tennessee"="47", "Texas"="48", "Utah"="49", "Vermont"="50", 
                                           "Virginia"="51", "Washington"="53", "West Virginia"="54",
                                           "Wisconsin"="55", "Wyoming"="56"),
                               selected = c("31","40","19","48","20","46"),
                               inline   = F))) }
      
      # if (is.null(input$Bar_sub_category))
      #   return()
      ##switch
      switch(
        input$Bar_sub_category,
        
        "Region"= checkboxGroupInput(inputId="b_location", 
                                     label= "Regions",
                                     choices = c("Northwest" ="Northwest","Southwest" ="Southwest","North.Rockies"="North.Rockies",
                                                 "South.Rockies"="South.Rockies","North.Plains"="North.Plains","South.Plains"="South.Plains",
                                                 "Great.Lakes"="Great.Lakes","Midwest"="Midwest","Gulf"="Gulf","South"="South",
                                                 "Appalachain"="Appalachain","East.seaboard"="East.seaboard","Northeast"="Northeast"),
                                     selected = c("Northwest" ="Northwest","Southwest" ="Southwest","North.Rockies"="North.Rockies",
                                                  "South.Rockies"="South.Rockies","North.Plains"="North.Plains","South.Plains"="South.Plains",
                                                  "Great.Lakes"="Great.Lakes","Midwest"="Midwest","Gulf"="Gulf","South"="South",
                                                  "Appalachain"="Appalachain","East.seaboard"="East.seaboard","Northeast"="Northeast")
        ),   
        "State" = fluidRow( controls,width = 4,tweaks) 
      )
      
    })
    
    ###Maximum number of selections 
    observe({
      if(length(input$b_location) > 13){
        updateCheckboxGroupInput(session, "b_location", selected= head(input$b_location,13))
      }
      
    })
    
    
    output$Bar_Plot<-renderPlot({
      BarData<-reactive({dplyr::filter(data,data$category%in%input$Bar_category)})
      BarData1<-reactive({dplyr::filter(data1,data1$category%in%input$Bar_category)})
      choice1<-reactive({input$Bar_sub_category})
      BarData2 <-reactive({
        if (choice1()==c("State")) {
          
          dplyr::filter(BarData(),BarData()$state_fips%in%input$b_location)
        }
        
        else {
          dplyr::filter(BarData1(),BarData1()$region%in%input$b_location)%>%dplyr::rename(.,state=region)
          
        }
        
      })
      BarData3 <-reactive({dplyr::filter(BarData2(),BarData2()$year%in%input$Bar_year)%>%drop_na()})
      
      if (input$Bar_category%in%c("STOCKER INDEX","HEIFER RETENTION"))
      {floor<-floor(min(req(BarData3()$value)))
      ceiling<-ceiling(max(req(BarData3()$value))) }
      else
      {floor<-floor(min(req(BarData3()$value))/100)*100
      ceiling<-ceiling(max(req(BarData3()$value))/100)*100}
      
      ggplot(BarData3(), aes(x=factor(BarData3()$state), y=value,na.rm = TRUE))+
        geom_bar(stat="identity", width=0.5, fill="steelblue",na.rm = TRUE)+
        ggtitle(paste(input$Bar_category,sep = " "),
                subtitle = paste("In",input$Bar_year,sep = " "))+
        scale_y_continuous( breaks =seq( from=floor,
                                         to=ceiling,
                                         by=(ceiling-floor)/5 ))+
        labs(y= "Value",x= "Location")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
      
    })
    
    output$Bar_Data<-renderTable({
      
      BarData<-reactive({dplyr::filter(data,data$category%in%input$Bar_category)})
      BarData1<-reactive({dplyr::filter(data1,data1$category%in%input$Bar_category)})
      choice1<-reactive({input$Bar_sub_category})
      BarData2 <-reactive({
        if (choice1()==c("State")) {
          
          dplyr::filter(BarData(),BarData()$state_fips%in%input$b_location)
        }
        
        else {
          dplyr::filter(BarData1(),BarData1()$region%in%input$b_location)%>%dplyr::rename(.,state=region)
          
        }
        
      })
      
      BarData3 <-reactive({dplyr::filter(BarData2(),BarData2()$year%in%input$Bar_year)})
      BarData3()
      
      dt<-BarData3()
      dt$year=as.character(dt$year)
      if ( dt$category[1]%in%c("STOCKER INDEX","HEIFER RETENTION")) {
        dt$value= round(dt$value,digits = 2)
      }
      else{
        dt$value=as.integer(dt$value)
      }
      choice5<-reactive({input$Bar_sub_category})
      
      if (choice5()==c("State")){ 
        names(dt)[1] <- "state"
      }
      
      else{
        names(dt)[1] <- "region"
      }
      dt
      
    })
    
    output$download_Bar_Plot = downloadHandler(paste(input$Bar_category,".png", sep=""), content =function(file) { 
      ggsave(file,plot = last_plot(), scale = 1,
             # width = 5, height = 3,
             units = c("in"),
             dpi = 300)})
    
    output$download_Bar_Data = downloadHandler(paste(input$Bar_category,".csv", sep=""), content =function(file) {
      BarData<-reactive({dplyr::filter(data,data$category%in%input$Bar_category)})
      BarData1<-reactive({dplyr::filter(data1,data1$category%in%input$Bar_category)})
      choice1<-reactive({input$Bar_sub_category})
      BarData2 <-reactive({
        if (choice1()==c("State")) {
          
          dplyr::filter(BarData(),BarData()$state_fips%in%input$b_location)
        }
        
        else {
          dplyr::filter(BarData1(),BarData1()$region%in%input$b_location)%>%dplyr::rename(.,state=region)
          
        }
        
      })
      BarData3 <-reactive({dplyr::filter(BarData2(),BarData2()$year%in%input$Bar_year)})  
      dt<-BarData3()
      dt$year=as.character(dt$year)
      if ( dt$category[1]==c("STOCKER INDEX")) {
        dt$value= round(dt$value,digits = 2)
      }
      else{
        dt$value=as.integer(dt$value)
      }
      
      choice2<-reactive({input$Bar_sub_category})
      
      if (choice2()==c("State")){ }
      
      else{
        names(dt)[1] <- "region"
      }
      
      dt
      
      write.table(dt, file  ,sep=",",row.names =FALSE,quote = FALSE) })
    
    
    
    ####################
    ##Line Chart #######
    #####################
    output$Line_ui <- renderUI({
      
      controls <-{
        list(
          #h4("States"),
          tags$div(
            # align = 'left', 
            class = 'multicol',
            checkboxGroupInput(inputId  = "l_location", 
                               label= "States",
                               choices = c("Alabama"="01", "Alaska"="02", "Arizona"="04", "Arkansas"="05",
                                           "California"="06", "Colorado"="08", "Connecticut"="09", 
                                           "Delaware"="10", "Florida"="12","Georgia"="13",
                                           "Hawaii"="15", "Idaho"="16", "Illinois"="17", "Indiana"="18",
                                           "Iowa"="19","Kansas"="20", "Kentucky"="21", "Louisiana"="22",
                                           "Maine"="23", "Maryland"="24", "Massachusetts"="25",
                                           "Michigan"="26", "Minnesota"="27", "Mississippi"="28", 
                                           "Missouri"="29", "Montana"="30", "Nebraska"="31", "Nevada"="32",
                                           "New Hampshire"="33", "New Jersey"="34", "New Mexico"="35",
                                           "New York"="36", "North Carolina"="37", "North Dakota"="38",
                                           "Ohio"="39", "Oklahoma"="40","Oregon"="41", "Pennsylvania"="42",
                                           "Rhode Island"="44", "South Carolina"="45", "South Dakota"="46",
                                           "Tennessee"="47", "Texas"="48", "Utah"="49", "Vermont"="50", 
                                           "Virginia"="51", "Washington"="53", "West Virginia"="54",
                                           "Wisconsin"="55", "Wyoming"="56"),
                               selected = c("31","40","19","48","20","46"),
                               inline   = F))) }
      
      
      if (is.null(input$Line_sub_category))
        return()
      ##switch
      switch(
        input$Line_sub_category,
        
        "Region"= checkboxGroupInput(inputId="l_location", 
                                     label= "Regions",
                                     choices = c("Northwest" ="Northwest","Southwest" ="Southwest","North.Rockies"="North.Rockies",
                                                 "South.Rockies"="South.Rockies","North.Plains"="North.Plains","South.Plains"="South.Plains",
                                                 "Great.Lakes"="Great.Lakes","Midwest"="Midwest","Gulf"="Gulf","South"="South",
                                                 "Appalachain"="Appalachain","East.seaboard"="East.seaboard","Northeast"="Northeast"),
                                     selected = c("Northwest" ="Northwest","Southwest" ="Southwest","North.Rockies"="North.Rockies",
                                                  "South.Rockies"="South.Rockies","North.Plains"="North.Plains","South.Plains"="South.Plains",
                                                  "Great.Lakes"="Great.Lakes","Midwest"="Midwest","Gulf"="Gulf","South"="South",
                                                  "Appalachain"="Appalachain","East.seaboard"="East.seaboard","Northeast"="Northeast")
        ),
        
        
        "State" = fluidRow(  controls,width = 4,tweaks) 
        
      )    
    })
    
    
    output$Line_Plot<-renderPlot({
      LineData<-reactive({dplyr::filter(data,data$category%in%input$Line_category)})
      LineData1<-reactive({dplyr::filter(data1,data1$category%in%input$Line_category)})
      choice<-reactive({input$Line_sub_category})
      LineData2 <-reactive({
        if (choice()==c("State")) {
          
          dplyr::filter(LineData(),LineData()$state_fips%in%input$l_location)
        }
        
        else  {
          dplyr::filter(LineData1(),LineData1()$region%in%input$l_location)%>%dplyr::rename(.,state=region)
          
        }
        
      })
      
      LineData3<-reactive({dplyr::filter(LineData2(),year<=input$Line_year[2])%>%dplyr::filter(.,year>=input$Line_year[1])%>%drop_na()})
      
      if (input$Line_category%in%c("STOCKER INDEX","HEIFER RETENTION"))
      {floor<-floor(min(req(LineData3()$value)))
      ceiling<-ceiling(max(req(LineData3()$value))) }
      else
      {floor<-floor(min(req(LineData3()$value))/100)*100
      ceiling<-ceiling(max(req(LineData3()$value))/100)*100}
      
      
      ggplot(LineData3(), aes(x=year, y=value, color=state,na.rm = TRUE))+
        geom_line()+
        ggtitle(paste(input$Line_category,sep = " "),
                subtitle = paste(input$Line_year[1],"-",input$Line_year[2],sep = " "))+
        scale_y_continuous( breaks =seq( from=floor,
                                         to=ceiling,
                                         by=(ceiling-floor)/5 ))+
        scale_x_yearmon(n=(input$Line_year[2]-input$Line_year[1]+1))+
        labs(y= "Value",x= "Year",color=input$Line_sub_category)+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
      
      
      
      
      
    })
    
    output$Line_Data<-renderTable({
      LineData<-reactive({dplyr::filter(data,data$category%in%input$Line_category)})
      LineData1<-reactive({dplyr::filter(data1,data1$category%in%input$Line_category)})
      choice<-reactive({input$Line_sub_category})
      LineData2 <-reactive({
        if (choice()==c("State")) {
          
          dplyr::filter(LineData(),LineData()$state_fips%in%input$l_location)
        }
        
        else  {
          dplyr::filter(LineData1(),LineData1()$region%in%input$l_location)%>%dplyr::rename(.,state=region)
          
        }
        
      })
      LineData3<-reactive({dplyr::filter(LineData2(),year<=input$Line_year[2])%>%dplyr::filter(.,year>=input$Line_year[1])})
      
      dt<-LineData3()
      dt$year=as.character(dt$year)
      
      # if ( dt$category[1]%in%c("STOCKER INDEX")) { }
      #  else{
      #   dt$value=as.integer(dt$value)
      # }
      # 
      # choice3<-reactive({input$Line_sub_category})
      #   
      #  if (choice3()==c("State")){ 
      #    names(dt)[1] <- "state"
      #    }
      #     
      #  else{
      #    names(dt)[1] <- "region"
      #           }
      
      dt
    })
    
    output$download_Line_Plot = downloadHandler(paste(input$Line_category,".png", sep=""), content =function(file)    { 
      ggsave(file,plot = last_plot(), scale = 1,
             # width = 5, height = 3,
             units = c("in"),
             dpi = 300)})
    
    output$download_Line_Data = downloadHandler(paste(input$Line_category,".csv", sep=""), content =function(file) {
      LineData<-reactive({dplyr::filter(data,data$category%in%input$Line_category)})
      LineData1<-reactive({dplyr::filter(data1,data1$category%in%input$Line_category)})
      choice<-reactive({input$Line_sub_category})
      LineData2 <-reactive({
        if (choice()==c("State")) {
          
          dplyr::filter(LineData(),LineData()$state_fips%in%input$l_location)
        }
        
        else  {
          dplyr::filter(LineData1(),LineData1()$region%in%input$l_location)%>%dplyr::rename(.,state=region)
          
        }
        
      })
      
      LineData3<-reactive({dplyr::filter(LineData2(),year<=input$Line_year[2])%>%dplyr::filter(.,year>=input$Line_year[1])})
      
      dt<-LineData3()
      dt$year=as.character(dt$year)
      
      if ( dt$category[1]==c("STOCKER INDEX")) { }
      else{
        dt$value=as.integer(dt$value)
      }
      
      choice3<-reactive({input$Line_sub_category})
      
      if (choice3()==c("State")){ }
      
      else{
        names(dt)[1] <- "region"
      }
      
      dt
      
      write.table(dt, file  ,sep=",",row.names =FALSE,quote = FALSE) })
    
    
    
    
    
    
    
    
    
  }
  
)

