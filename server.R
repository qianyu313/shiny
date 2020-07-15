



setwd("/Users/qianyudong/Documents/R/shiny/shiny05")

library(magrittr)
library(stats)
library(forecast)
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


#load data 
CARCASS<-readRDS("/Users/qianyudong/Documents/R/shiny/shiny05/CARCASS1.rds")
CUTS<-readRDS("/Users/qianyudong/Documents/R/shiny/shiny05/CUTS.rds")
OTHER<-readRDS("/Users/qianyudong/Documents/R/shiny/shiny05/OTHER.rds")




shinyServer(
  
  function(input, output,session) {
    
 #################   
 #####Historical##
 ################# 
    output$Hist_Plot<-renderPlot({
      
  if (input$Hist_time==c("Daily")){
    
      if (input$Hist_Daily_cate==c("Carcass"))
      { req(input$Hist_Daily_Carcass)
        histData1<-reactive({dplyr::filter(CARCASS,CARCASS$category%in%input$Hist_Daily_Carcass)})
        histData2<-reactive({
          req(input$value_Gross)
          req(input$value_Foresaddle)
          req(input$value_Hindsaddle)
          req(input$value_Net)
          if (input$Hist_Daily_Carcass==c("Gross"))
          { histData1()%>%select(report_date,category,input$value_Gross)}
          
          else if(input$Hist_Daily_Carcass==c("Foresaddle"))
          { req(input$value_Foresaddle)
            histData1()%>%select(report_date,category,input$value_Foresaddle)}
          
          else if(input$Hist_Daily_Carcass==c("Hindsaddle"))
          { histData1()%>%select(report_date,category,input$value_Hindsaddle)}
          
          else
          { histData1()%>%select(report_date,category,input$value_Net)}
          
          
        })
        
        histData3<-reactive({dplyr::filter(histData2(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.Date(report_date,format='%m-%d-%Y'))
        })
        ylab <-reactive({
            if (colnames(histData3())[3]==c("Price"))
            { c("U.S. dollars per 100 pounds")}
            
            else if(colnames(histData3())[3]==c("Change"))
            {c("U.S. dollars per 100 pounds")}
            
            else if(colnames(histData3())[3]==c("Percentage"))
            { c("%")}
            
            else
            {c("Pounds")}})
        
        value_in_title <-reactive({
          if (colnames(histData3())[3]==c("Price"))
          { c("Price")}
          
          else if(colnames(histData3())[3]==c("Change"))
          {c("Price Change")}
          
          else if(colnames(histData3())[3]==c("Percentage"))
          { c("Percentage")}
          
          else
          {c("Weight")}})
      
       ggplot(data=histData3()) +
            geom_line( aes(x=report_date, y=histData3()[,3],group = 1))+
            labs(y= ylab(),x="Date")+
            ggtitle(paste(input$Hist_time,"Lamb",input$Hist_Daily_Carcass,value_in_title(),sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
            # scale_y_continuous(breaks = NULL)+
            scale_x_date(labels = date_format("%b %d %Y")) +
            theme_bw()+
            theme(axis.text.x = element_text(angle = 45,hjust = 1),
                  plot.margin = unit(c(1, 1, 1, 1),"cm"))
          
          
       
             
        
      }
      else if (input$Hist_Daily_cate==c("Cuts"))
      {
        histData1<-reactive({dplyr::filter(CUTS,CUTS$imps_description%in%input$Hist_Daily_Cuts)})
        histData2<-reactive({histData1()%>%select(report_date,imps_description,input$Hist_Daily_Cuts_Value)%>%
          rename(Type=imps_description)})
        histData3<-reactive({dplyr::filter(histData2(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.Date(report_date,format='%m-%d-%Y'))
        })
        ylab <-reactive({
          if (colnames(histData3())[3]==c("fob_price"))
          { c("U.S. dollars per 100 pounds")}
          
          else if(colnames(histData3())[3]==c("price_change_from_last"))
          {c("U.S. dollars per 100 pounds")}
          
          else if(colnames(histData3())[3]==c("percentage_carcass"))
          { c("%")}
          
          else
          {c("Pounds")}
        })
        title <-reactive({
          if (colnames(histData3())[3]==c("fob_price"))
          { c("FOB Price")}
          
          else if(colnames(histData3())[3]==c("price_change_from_last"))
          {c("Price Change")}
          
          else if(colnames(histData3())[3]==c("percentage_carcass"))
          { c("Percentage")}
          
          else
          {c("Weight")}
        })
        
        ggplot(data=histData3(),aes(x=report_date, y=histData3()[,3], color=Type, na.rm=T, group=Type)) +
          geom_line()+
          labs(y= ylab(),x="Date")+
          ggtitle(paste(input$Hist_time,"Lamb",title(),sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
          #scale_x_discrete(breaks = 5)+
          scale_x_date(labels = date_format("%b %d %Y")) +
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45,hjust = 1),
                plot.margin = unit(c(1, 1, 1, 1),"cm")
                
                )
        
      }
      else
      {
        histData1<-reactive({OTHER%>%select(report_date,input$Hist_Daily_Other_Value)   })
        histData2<-reactive({dplyr::filter(histData1(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.Date(report_date,format='%m-%d-%Y'))
        })


        ylab <-reactive({
          if (colnames(histData2())[2]==c("processing_cost"))
          { c("U.S. dollars per 100 pounds")}
          
          else if(colnames(histData2())[2]==c("shrink_value"))
          {c("Pounds")}
          
          else if(colnames(histData2())[2]==c("ls711_weight"))
          { c("Pounds")}
          
          else
          {c("Pounds")}
        })
        title <-reactive({
          if (colnames(histData2())[2]==c("processing_cost"))
          { c("Processing Cost")}
          
          else if(colnames(histData2())[2]==c("shrink_value"))
          {c("Shrink and Trim")}
          
          else if(colnames(histData2())[2]==c("ls711_weight"))
          { c("Carcass Weight")}
          
          else
          {c("Weight Difference")}
        })        
         ggplot(data=histData2()) +
          geom_line( aes(x=report_date, y=histData2()[,2],group = 1))+
          labs(y= ylab(),x="Date")+
          ggtitle(paste(input$Hist_time,"Lamb",title(),sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
          #scale_x_discrete(breaks = 5)+
           scale_x_date(labels = date_format("%b %d %Y")) +
           
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45,hjust = 1),
                plot.margin = unit(c(1, 1, 1, 1),"cm"))


      }
  }
  else
  {}
      
    })
  
    output$Hist_Data<-renderTable({
  
   if (input$Hist_time==c("Daily")){
      
     asdfasdfasdfasfasdfas
    if (input$Hist_Daily_cate==c("Carcass"))
       { req(input$Hist_Daily_Carcass)
       histData1<-reactive({dplyr::filter(CARCASS,CARCASS$category%in%input$Hist_Daily_Carcass)})
       histData2<-reactive({
       req(input$value_Gross)
       req(input$value_Foresaddle)
       req(input$value_Hindsaddle)
       req(input$value_Net)
       if (input$Hist_Daily_Carcass==c("Gross"))
       { histData1()%>%select(report_date,category,input$value_Gross)}

       else if(input$Hist_Daily_Carcass==c("Foresaddle"))
       { req(input$value_Foresaddle)
         histData1()%>%select(report_date,category,input$value_Foresaddle)}

       else if(input$Hist_Daily_Carcass==c("Hindsaddle"))
       { histData1()%>%select(report_date,category,input$value_Hindsaddle)}

       else
       { histData1()%>%select(report_date,category,input$value_Net)}


       })

      histData3<-reactive({dplyr::filter(histData2(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
 })
      histData3()
        }
     else if (input$Hist_Daily_cate==c("Cuts"))
       {
        histData1<-reactive({dplyr::filter(CUTS,CUTS$imps_description%in%input$Hist_Daily_Cuts)})
        histData2<-reactive({histData1()%>%select(report_date,imps_description,input$Hist_Daily_Cuts_Value)})
        histData3<-reactive({dplyr::filter(histData2(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
          
           })
        histData3()
        }
     else
       {
        
       histData2<-reactive({OTHER%>%select(report_date,input$Hist_Daily_Other_Value)   })
       histData3<-reactive({dplyr::filter(histData2(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
       
         })
       histData3() 
  
       
     }   
      
   }
      
   else
   {}
      
     
    })


 ################ 
 #####Seasonal###
 ################
    output$Seasonal_Plot<-renderPlot({
      if (input$Seasonal_time==c("Daily")){
        
          
      if (input$Seasonal_Daily_cate==c("Carcass"))
      {
      
       req(input$Seasonal_Daily_Carcass)
      
      #select Carcass type(gross,Foresaddle,Hindsaddle,net), and add month, year, ym(year and month)
      SeasonalData1<-reactive({
        req(input$Seasonal_Daily_Carcass)
        
        dplyr::filter(CARCASS,CARCASS$category%in%input$Seasonal_Daily_Carcass)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
      #select Value(price, change...)
      SeasonalData2<-reactive({
        req(input$Seasonal_Daily_Carcass)
        req(input$Seasonal_Carcass_Gross_Value)
        req(input$Seasonal_Carcass_Foresaddle_Value)
        req(input$Seasonal_Carcass_Hindsaddle_Value)
        req(input$Seasonal_Carcass_Net_Value)
        if (input$Seasonal_Daily_Carcass==c("Gross"))
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Gross_Value,year,month,ym)}
        
        else if(input$Seasonal_Daily_Carcass==c("Foresaddle"))
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Foresaddle_Value,year,month,ym)}
        
        else if(input$Seasonal_Daily_Carcass==c("Hindsaddle"))
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Hindsaddle_Value,year,month,ym)}
        
        else
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Net_Value,year,month,ym)}
        
        
      })
      
      #select time range
    
      SeasonalData3<-reactive({
      
        dplyr::filter(SeasonalData2(),year>=input$Seasonal_yrs[1])%>%
          dplyr::filter(.,year<=input$Seasonal_yrs[2])%>%
          dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
        
      })
      
      #calculate mean and order by time
      SeasonalData4<-reactive({ aggregate(SeasonalData3()[,3],list(SeasonalData3()$ym), FUN=mean) %>%
          mutate(date = as.yearmon(Group.1 )) %>%
          arrange(date) 
      })
      #ts()
      SeasonalData5<-reactive({ ts(SeasonalData4()[,2],start=as.numeric(head(SeasonalData4(),n=1)[,3]), end=as.numeric(tail(SeasonalData4(),n=1)[,3]),frequency=12) })
       

      ylab <-reactive({
        if (colnames(SeasonalData2())[3]==c("Price"))
        { c("U.S. dollars per 100 pounds")}

        else if(colnames(SeasonalData2())[3]==c("Change"))
        {c("U.S. dollars per 100 pounds")}

        else if(colnames(SeasonalData2())[3]==c("Percentage"))
        { c("%")}

        else
        {c("Pounds")}
      })
      tittle <-reactive({
        if (colnames(SeasonalData2())[3]==c("Price"))
        { c("Price")}
        
        else if(colnames(SeasonalData2())[3]==c("Change"))
        {c("Change")}
        
        else if(colnames(SeasonalData2())[3]==c("Percentage"))
        { c("Percentage")}
        
        else
        {c("Weight")}
      })
      
        ggseasonplot(SeasonalData5(), year.labels=TRUE, year.labels.left=TRUE)+
       theme_bw()+
      # ggplot(data=SeasonalData3()) +
      #   geom_line( aes(x=report_date, y=SeasonalData3()[,3],group = 1))+
       labs(y= ylab(),x="Month")+
       ggtitle(paste(input$Seasonal_time,"Lamb",input$Seasonal_Daily_Carcass,tittle(),sep = " "),subtitle = paste(input$Seasonal_yrs[1],"--",input$Seasonal_yrs[2],sep = " ")) 
      #   # scale_x_date(breaks = date_breaks("1 day"),
      #   #              labels = date_format("%d")) +
      # 
      #   theme_bw()+
      #   theme(axis.text.x = element_text(angle = 45,hjust = 1),
      #         plot.margin = unit(c(1, 1, 1, 1),"cm"))

      }
      else if (input$Seasonal_Daily_cate==c("Cuts"))
      {
        #select cut type, and add month, year, ym(year and month)
        SeasonalData1<-reactive({
          dplyr::filter(CUTS,CUTS$imps_description%in%input$Seasonal_Daily_Cuts)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
        #select value 
        SeasonalData2<-reactive({SeasonalData1()%>%select(year,month,ym,imps_description,input$Seasonal_Daily_Cuts_Value)})
        #select year range
        SeasonalData3<-reactive({dplyr::filter(SeasonalData2(),year<=input$Seasonal_yrs[2])%>%dplyr::filter(.,year>=input$Seasonal_yrs[1])
        })
        #calculate mean
        SeasonalData4<-reactive({ aggregate(SeasonalData3()[,5],list(SeasonalData3()$ym,SeasonalData3()$imps_description), FUN=mean) %>%
            mutate(date = as.yearmon(Group.1 )) %>%
            mutate(month = as.Date(month(date ))) %>%
            arrange(.,date)
        })
        # SeasonalData5<-reactive({ ts(SeasonalData4()[,3],start=as.numeric(head(SeasonalData4(),n=1)[,4]), end=as.numeric(tail(SeasonalData4(),n=1)[,4]),frequency=12,class = c("mts")) })
        
      
        
        
        SeasonalData5<-reactive({ matrix(SeasonalData4()[,3],nrow = length(unique(SeasonalData4()[,1])),ncol=length(unique(SeasonalData4()[,2])),byrow=T)
        })

        
        ylab <-reactive({
          if (colnames(SeasonalData3())[3]==c("fob_price"))
          { c("U.S. dollars per 100 pounds")}
          
          else if(colnames(SeasonalData3())[3]==c("price_change_from_last"))
          {c("U.S. dollars per 100 pounds")}
          
          else if(colnames(SeasonalData3())[3]==c("percentage_carcass"))
          { c("%")}
          
          else
          {c("Pounds")}
        })
        title <-reactive({
          if (colnames(SeasonalData3())[3]==c("fob_price"))
          { c("FOB Price")}
          
          else if(colnames(SeasonalData3())[3]==c("price_change_from_last"))
          {c("Price Change")}
          
          else if(colnames(SeasonalData3())[3]==c("percentage_carcass"))
          { c("Percentage")}
          
          else
          {c("Weight")}
        })        
        ggplot(data=SeasonalData4(),aes(x=month, y=SeasonalData4()[,3], color=Group.2, na.rm=T, group=Group.2)) +
          geom_line()+
          labs(y= ylab(),x="Date")+
          ggtitle(paste(input$Hist_time,"Lamb",title(),sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
        #   #scale_x_discrete(breaks = 5)+
          scale_x_date(labels = date_format("%b")) +

        # ggseasonplot(SeasonalData4(), year.labels=TRUE, year.labels.left=TRUE)+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45,hjust = 1),
                plot.margin = unit(c(1, 1, 1, 1),"cm"))
      }
      else
      {
        SeasonalData1<-reactive({OTHER%>%select(report_date,input$Seasonal_Daily_Other_Value)   })
        SeasonalData2<-reactive({dplyr::filter(SeasonalData1(),report_date<=input$Seasonal_yrs[2])%>%dplyr::filter(.,report_date>=input$Seasonal_yrs[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y')) })
        
        #calculate mean and order by time
        SeasonalData3<-reactive({ aggregate(SeasonalData2()[,3],list(SeasonalData3()$ym), FUN=mean) %>%
            mutate(date = as.yearmon(Group.1 )) %>%
            arrange(date) 
        })
        #ts()
        SeasonalData5<-reactive({ ts(SeasonalData4()[,2],start=as.numeric(head(SeasonalData4(),n=1)[,3]), end=as.numeric(tail(SeasonalData4(),n=1)[,3]),frequency=12) })
        
        
          ylab <-reactive({input$Hist_Daily_Other_Value })
          
          ggplot(data=histData3()) +
            geom_line( aes(x=report_date, y=histData3()[,2],group = 1))+
            labs(y= ylab(),x="Date")+
            ggtitle(paste(input$Hist_time,"Lamb",input$Hist_Daily_Other_Value,sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
            #scale_x_discrete(breaks = 5)+
            # scale_x_date(breaks = date_breaks("1 day"),
            #              labels = date_format("%d")) +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 45,hjust = 1),
                  plot.margin = unit(c(1, 1, 1, 1),"cm"))
          
          
      
        
      }
 }
      else{}  
    })
    
    output$Seasonal_Data<-renderTable({
      if (input$Seasonal_time==c("Daily")){ 
      
       if (input$Seasonal_Daily_cate==c("Carcass"))
        {
      #select Carcass type(gross,Foresaddle,Hindsaddle,net), and add month, year, ym(year and month)
      SeasonalData1<-reactive({dplyr::filter(CARCASS,CARCASS$category%in%input$Seasonal_Daily_Carcass) %>%
       dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))
        })
      #select Value(price, change...)
      SeasonalData2<-reactive({
        req(input$Seasonal_Daily_Carcass)
        
        if (input$Seasonal_Daily_Carcass==c("Gross"))
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Gross_Value,year,month,ym)}

        else if(input$Seasonal_Daily_Carcass==c("Foresaddle"))
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Foresaddle_Value,year,month,ym)}

        else if(input$Seasonal_Daily_Carcass==c("Hindsaddle"))
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Hindsaddle_Value,year,month,ym)}

        else
        { SeasonalData1()%>%select(report_date,category,input$Seasonal_Carcass_Net_Value,year,month,ym)}


      })
      #select time range
      SeasonalData3<-reactive({dplyr::filter(SeasonalData2(),year>=input$Seasonal_yrs[1])%>%dplyr::filter(.,year<=input$Seasonal_yrs[2])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))

     })
      #calculate mean and order by time
      SeasonalData4<-reactive({ aggregate(SeasonalData3()[,3],list(SeasonalData3()$ym), FUN=mean,na.rm=T) %>%
          mutate(date = as.yearmon(Group.1 )) %>%
          arrange(date)
       })
      #ts()
      SeasonalData5<-reactive({ ts(SeasonalData4()[,2],start=as.numeric(head(SeasonalData4(),n=1)[,3]), end=as.numeric(tail(SeasonalData4(),n=1)[,3]),frequency=12) })
      
      # SeasonalData5<-reactive({ ts(SeasonalData4()[,2],start=c(input$Seasonal_yrs[1], 1), end=c(input$Seasonal_yrs[2], 12),frequency=12) })
      SeasonalData3()[,1:3]
      }
       else if (input$Seasonal_Daily_cate==c("Cuts"))
        {
         req(input$Seasonal_Daily_Cuts)
         #select cut type, and add month, year, ym(year and month)
         SeasonalData1<-reactive({
         dplyr::filter(CUTS,CUTS$imps_description%in%input$Seasonal_Daily_Cuts)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
         #select value 
         SeasonalData2<-reactive({SeasonalData1()%>%select(year,month,ym,imps_description,input$Seasonal_Daily_Cuts_Value)})
         #select year range
         SeasonalData3<-reactive({dplyr::filter(SeasonalData2(),year<=input$Seasonal_yrs[2])%>%dplyr::filter(.,year>=input$Seasonal_yrs[1])
          })
         #calculate mean
         SeasonalData4<-reactive({ aggregate(SeasonalData3()[,5],list(SeasonalData3()$ym,SeasonalData3()$imps_description), FUN=mean,na.rm=T) %>%
             mutate(dt = as.yearmon(Group.1 )) %>%
             rename(date = Group.1,item=Group.2) %>%
                 arrange(.,date)
             })
         # SeasonalData5 <- reactive({ as.data.frame(matrix(ncol=length(unique(SeasonalData4()$item)), nrow=(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12+1) 
         #                                           ) %>%
         #     
         #     set_colnames(.,unique(SeasonalData4()$item)) %>%
         #    # with(., row.names=as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
         #     set_rownames(.,as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
         #   })
         
         # row.names(output)<-as.character.Date(unique(table2$date))
         #colnames(output)<-unique(table2$item)
         
         SeasonalData5 <- reactive({ 
          df<- as.data.frame(matrix(ncol=length(unique(SeasonalData4()$item)), nrow=(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12+1)) %>%
               set_colnames(.,unique(SeasonalData4()$item)) 
               # with(., row.names=as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
               #set_rownames(.,as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
          nrow=(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12
           row.names(df)<-as.yearmon(min(SeasonalData4()[,4]) + seq(0, nrow)/12)
          
          
          
          for(i in 1:(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12){
           date = SeasonalData4()[i, 4]
           item = SeasonalData4()[i, 2]
           df[which(row.names(df)==date),which(colnames(df)==item)] = SeasonalData4() [i ,3]
          }
          return(df)
         })
         # 
         # SeasonalData5<-reactive({ ts(SeasonalData4()[,3],start=as.numeric(head(SeasonalData4(),n=1)[,4]), end=as.numeric(tail(SeasonalData4(),n=1)[,4]),frequency=12,class = c("mts")) })
         # SeasonalData5<-reactive({ matrix(SeasonalData4()[,3],ncol=length(unique(SeasonalData4()[,2])),byrow=T)
         #  })
         
         SeasonalData5()
        
       
        
        
        
      }
       else
        {
         # histData2<-reactive({OTHER%>%select(report_date,input$Hist_Daily_Other_Value)   })
          require(OTHER)
        SeasonalData1<-reactive({OTHER%>%select(report_date,input$Seasonal_Daily_Other_Value)   })
        SeasonalData2<-reactive({dplyr::filter(SeasonalData1(),report_date<=input$Seasonal_yrs[2])%>%dplyr::filter(.,report_date>=input$Seasonal_yrs[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y')) })
        SeasonalData1()
        
      }
      }
      
      else
      {}
    })
    
 ################### 
 #####SeasonalSub###
 ################### 
    output$Seasonal_sub_Plot<-renderPlot({
      if (input$Seasonal_sub_time==c("Daily")){
        
        
        if (input$Seasonal_sub_Daily_cate==c("Carcass"))
        {
          req(input$Seasonal_sub_Daily_Carcass)
          
          #select Carcass type(gross,Foresaddle,Hindsaddle,net), and add month, year, ym(year and month)
          Seasonal_subData1<-reactive({
            req(input$Seasonal_sub_Daily_Carcass)
            
            dplyr::filter(CARCASS,CARCASS$category%in%input$Seasonal_sub_Daily_Carcass)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
          #select Value(price, change...)
          Seasonal_subData2<-reactive({
            req(input$Seasonal_sub_Daily_Carcass)
            req(input$Seasonal_sub_Carcass_Gross_Value)
            req(input$Seasonal_sub_Carcass_Foresaddle_Value)
            req(input$Seasonal_sub_Carcass_Hindsaddle_Value)
            req(input$Seasonal_sub_Carcass_Net_Value)
            if (input$Seasonal_sub_Daily_Carcass==c("Gross"))
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Gross_Value,year,month,ym)}
            
            else if(input$Seasonal_sub_Daily_Carcass==c("Foresaddle"))
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Foresaddle_Value,year,month,ym)}
            
            else if(input$Seasonal_sub_Daily_Carcass==c("Hindsaddle"))
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Hindsaddle_Value,year,month,ym)}
            
            else
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Net_Value,year,month,ym)}
            
            
          })
          
          #select time range
          
          Seasonal_subData3<-reactive({
            
            dplyr::filter(Seasonal_subData2(),year>=input$Seasonal_sub_yrs[1])%>%dplyr::filter(.,year<=input$Seasonal_sub_yrs[2])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
            
          })
          #calculate mean and order by time
          Seasonal_subData4<-reactive({ aggregate(Seasonal_subData3()[,3],list(Seasonal_subData3()$ym), FUN=mean) %>%
              mutate(date = as.yearmon(Group.1 )) %>%
              arrange(date) 
          })
          #ts()
          Seasonal_subData5<-reactive({
            ts(Seasonal_subData4()[,2],start=as.numeric(head(Seasonal_subData4(),n=1)[,3]), end=as.numeric(tail(Seasonal_subData4(),n=1)[,3]),frequency=12) })
          
          
          
          ylab <-reactive({
            if (colnames(Seasonal_subData2())[3]==c("Price"))
            { c("U.S. dollars per 100 pounds")}
            
            else if(colnames(Seasonal_subData2())[3]==c("Change"))
            {c("U.S. dollars per 100 pounds")}
            
            else if(colnames(Seasonal_subData2())[3]==c("Percentage"))
            { c("%")}
            
            else
            {c("Pounds")}
          })
          tittle <-reactive({
            if (colnames(Seasonal_subData2())[3]==c("Price"))
            { c("Price")}
            
            else if(colnames(Seasonal_subData2())[3]==c("Change"))
            {c("Change")}
            
            else if(colnames(Seasonal_subData2())[3]==c("Percentage"))
            { c("Percentage")}
            
            else
            {c("Weight")}
          })
          
          ggsubseriesplot(Seasonal_subData5(), year.labels=TRUE, year.labels.left=TRUE)+
            theme_bw()+
            # ggplot(data=Seasonal_subData3()) +
            #   geom_line( aes(x=report_date, y=Seasonal_subData3()[,3],group = 1))+
            labs(y= ylab(),x="Month")+
            ggtitle(paste(input$Seasonal_sub_time,"Lamb",input$Seasonal_sub_Daily_Carcass,tittle(),sep = " "),subtitle = paste(input$Seasonal_yrs[1],"--",input$Seasonal_yrs[2],sep = " ")) 
          #   # scale_x_date(breaks = date_breaks("1 day"),
          #   #              labels = date_format("%d")) +
          # 
          #   theme_bw()+
          #   theme(axis.text.x = element_text(angle = 45,hjust = 1),
          #         plot.margin = unit(c(1, 1, 1, 1),"cm"))
          
        }
        else if (input$Seasonal_sub_Daily_cate==c("Cuts"))
        {
          #select cut type, and add month, year, ym(year and month)
          Seasonal_subData1<-reactive({
            dplyr::filter(CUTS,CUTS$imps_description%in%input$Seasonal_sub_Daily_Cuts)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
          #select value 
          Seasonal_subData2<-reactive({Seasonal_subData1()%>%select(year,month,ym,imps_description,input$Seasonal_sub_Daily_Cuts_Value)})
          #select year range
          Seasonal_subData3<-reactive({dplyr::filter(Seasonal_subData2(),year<=input$Seasonal_sub_yrs[2])%>%dplyr::filter(.,year>=input$Seasonal_sub_yrs[1])
          })
          #calculate mean
          Seasonal_subData4<-reactive({ aggregate(Seasonal_subData3()[,5],list(Seasonal_subData3()$ym,Seasonal_subData3()$imps_description), FUN=mean) %>%
              mutate(date = as.yearmon(Group.1 )) %>%
              arrange(.,date)
          })

          
          
          
          Seasonal_subData5<-reactive({ matrix(Seasonal_subData4()[,3],nrow = length(unique(Seasonal_subData4()[,1])),
                                           ncol=length(unique(Seasonal_subData4()[,2])),byrow=T)
          })
          
          
          ylab <-reactive({input$Seasonal_sub_Daily_Other_Value })
          
          ggplot(data=Seasonal_subData4(),aes(x=Group.1, y=SeasonalData4()[,3], color=Group.2, na.rm=T, group=Group.2)) +
            geom_line()
          #   labs(y= ylab(),x="Date")+
          #   ggtitle(paste(input$Hist_time,"Lamb",input$Hist_Daily_Other_Value,sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
          #   #scale_x_discrete(breaks = 5)+
          #   # scale_x_date(breaks = date_breaks("1 day"),
          #   #              labels = date_format("%d")) +
          # 
          # ggseasonplot(SeasonalData5(), year.labels=TRUE, year.labels.left=TRUE)+
          #   theme_bw()+
          #   theme(axis.text.x = element_text(angle = 45,hjust = 1),
          #         plot.margin = unit(c(1, 1, 1, 1),"cm"))
        }
        else
        {
          histData2<-reactive({OTHER%>%select(report_date,input$Hist_Daily_Other_Value)   })
          histData3<-reactive({dplyr::filter(histData2(),report_date<=input$Hist_days[2])%>%dplyr::filter(.,report_date>=input$Hist_days[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y')) })
          
          ylab <-reactive({input$Hist_Daily_Other_Value })
          
          ggplot(data=histData3()) +
            geom_line( aes(x=report_date, y=histData3()[,2],group = 1))+
            labs(y= ylab(),x="Date")+
            ggtitle(paste(input$Hist_time,"Lamb",input$Hist_Daily_Other_Value,sep = " "),subtitle = paste(input$Hist_days[1],"--",input$Hist_days[2],sep = " ")) +
            #scale_x_discrete(breaks = 5)+
            # scale_x_date(breaks = date_breaks("1 day"),
            #              labels = date_format("%d")) +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 45,hjust = 1),
                  plot.margin = unit(c(1, 1, 1, 1),"cm"))
          
          
          
          
        }
      }
      else{}  
    })
    
    output$Seasonal_sub_Data<-renderTable({
      if (input$Seasonal_sub_time==c("Daily")){ 
        
        if (input$Seasonal_sub_Daily_cate==c("Carcass"))
        {
          req(input$Seasonal_sub_Daily_Carcass)
          
          #select Carcass type(gross,Foresaddle,Hindsaddle,net), and add month, year, ym(year and month)
          Seasonal_subData1<-reactive({
            req(input$Seasonal_sub_Daily_Carcass)
            
            dplyr::filter(CARCASS,CARCASS$category%in%input$Seasonal_sub_Daily_Carcass)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
          #select Value(price, change...)
          Seasonal_subData2<-reactive({
            req(input$Seasonal_sub_Daily_Carcass)
            req(input$Seasonal_sub_Carcass_Gross_Value)
            req(input$Seasonal_sub_Carcass_Foresaddle_Value)
            req(input$Seasonal_sub_Carcass_Hindsaddle_Value)
            req(input$Seasonal_sub_Carcass_Net_Value)
            if (input$Seasonal_sub_Daily_Carcass==c("Gross"))
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Gross_Value,year,month,ym)}
            
            else if(input$Seasonal_sub_Daily_Carcass==c("Foresaddle"))
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Foresaddle_Value,year,month,ym)}
            
            else if(input$Seasonal_sub_Daily_Carcass==c("Hindsaddle"))
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Hindsaddle_Value,year,month,ym)}
            
            else
            { Seasonal_subData1()%>%select(report_date,category,input$Seasonal_sub_Carcass_Net_Value,year,month,ym)}
            
            
          })
          
          #select time range
          
          Seasonal_subData3<-reactive({
            
            dplyr::filter(Seasonal_subData2(),year>=input$Seasonal_sub_yrs[1])%>%dplyr::filter(.,year<=input$Seasonal_sub_yrs[2])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
            
          })
          #calculate mean and order by time
          Seasonal_subData4<-reactive({ aggregate(Seasonal_subData3()[,3],list(Seasonal_subData3()$ym), FUN=mean) %>%
              mutate(date = as.yearmon(Group.1 )) %>%
              arrange(date) 
          })
          #ts()
          Seasonal_subData5<-reactive({
            ts(Seasonal_subData4()[,2],start=as.numeric(head(Seasonal_subData4(),n=1)[,3]), end=as.numeric(tail(Seasonal_subData4(),n=1)[,3]),frequency=12) })
          
          
          Seasonal_subData3()[,1:3]
        }
        else if (input$Seasonal_Daily_cate==c("Cuts"))
        {
          req(input$Seasonal_Daily_Cuts)
          #select cut type, and add month, year, ym(year and month)
          SeasonalData1<-reactive({
            dplyr::filter(CUTS,CUTS$imps_description%in%input$Seasonal_Daily_Cuts)%>%dplyr::mutate(.,month=month.abb[month(report_date)],year=as.character(year(report_date)),ym=as.character(as.yearmon(report_date,"%b %Y")))})
          #select value 
          SeasonalData2<-reactive({SeasonalData1()%>%select(year,month,ym,imps_description,input$Seasonal_Daily_Cuts_Value)})
          #select year range
          SeasonalData3<-reactive({dplyr::filter(SeasonalData2(),year<=input$Seasonal_yrs[2])%>%dplyr::filter(.,year>=input$Seasonal_yrs[1])
          })
          #calculate mean
          SeasonalData4<-reactive({ aggregate(SeasonalData3()[,5],list(SeasonalData3()$ym,SeasonalData3()$imps_description), FUN=mean,na.rm=T) %>%
              mutate(dt = as.yearmon(Group.1 )) %>%
              rename(date = Group.1,item=Group.2) %>%
              arrange(.,date)
          })
          # SeasonalData5 <- reactive({ as.data.frame(matrix(ncol=length(unique(SeasonalData4()$item)), nrow=(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12+1) 
          #                                           ) %>%
          #     
          #     set_colnames(.,unique(SeasonalData4()$item)) %>%
          #    # with(., row.names=as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
          #     set_rownames(.,as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
          #   })
          
          # row.names(output)<-as.character.Date(unique(table2$date))
          #colnames(output)<-unique(table2$item)
          
          SeasonalData5 <- reactive({ 
            df<- as.data.frame(matrix(ncol=length(unique(SeasonalData4()$item)), nrow=(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12+1)) %>%
              set_colnames(.,unique(SeasonalData4()$item)) 
            # with(., row.names=as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
            #set_rownames(.,as.yearmon(min(SeasonalData4()[,4]) +  seq(0, ((max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12))/12))
            nrow=(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12
            row.names(df)<-as.yearmon(min(SeasonalData4()[,4]) + seq(0, nrow)/12)
            
            
            
            for(i in 1:(max(SeasonalData4()[,4])-min(SeasonalData4()[,4]))*12){
              date = SeasonalData4()[i, 4]
              item = SeasonalData4()[i, 2]
              df[which(row.names(df)==date),which(colnames(df)==item)] = SeasonalData4() [i ,3]
            }
            return(df)
          })
          # 
          # SeasonalData5<-reactive({ ts(SeasonalData4()[,3],start=as.numeric(head(SeasonalData4(),n=1)[,4]), end=as.numeric(tail(SeasonalData4(),n=1)[,4]),frequency=12,class = c("mts")) })
          # SeasonalData5<-reactive({ matrix(SeasonalData4()[,3],ncol=length(unique(SeasonalData4()[,2])),byrow=T)
          #  })
          
          SeasonalData5()
          
          
          
          
          
        }
        else
        {
          SeasonalData1<-reactive({OTHER%>%select(report_date,input$Seasonal_Daily_Other_Value)   })
          SeasonalData2<-reactive({dplyr::filter(SeasonalData1(),report_date<=input$Seasonal_yrs[2])%>%dplyr::filter(.,report_date>=input$Seasonal_yrs[1])%>%dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y')) })
          SeasonalData2()
          
        }
      }
      
      else
      {}
    })
    
    
    
    
 
 ################## 
 #####Decomposed###
 ##################
    output$Decomposed_Plot<-renderPlot({
      
      req(input$Decomposed_Daily_Carcass)
      DecomposedData1<-reactive({dplyr::filter(CARCASS,CARCASS$category%in%input$Decomposed_Daily_Carcass)%>%
          dplyr::mutate(.,month=month(report_date),year=as.character(year(report_date)))})
      
      DecomposedData2<-reactive({
        
        if (input$Decomposed_Daily_Carcass==c("Gross"))
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Gross_Value,year,month)}
        
        else if(input$Decomposed_Daily_Carcass==c("Foresaddle"))
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Foresaddle_Value,year,month)}
        
        else if(input$Decomposed_Daily_Carcass==c("Hindsaddle"))
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Hindsaddle_Value,year,month)}
        
        else if(input$Decomposed_Daily_Carcass==c("Net"))
          
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Net_Value,year,month)}
        
        
      })
      
      DecomposedData2.5<-reactive({DecomposedData2()%>%
          dplyr::group_by(.,year,month)%>%
          filter(.,row_number()==1)
        
        
      })
      
      DecomposedData3<-reactive({dplyr::filter(DecomposedData2.5(),year>=input$Decomposed_yrs[1])%>%
          dplyr::filter(.,year<=input$Decomposed_yrs[2])%>%
          dplyr::mutate(ym=as.character(as.yearmon(report_date,"%b %Y")))
        # %>%
        #   dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
        
      })
      
      DecomposedData4<-reactive({ aggregate(DecomposedData3()[,3],list(DecomposedData3()$ym), FUN=mean) %>%
          mutate(date = as.yearmon(Group.1 )) %>%
          mutate(year = as.integer(year(date ))) %>%
          mutate(month =as.integer(month(date )))%>%
          arrange(date) 
      })
      
      
      DecomposedData5.5<-reactive({
        df<- as.data.frame(matrix(nrow = input$Decomposed_yrs[2]-input$Decomposed_yrs[1]+1, ncol = 12)) %>%
          set_colnames(.,c("1","2","3","4","5","6","7","8","9","10","11","12"))
        max<-input$Decomposed_yrs[2]
        min<-input$Decomposed_yrs[1]
        row.names(df)<-c(min:max)
        nrow = input$Decomposed_yrs[2]-input$Decomposed_yrs[1]+1
        for(i in 1:1000){
          year = DecomposedData4()[i, 4]
          month = DecomposedData4()[i, 5]
          df[which(row.names(df)==year),which(colnames(df)==month)] = DecomposedData4() [i ,2]
        }



        return(df)

      })

      DecomposedData5<-reactive({

        ts(
          data=DecomposedData4()[,2], 
          start = c(input$Decomposed_yrs[1],1), 
          end = c(input$Decomposed_yrs[2],12), 
          frequency = 12)

      })

      
      DecomposedData6<-reactive({
        
        stl(DecomposedData5(), s.window="periodic")
        
      })
       
      plot(DecomposedData6())
 
    })
    
    output$Decomposed_Data<-renderTable({
      
      
      
      

      req(input$Decomposed_Daily_Carcass)
      DecomposedData1<-reactive({dplyr::filter(CARCASS,CARCASS$category%in%input$Decomposed_Daily_Carcass)%>%
          dplyr::mutate(.,month=month(report_date),year=as.character(year(report_date)))})
      
      DecomposedData2<-reactive({
        
        if (input$Decomposed_Daily_Carcass==c("Gross"))
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Gross_Value,year,month)}
        
        else if(input$Decomposed_Daily_Carcass==c("Foresaddle"))
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Foresaddle_Value,year,month)}
        
        else if(input$Decomposed_Daily_Carcass==c("Hindsaddle"))
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Hindsaddle_Value,year,month)}
        
        else if(input$Decomposed_Daily_Carcass==c("Net"))
          
        { DecomposedData1()%>%select(report_date,category,input$Decomposed_Carcass_Net_Value,year,month)}
        
        
      })
      
      DecomposedData2.5<-reactive({DecomposedData2()%>%
          dplyr::group_by(.,year,month)%>%
          filter(.,row_number()==1)
        
        
      })
      
      DecomposedData3<-reactive({dplyr::filter(DecomposedData2.5(),year>=input$Decomposed_yrs[1])%>%
          dplyr::filter(.,year<=input$Decomposed_yrs[2])%>%
          dplyr::mutate(ym=as.character(as.yearmon(report_date,"%b %Y")))
        # %>%
        #   dplyr::mutate(report_date=as.character.Date(report_date,format='%m-%d-%Y'))
        
      })
   
      DecomposedData4<-reactive({ aggregate(DecomposedData3()[,3],list(DecomposedData3()$ym), FUN=mean) %>%
          mutate(date = as.yearmon(Group.1 )) %>%
          mutate(year = as.integer(year(date ))) %>%
          mutate(month =as.integer(month(date )))%>%
          arrange(-date) 
      })
  
      DecomposedData5.5<-reactive({
        df<- as.data.frame(matrix(nrow = input$Decomposed_yrs[2]-input$Decomposed_yrs[1]+1, ncol = 12)) %>%
              set_colnames(.,c("1","2","3","4","5","6","7","8","9","10","11","12"))
        max<-input$Decomposed_yrs[2]
        min<-input$Decomposed_yrs[1]
        row.names(df)<-c(min:max)
        nrow = input$Decomposed_yrs[2]-input$Decomposed_yrs[1]+1
        for(i in 1:1000){
              year = DecomposedData4()[i, 4]
              month = DecomposedData4()[i, 5]
              df[which(row.names(df)==year),which(colnames(df)==month)] = DecomposedData4() [i ,2]
            }



         return(df)

        })

      
      DecomposedData5<-reactive({
    
        ts(
          data=DecomposedData4()[,2], 
          start = c(input$Decomposed_yrs[1],1), 
          end = c(input$Decomposed_yrs[2],12), 
          frequency = 12)
        
      })
      
      
      DecomposedData6<-reactive({
        
        stl(DecomposedData5(), s.window="periodic")
        
      })

      DecomposedData6()$time.series
      
    })
    
        
  })

