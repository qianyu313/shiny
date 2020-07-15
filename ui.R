setwd("/Users/qianyudong/Documents/R/shiny/shiny05")

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinycssloaders)



#state state_alpha year CALF_CROP fips HEIFERS STEERS CALVES OnFeed FeederSupply   stocker



shinyUI(
  navbarPage(title = "Stocker APP",
             
             tabPanel("Spatial at State Level",{
               fluidPage(
                 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId = "State_category",
                                 label = "Category",
                                 choices = c("Stocker index"="STOCKER INDEX",
                                             "Calf crop"="CALF CROP",
                                             "Feeder supply"="FEEDER SUPPLY",
                                             "Heifer retention"="HEIFER RETENTION"
                                             
                                 )),
                     
                     selectInput(inputId="State_Year_Type",
                                 label = "Subcategory",
                                 choices=c("Single year", 
                                           "Nominal change between two years",
                                           "Percent change between two years")
                     ),
                     
                     uiOutput("State_ui") 
                     
                   ),
                   
                   
                   mainPanel(   tabsetPanel(type = "tabs",
                                            tabPanel("Plot",
                                                     #  withSpinner(),
                                                     plotOutput("State_Plot")%>% withSpinner(type = 4),
                                                     fluidRow(  downloadButton('download_State_Plot', 'download plot'))),
                                            tabPanel("Data",
                                                     div(style='height:400px; overflow-y:scroll;width:600px',
                                                         tableOutput("State_Data"))
                                                     , fluidRow(  downloadButton('download_State_Data', 'download data'))),
                                            tabPanel("App Instruction",h4("text here")))
                                
                   )
                 )
               )
             })
             , 
             
             tabPanel("Bar Chart",{
               fluidPage(sidebarLayout(
                 sidebarPanel( 
                   fluidRow(column(12,
                                   selectInput(inputId="Bar_category", 
                                               label="Category",
                                               c( "Stocker index"="STOCKER INDEX",
                                                  "Calf crop"="CALF CROP (1000 Head)",
                                                  "Feeder supply"="FEEDER SUPPLY (1000 Head)",
                                                  "Heifer retention"="HEIFER RETENTION"
                                               )))),
                   
                   fluidRow(column(12,
                                   selectInput(inputId="Bar_sub_category", 
                                               label="Location",
                                               c( "Region"="Region",
                                                  "State"="State"
                                               )))),
                   
                   # This outputs the dynamic UI component
                   fluidRow(column(12,
                                   uiOutput("Bar_ui")     
                   )),
                   
                   fluidRow(column(12,  selectInput(inputId = "Bar_year",
                                                    label = "Year",
                                                    choices = c("1997","1998","1999","2000","2001","2002","2003","2004","2005",
                                                                "2006","2007","2008","2009","2010","2011","2012","2013","2014",
                                                                "2015","2016","2017","2018","2019","2020" )
                                                    ,selected= 2020
                   )
                   ))
                   
                   , width = 4),
                 
                 mainPanel(   
                   tabsetPanel(type = "tabs",
                               tabPanel("Plot",
                                        plotOutput("Bar_Plot")
                                        ,fluidRow(  downloadButton('download_Bar_Plot', 'download plot'))),
                               tabPanel("Data", 
                                        div(style='height:400px; overflow-y:scroll;width:600px',
                                            tableOutput("Bar_Data"))
                                        , fluidRow( downloadButton('download_Bar_Data', 'download data'))),
                               tabPanel("App Instruction",h4("text here")))
                 )     
               ))
               
             })
             ,
             
             tabPanel("Line Chart",{
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId = "Line_category",
                                 label = "Category",
                                 choices = c(
                                   "Stocker index"="STOCKER INDEX",
                                   "Calf crop"="CALF CROP (1000 Head)",
                                   "Feeder supply"="FEEDER SUPPLY (1000 Head)",
                                   "Heifer retention"="HEIFER RETENTION"               
                                 )),
                     
                     
                     fluidRow(column(12,
                                     selectInput(inputId="Line_sub_category", 
                                                 label="Location",
                                                 c( "Region"="Region",
                                                    "State"="State"
                                                 )))),
                     
                     # This outputs the dynamic UI component
                     fluidRow(column(12,
                                     uiOutput("Line_ui")     
                     )),
                     
                     fluidRow(column(12,   sliderInput(inputId = "Line_year",
                                                       label = "Year",
                                                       min = 1980,
                                                       max = 2020,
                                                       value = c(2009,2019),sep="")
                     ))),
                   
                   mainPanel(   tabsetPanel(type = "tabs",
                                            tabPanel("Plot",
                                                     #  withSpinner(),
                                                     plotOutput("Line_Plot"),
                                                     fluidRow(  downloadButton('download_Line_Plot', 'download plot'))),
                                            tabPanel("Data",
                                                     div(style='height:400px; overflow-y:scroll;width:600px',
                                                         tableOutput("Line_Data"))
                                                     , fluidRow(  downloadButton('download_Line_Data', 'download data'))),
                                            tabPanel("App Instruction",h4("text here")))        
                   )
                 )
               )
             })    
  )
)




