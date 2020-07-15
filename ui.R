setwd("/Users/qianyudong/Documents/R/shiny/shiny05")

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shinycssloaders)



#state state_alpha year CALF_CROP fips HEIFERS STEERS CALVES OnFeed FeederSupply   stocker



shinyUI(
  
  navbarPage(title = "Lamb",
             
             tabPanel("Historical",{
               fluidPage(
                 
                 sidebarLayout(
                   sidebarPanel(
                  
                     
                     selectInput(inputId = "Hist_time",
                                 label = "Time",
                                 choices = c("Daily"="Daily",
                                             "5-Day Average"="5-Day Average"
                                 )),
                     
                     conditionalPanel(
                       condition = "input.Hist_time == 'Daily'",
                                       selectInput(inputId = "Hist_Daily_cate",
                                                   label = "Category",
                                                   choices = c("Carcass"="Carcass",
                                                               "Cuts"="Cuts",
                                                               "Other"="Other"
                                                   ),
                                                   selected =  c("Carcass")
                                       ),
                         conditionalPanel(
                                         condition = "input.Hist_Daily_cate == 'Carcass'",
                                         selectInput(inputId = "Hist_Daily_Carcass",
                                                     label = "Carcass type",
                                                     choices = c("Gross"="Gross",
                                                                 "Foresaddle"="Foresaddle",
                                                                 "Hindsaddle"="Hindsaddle",
                                                                 "Net"="Net"
                                                     ),
                                                     selected =  c("Gross")),
                                           conditionalPanel(
                                              condition = "input.Hist_Daily_Carcass == 'Gross'",
                                              selectInput(inputId = "value_Gross",
                                                          label = "Value",
                                                          choices = c("Price"="Price",
                                                                      "Price Change"="Change",
                                                                      "Percentage"="Percentage",
                                                                      "Weight"="Weight")
                                           
                                               ))
                                         ,
                                         
                                            conditionalPanel(
                                              condition = "input.Hist_Daily_Carcass == 'Foresaddle'",
                                              selectInput(inputId = "value_Foresaddle",
                                                       label = "Value",
                                                       choices = c("Price"="Price",
                                                                   "Price Change"="Change",
                                                                   "Percentage"="Percentage",
                                                                   "Weight"="Weight")

                                              )),
                                           conditionalPanel(
                                              condition = "input.Hist_Daily_Carcass == 'Net'",
                                              selectInput(inputId = "value_Net",
                                                       label = "Value",
                                                       choices = c("Price"="Price",
                                                                   "Price Change"="Change"
                                                                   )
                                                       
                                             )),
                                            conditionalPanel(
                                              condition = "input.Hist_Daily_Carcass == 'Hindsaddle'",
                                              selectInput(inputId = "value_Hindsaddle",
                                                       label = "Value",
                                                       choices = c("Price"="Price",
                                                                   "Price Change"="Change",
                                                                   "Percentage"="Percentage",
                                                                   "Weight"="Weight")
                                                       
                                           ))
                                         
                                       
                                         
                              ),
                                      
                          conditionalPanel(
                                        condition = "input.Hist_Daily_cate == 'Cuts'",
                                        checkboxGroupInput(inputId = "Hist_Daily_Cuts",
                                                    label = "Cuts type",
                                                    choices = c(
                                                                "Breast"="BREAST **",
                                                                # "Fat and bone"="FAT and BONE",
                                                                # "Fat and Bone"="FAT and BONE",
                                                                "Flank, Untrimmed"="FLANK, UNTRIMMED **",
                                                                "Foresaddle"="FORESADDLE",
                                                                "Foreshank"="FORESHANK",
                                                                "Ground lamb"="GROUND LAMB",
                                                                "Hindsaddle"="HINDSADDLE",
                                                                "Leg, Boneless, tied"="LEG, BONELESS, TIED",
                                                                "Leg, trotter off"="LEG, TROTTER OFF",
                                                                "Loins, trimmed 1X1"="LOINS, TRIMMED 1X1",
                                                                "Loins, trimmed 4X4"="LOINS, TRIMMED 4X4",
                                                                "Neck"="NECK **",
                                                                "Rack, 8-rib medium"= "RACK, 8-RIB MEDIUM",
                                                                "Rack, Roast-ready, frenched"="RACK, ROAST-READY, FRENCHED",
                                                                "Shoulders, SQ-cut"="SHOULDERS, SQ-CUT",
                                                                "Shrink"="SHRINK"           
                                                                #"Shrink"="SHRINK",
                                                               
                                                                
                                                    ),
                                                    selected =  c("FORESADDLE","RACK, 8-RIB MEDIUM","RACK, ROAST-READY, FRENCHED")),
                                        selectInput(inputId = "Hist_Daily_Cuts_Value",
                                                    label = "Value",
                                                    choices = c("FOB Price"="fob_price",
                                                                "Price Change"="price_change_from_last",
                                                                "Percentage"="percentage_carcass",
                                                                "Weight"="cut_weight"
                                                                )
                                                    
                                        )
                                      ),
                     #  report_date processing_cost shrink_value ls711_weight ls711_date weight_difference  
                                      conditionalPanel(
                                        condition = "input.Hist_Daily_cate == 'Other'",
                                        radioButtons(inputId = "Hist_Daily_Other_Value",
                                                    label = "Value",
                                                    choices = c("Processing Cost"="processing_cost",
                                                                "Shrink and Trim"="shrink_value",
                                                                "Carcass Weight"="ls711_weight",
                                                               # "Carcass Weight Date"= "Carcass Weight Date",
                                                                "Weight Difference"="weight_difference"
                                                    )       
                                        )
                                      )
                  )
                  
                ,
                dateRangeInput(inputId = "Hist_days",
                               label="Range of days",
                               min = Sys.Date()-3650,
                               max = Sys.Date(),
                               start = Sys.Date()-30,
                               end = Sys.Date(),sep="")
                  
               
                  
                  
                     
                 
                     
 
                     # uiOutput("Hist_time_ui")
             
                     
                     
                    ),
                   
                   
                   mainPanel(   tabsetPanel(type = "tabs",
                                            tabPanel("Plot",
                                                     #  withSpinner(),
                                                     plotOutput("Hist_Plot")%>% withSpinner(type = 4),
                                                     fluidRow(  downloadButton('download_Hist_Plot', 'download plot'))),
                                            tabPanel("Data",
                                                     div(style='height:400px; overflow-y:scroll;width:600px',
                                                         tableOutput("Hist_Data"))
                                                     , fluidRow(  downloadButton('download_Hist_Data', 'download data'))),
                                            tabPanel("App Instruction",h4("text here")))
                                
                   )
                 )
               )
             })
             ,
             tabPanel("Seasonal",{
               fluidPage(sidebarLayout(
                 sidebarPanel(

          
                   selectInput(inputId = "Seasonal_time",
                               label = "Time",
                               choices = c("Daily"="Daily",
                                           "5-Day Average"="5-Day Average"
                               )),

                   conditionalPanel(
                     condition = "input.Seasonal_time == 'Daily'",
                     selectInput(inputId = "Seasonal_Daily_cate",
                                 label = "Category",
                                 choices = c("Carcass"="Carcass",
                                             "Cuts"="Cuts",
                                             "Other"="Other"
                                 ),
                                 selected =  c("Carcass")
                     ),
                     conditionalPanel(
                       condition = "input.Seasonal_Daily_cate == 'Carcass'",
                       selectInput(inputId = "Seasonal_Daily_Carcass",
                                   label = "Carcass type",
                                   choices = c("Gross"="Gross",
                                               "Foresaddle"="Foresaddle",
                                               "Hindsaddle"="Hindsaddle",
                                               "Net"="Net"
                                   ),
                                   selected =  c("Gross")),
                       conditionalPanel(
                         condition = "input.Seasonal_Daily_Carcass == 'Gross'",
                         selectInput(inputId = "Seasonal_Carcass_Gross_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")

                         )

                       ),
                       conditionalPanel(
                         condition = "input.Seasonal_Daily_Carcass == 'Foresaddle'",
                         selectInput(inputId = "Seasonal_Carcass_Foresaddle_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")

                         )

                       ),
                       conditionalPanel(
                         condition = "input.Seasonal_Daily_Carcass =='Net'",
                         selectInput(inputId = "Seasonal_Carcass_Net_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change"
                                     )

                         )

                       ),
                       conditionalPanel(
                         condition = "input.Seasonal_Daily_Carcass == 'Hindsaddle'",
                         selectInput(inputId = "Seasonal_Carcass_Hindsaddle_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")
                                     

                         )

                       )
                     ),

                     conditionalPanel(
                       condition = "input.Seasonal_Daily_cate == 'Cuts'",
                       checkboxGroupInput(inputId = "Seasonal_Daily_Cuts",
                                          label = "Cuts type",
                                          choices = c("Breast"="BREAST **",
                                                      # "Fat and bone"="FAT and BONE",
                                                      # "Fat and Bone"="FAT and BONE",
                                                      "Flank, Untrimmed **"="FLANK, UNTRIMMED **",
                                                      "Foresaddle"="FORESADDLE",
                                                      "Foreshank"="FORESHANK",
                                                      "Ground lamb"="GROUND LAMB",
                                                      "Hindsaddle"="HINDSADDLE",
                                                      "Leg, Boneless, tied"="LEG, BONELESS, TIED",
                                                      "Leg, trotter off"="LEG, TROTTER OFF",
                                                      "Loins, trimmed 1X1"="LOINS, TRIMMED 1X1",
                                                      "Loins, trimmed 4X4"="LOINS, TRIMMED 4X4",
                                                      "Neck"="NECK **",
                                                      "Rack, 8-rib medium"= "RACK, 8-RIB MEDIUM",
                                                      "Rack, Roast-ready, frenched"="RACK, ROAST-READY, FRENCHED",
                                                      "Shoulders, SQ-cut"="SHOULDERS, SQ-CUT",
                                                      "Shrink"="SHRINK"           
                                                      #"Shrink"="SHRINK",
                                          ),
                                          selected =  c("FORESADDLE","RACK, 8-RIB MEDIUM","RACK, ROAST-READY, FRENCHED")),
                       selectInput(inputId = "Seasonal_Daily_Cuts_Value",
                                   label = "Value",
                                   choices = c("FOB Price"="fob_price",
                                               "Price Change"="price_change_from_last",
                                               "Percentage"="percentage_carcass",
                                               "Weight"="cut_weight"
                                   )

                       )
                     ),

                     
                     # 
                     # conditionalPanel(
                     #   condition = "input.Hist_Daily_cate == 'Other'",
                     #   radioButtons(inputId = "Hist_Daily_Other_Value",
                     #                label = "Value",
                     #                choices = c("Processing Cost"="processing_cost",
                     #                            "Shrink and Trim"="shrink_value",
                     #                            "Carcass Weight"="ls711_weight",
                     #                            # "Carcass Weight Date"= "Carcass Weight Date",
                     #                            "Weight Difference"="weight_difference"
                     # 
                     
                     conditionalPanel(
                       condition = "input.Seasonal_Daily_cate == 'Other'",
                       radioButtons(inputId = "Seasonal_Daily_Other_Value",
                                    label = "Value",
                                    choices = c("Processing Cost"="processing_cost",
                                                "Shrink and Trim"="shrink_value",
                                                "Carcass Weight"="ls711_weight",
                                                "Weight Difference"="weight_difference"
                                    )
                       )
                     )
                   )
                   ,
                   sliderInput(inputId = "Seasonal_yrs",
                               label="Range of years",
                               min = round(year(now())-20),
                               max = round(year(now())),
                               value = c(round(year(now())-3),round(year(now()))),sep=""),
                   
                   checkboxGroupInput(inputId = "Seasonal_rd",
                                      label="Years averages",
                                      choices=c(3,5,10),
                                      selected=3)
    



                 ),

                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("Plot",
                                        plotOutput("Seasonal_Plot")
                                        ,fluidRow(  downloadButton('download_Seasonal_Plot', 'download plot'))),
                               tabPanel("Data",
                                        div(style='height:400px; overflow-y:scroll;width:600px',
                                            tableOutput("Seasonal_Data"))
                                        , fluidRow( downloadButton('download_Seasonal_Data', 'download data'))),
                               tabPanel("App Instruction",h4("text here")))
                 )
               ))

             })
             ,
             tabPanel("Seasonal subseries ",{
               fluidPage(sidebarLayout(
                 sidebarPanel(
               


                   selectInput(inputId = "Seasonal_sub_time",
                               label = "Time",
                               choices = c("Daily"="Daily",
                                           "5-Day Average"="5-Day Average"
                               )),

                   conditionalPanel(
                     condition = "input.Seasonal_sub_time == 'Daily'",
                     selectInput(inputId = "Seasonal_sub_Daily_cate",
                                 label = "Category",
                                 choices = c("Carcass"="Carcass",
                                             "Cuts"="Cuts",
                                             "Other"="Other"
                                 ),
                                 selected =  c("Carcass")
                                 ),
                     
                     conditionalPanel(
                       condition = "input.Seasonal_sub_Daily_cate == 'Carcass'",
                       selectInput(inputId = "Seasonal_sub_Daily_Carcass",
                                   label = "Carcass type",
                                   choices = c("Gross"="Gross",
                                               "Foresaddle"="Foresaddle",
                                               "Hindsaddle"="Hindsaddle",
                                               "Net"="Net"
                                   ),
                                   selected =  c("Gross")),
                       conditionalPanel(
                         condition = "input.Seasonal_sub_Daily_Carcass == 'Gross'",
                         selectInput(inputId = "Seasonal_sub_Carcass_Gross_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")

                         )

                       ),
                       conditionalPanel(
                         condition = "input.Seasonal_sub_Daily_Carcass == 'Foresaddle'",
                         selectInput(inputId = "Seasonal_sub_Carcass_Foresaddle_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")

                         )

                       ),
                       conditionalPanel(
                         condition = "input.Seasonal_sub_Daily_Carcass == 'Net'",
                         selectInput(inputId = "Seasonal_sub_Carcass_Net_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change"
                                     )

                         )

                       ),
                       conditionalPanel(
                         condition = "input.Seasonal_sub_Daily_Carcass == 'Hindsaddle'",
                         selectInput(inputId = "Seasonal_sub_Carcass_Hindsaddle_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")

                         )

                       )
                     ),

                     conditionalPanel(
                       condition = "input.Seasonal_sub_Daily_cate == 'Cuts'",
                       checkboxGroupInput(inputId = "Seasonal_sub_Daily_Cuts",
                                          label = "Cuts type",
                                          choices = c("Breast"="BREAST **",
                                                      # "Fat and bone"="FAT and BONE",
                                                      # "Fat and Bone"="FAT and BONE",
                                                      "Flank, Untrimmed **"="FLANK, UNTRIMMED **",
                                                      "Foresaddle"="FORESADDLE",
                                                      "Foreshank"="FORESHANK",
                                                      "Ground lamb"="GROUND LAMB",
                                                      "Hindsaddle"="HINDSADDLE",
                                                      "Leg, Boneless, tied"="LEG, BONELESS, TIED",
                                                      "Leg, trotter off"="LEG, TROTTER OFF",
                                                      "Loins, trimmed 1X1"="LOINS, TRIMMED 1X1",
                                                      "Loins, trimmed 4X4"="LOINS, TRIMMED 4X4",
                                                      "Neck"="NECK **",
                                                      "Rack, 8-rib medium"= "RACK, 8-RIB MEDIUM",
                                                      "Rack, Roast-ready, frenched"="RACK, ROAST-READY, FRENCHED",
                                                      "Shoulders, SQ-cut"="SHOULDERS, SQ-CUT",
                                                      "Shrink"="SHRINK"           
                                                      #"Shrink"="SHRINK",
                                          ),
                                          selected =  c("FORESADDLE","RACK, 8-RIB MEDIUM","RACK, ROAST-READY, FRENCHED")),
                       selectInput(inputId = "Seasonal_sub_Daily_Cuts_Value",
                                   label = "Value",
                                   choices = c("FOB Price"="fob_price",
                                               "Price Change"="price_change_from_last",
                                               "Percentage"="percentage_carcass",
                                               "Weight"="cut_weight"
                                   )

                       )
                     ),

                     conditionalPanel(
                       condition = "input.Seasonal_sub_Daily_cate == 'Other'",
                       radioButtons(inputId = "Seasonal_sub_Daily_Other_Value",
                                    label = "Value",
                                    choices = c("Processing Cost"="processing_cost",
                                                "Shrink and Trim"="shrink_value",
                                                "Carcass Weight"="ls711_weight",
                                                # "Carcass Weight Date"= "Carcass Weight Date",
                                                "Weight Difference"="weight_difference"
                                    )
                       )
                     )
                   ),
                   
                   sliderInput(inputId = "Seasonal_sub_yrs",
                               label="Range of years",
                               min = round(year(now())-20),
                               max = round(year(now())),
                               value = c(round(year(now())-3),round(year(now()))),sep="")



                 ),

                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("Plot",
                                        plotOutput("Seasonal_sub_Plot")
                                        ,fluidRow(  downloadButton('download_Seasonal_sub_Plot', 'download plot'))),
                               tabPanel("Data",
                                        div(style='height:400px; overflow-y:scroll;width:600px',
                                            tableOutput("Seasonal_sub_Data"))
                                        , fluidRow( downloadButton('download_Seasonal_sub_Data', 'download data'))),
                               tabPanel("App Instruction",h4("text here")))
                 )
               ))

             })
             ,
             tabPanel("Decomposed",{
               fluidPage(sidebarLayout(
                 sidebarPanel(
                 
                   
                   
                   selectInput(inputId = "Decomposed_time",
                               label = "Time",
                               choices = c("Daily"="Daily",
                                           "5-Day Average"="5-Day Average"
                               )),
                   
                   conditionalPanel(
                     condition = "input.Decomposed_time == 'Daily'",
                     selectInput(inputId = "Decomposed_Daily_cate",
                                 label = "Category",
                                 choices = c("Carcass"="Carcass",
                                             "Cuts"="Cuts",
                                             "Other"="Other"
                                 ),
                                 selected =  c("Carcass")
                     ),
                     
                     conditionalPanel(
                       condition = "input.Decomposed_Daily_cate == 'Carcass'",
                       selectInput(inputId = "Decomposed_Daily_Carcass",
                                   label = "Carcass type",
                                   choices = c("Gross"="Gross",
                                               "Foresaddle"="Foresaddle",
                                               "Hindsaddle"="Hindsaddle",
                                               "Net"="Net"
                                   ),
                                   selected =  c("Gross")),
                       conditionalPanel(
                         condition = "input.Decomposed_Daily_Carcass== 'Gross'",
                         selectInput(inputId = "Decomposed_Carcass_Gross_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")
                                     
                         )
                         
                       ),
                       conditionalPanel(
                         condition = "input.Decomposed_Daily_Carcass == 'Foresaddle'",
                         selectInput(inputId = "Decomposed_Carcass_Foresaddle_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")
                                     
                         )
                         
                       ),
                       conditionalPanel(
                         condition = "input.Decomposed_Daily_Carcass == 'Net'",
                         selectInput(inputId = "Decomposed_Carcass_Net_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change"
                                     )
                                     
                         )
                         
                       ),
                       conditionalPanel(
                         condition = "input.Decomposed_Daily_Carcass == 'Hindsaddle'",
                         selectInput(inputId = "Decomposed_Carcass_Hindsaddle_Value",
                                     label = "Value",
                                     choices = c("Price"="Price",
                                                 "Change"="Change",
                                                 "Percentage"="Percentage",
                                                 "Weight"="Weight")
                                     
                         )
                         
                       )
                     ),
                     
                     conditionalPanel(
                       condition = "input.Decomposed_Daily_cate == 'Cuts'",
                       checkboxGroupInput(inputId = "Decomposed_Daily_Cuts",
                                          label = "Cuts type",
                                          choices = c("Breast"="BREAST **",
                                                      # "Fat and bone"="FAT and BONE",
                                                      # "Fat and Bone"="FAT and BONE",
                                                      "Flank, Untrimmed **"="FLANK, UNTRIMMED **",
                                                      "Foresaddle"="FORESADDLE",
                                                      "Foreshank"="FORESHANK",
                                                      "Ground lamb"="GROUND LAMB",
                                                      "Hindsaddle"="HINDSADDLE",
                                                      "Leg, Boneless, tied"="LEG, BONELESS, TIED",
                                                      "Leg, trotter off"="LEG, TROTTER OFF",
                                                      "Loins, trimmed 1X1"="LOINS, TRIMMED 1X1",
                                                      "Loins, trimmed 4X4"="LOINS, TRIMMED 4X4",
                                                      "Neck"="NECK **",
                                                      "Rack, 8-rib medium"= "RACK, 8-RIB MEDIUM",
                                                      "Rack, Roast-ready, frenched"="RACK, ROAST-READY, FRENCHED",
                                                      "Shoulders, SQ-cut"="SHOULDERS, SQ-CUT",
                                                      "Shrink"="SHRINK"           
                                                      #"Shrink"="SHRINK",
                                          ),
                                          selected =  c("FORESADDLE","RACK, 8-RIB MEDIUM","RACK, ROAST-READY, FRENCHED")),
                       selectInput(inputId = "Decomposed_Daily_Cuts_Value",
                                   label = "Value",
                                   choices = c("FOB Price"="fob_price",
                                               "Price Change"="price_change_from_last",
                                               "Percentage"="percentage_carcass",
                                               "Weight"="cut_weight"
                                   )
                                   
                       )
                     ),
                     
                     conditionalPanel(
                       condition = "input.Decomposed_Daily_cate == 'Other'",
                       radioButtons(inputId = "Decomposed_Daily_Other_Value",
                                    label = "Value",
                                    choices = c("Processing Cost"="processing_cost",
                                                "Shrink and Trim"="shrink_value",
                                                "Carcass Weight"="ls711_weight",
                                                # "Carcass Weight Date"= "Carcass Weight Date",
                                                "Weight Difference"="weight_difference"
                                    )
                       )
                     )
                   ),
                   
                   sliderInput(inputId = "Decomposed_yrs",
                               label="Range of years",
                               min = round(year(now())-20),
                               max = round(year(now())-1),
                               value = c(round(year(now())-3),round(year(now())-1)),sep="")
                   
                   
                   
                   
                   
                   
                   
                 ),
                 
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("Plot",
                                        plotOutput("Decomposed_Plot")
                                        ,fluidRow(  downloadButton('download_Decomposed_Plot', 'download plot'))),
                               tabPanel("Data",
                                        div(style='height:400px; overflow-y:scroll;width:600px',
                                            tableOutput("Decomposed_Data"))
                                        , fluidRow( downloadButton('download_Decomposed_Data', 'download data'))),
                               tabPanel("App Instruction",h4("text here")))
                 )
               ))
               
             })
             
             
  )
)




