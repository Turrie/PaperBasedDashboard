library(shinydashboard)

library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
library(grid)
require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
library(formattable)




### SHINY UI ###
ui <- bootstrapPage(
  # tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("superhero"), collapsible = TRUE,
             "Paper Based Tools Annex", id="nav",
             
             tabPanel("Use case mapper"
                      ,
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          #leafletOutput("mymap", width="100%", height="100%"),
                          tmapOutput(outputId = "ureaplot", width="100%", height="100%"),
                          
                          # tmapOutput(outputId = "npkplot", width="100%", height="100%"),
                          # tmapOutput(outputId = "yieldplot", width="100%", height="100%"),

                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                      

                                        span(tags$i(h6("This tool contains tables and maps with advice on application rates of urea, DAP &
                      NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer depends on soil
                      conditions and the time of planting.")), style="color:#045a8d"),
                                        pickerInput("country", "Select Country:",
                                                    choices = c("Tanzania", "Nigeria"),
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
                                        
                                        pickerInput("usecase", "Use case:",
                                                    choices = c("FR", "SPHS"),
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
                                        
                                        
                                        column(12,conditionalPanel(
                                          condition = "input.country == 'Nigeria'",
                                          pickerInput("lga_Groups", "Select State:",
                                                      choices = c("Benue_Cross River", "Delta", "Edo", "Imo", "Abia", "Akwa Ibom", "Ekiti", "Ondo", "Osun", 
                                                                  "Anambra", "Enugu", "Ebonyi", "Taraba", "Kogi", "Kwara", "Oyo", "Ogun"),
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = pickerOptions(maxOptions = 1))),
                                          
                                          conditionalPanel(
                                            condition = "input.country == 'Tanzania'",
                                            pickerInput("region", "Select region(s):", choices = c("Mtwara_Lindi", "Pwani_Tanga", "Mwanza_Shinyanga", "Mara_Simiyu",  "Kagera_Geita_Kigoma", 
                                                                                                   "Zanzibar South and Central"), 
                                                        selected = NULL,
                                                        multiple = TRUE,
                                                        options = pickerOptions(maxOptions = 1))
                                          )
                                          
                                          ),
                                        
                                        # column(12,conditionalPanel(
                                        #   condition = "input.country == 'Nigeria'",
                                        #   pickerInput("city", "City:",
                                        #               choices = c("Calabar", "Makurdi", "Asaba", "Benin City", "Owerri", "Umuahia", "Uyo", "Ado Ekiti", "Akure", 
                                        #                           "Osogbo", "Awka", "Enugu", "Ebonyi", "Taraba", "Kwara", "Oyo", "Abeokuta"),
                                        #               selected = c("Kwara"),
                                        #               multiple = FALSE)),
                                        #   
                                        #   conditionalPanel(
                                        #     condition = "input.country == 'Tanzania'",
                                        #     pickerInput("citytz", "City:", choices = c("Mtwaraacity", "Lindicity", "Tangacity", "Geita",  "Kageracity", "Kigomacity", "Maracity", 
                                        #                                                            "Mwanzacity", "Pwaniacity", "Shinyangacity", "Simiyucity",  "Zanzibarcity"), selected = c("Tangacity"),
                                        #                 multiple = TRUE)
                                        #   )),
                                        pickerInput("unit_loc", "Select unit of land:",
                                                    choices = c("acre", "ha"),
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 1)),
                                        
                                        pickerInput("FCY", "Select Your Current Yield (Tonnes):",
                                                    choices = c("3.75", "11.25", "18.75", "26.25", "33.75"),
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 1)),
                                        
                                        
                                        # pickerInput("region_tables", "Region:",
                                        #             choices = c("Mtwara_Lindi", "Pwani_Tanga", "Mwanza_Shinyanga", "Mara_Simiyu", "Kagera_Geita_Kigoma", 
                                        #                         "Zanzibar South and Central", "Zanzibar West", "Zanzibar North"),
                                        #             selected = c("Mtwara_Lindi"),
                                        #             multiple = FALSE),
                                        pickerInput("plntmth", "Planting month:",
                                                    choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September", 
                                                                "October", "November", "December"),
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 1)),
                                        # pickerInput("lga_Groups", "LGA:",
                                        #             choices = c("Benue_Cross River", "Delta", "Edo", "Imo", "Abia", "Akwa Ibom", "Ekiti", "Ondo", "Osun", 
                                        #                         "Anambra", "Enugu", "Ebonyi", "Taraba", "Kogi", "Kwara", "Oyo", "Ogun"),
                                        #             selected = c("Kwara"),
                                        #             multiple = FALSE),
                                      
                                        
                                        # pickerInput("selection", "Select map to view:",
                                        #             choices = c("NPK 15:15:15","NPK 17:17:17", "DAP", "Yield", "Urea"),
                                        #             selected = NULL,
                                        #             multiple = TRUE,
                                        #             options = pickerOptions(maxOptions = 1)),
                                        
                                        column(12,conditionalPanel(
                                          condition = "input.country == 'Nigeria'",
                                          pickerInput("selection", "Select variable to view:",
                                                      choices = c("NPK 15:15:15", "Yield", "Urea"),
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = pickerOptions(maxOptions = 1))
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.country == 'Tanzania'",
                                            pickerInput("selection2", "Select variable to view:",
                                                        choices = c("NPK 17:17:17", "DAP", "Yield", "Urea"),
                                                        selected = NULL,
                                                        multiple = TRUE,
                                                        options = pickerOptions(maxOptions = 1)),
                                          )
                                          
                                        ),
                                        
                                     
                                        
                                        uiOutput("btn_go"),
                                       
                                        
                          ),
                          
                      
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, right = 60, width = 150, fixed=TRUE, draggable = FALSE, 
                                        tags$a(href='https://akilimo.org', tags$img(src='pics/akilimo4.jpg',height = 100, width = 150,
                          id="big-heading", "Click me"))),
                          
                          # absolutePanel(div(
                          #   h2(img(height = 100, width = 150, src = "pics/akilimo4.jpg"), id="big-heading", "SCENARIO ANALYSIS FOR FERTILIZER BLENDS"),
                          #   tags$style(HTML("#big-heading{color: green;font-style: bold;}")))
                          #   
                          # ),
                          
                         

                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "100",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')",
                                                                       "https://twitter.com/ACAI_IITA")))
                          #absolutePanel(uiOutput("btn_go"))https://twitter.com/ACAI_IITA

                      ),
                      
             ),

             tabPanel("View Maps side by side",width = "100%",
                   
                      textOutput("text"),
                      br(),
                      
                      fluidRow(
                        column(6,
                               downloadButton("downloadData", "Download pdf of maps")
                               
                        ),
                        column(6,
                               downloadButton("downloadDatafr", "Download printable guide")
                        )
                      ),
                      
                     

                      fluidRow(
                        shinydashboard::box(width = 4,title = "Urea",tmapOutput(outputId = "ureaplot2", width = 450, height = 800)
                        ),
                        shinydashboard::box(width = 4,title = "NPK 15:15:15",tmapOutput(outputId = "npkplot", width = 450, height = 800)),
                        shinydashboard::box(width = 4,title = "Yield",tmapOutput(outputId = "yieldplot", width = 450, height = 800)),
                        shinydashboard::box(width = 4,title = "DAP",tmapOutput(outputId = "dapplot", width = 450, height = 800))

                      )
             ),

             
            
             tabPanel("View Table",
                    
                    #downloadButton("downloadcsv", "Download csv"),
                  
                    
                    h5('The table below specifies the recommended fertilizer application rates by LGA and month of planting, as well as the expected root yield response. '),
                    box(class = "mybg",
                        br(),
                      width = NULL, status = 'primary',
                      DT::dataTableOutput('mytable', width = "80%")
                    ),
                    
                    br(),
                    div(style="display:inline-block",downloadButton('downloadcsv', 'Download csv'), style="float:right"),   #DT::dataTableOutput("tables",width = "70%",  height = '1000px'),
                    br()
                      ),
             
             tabPanel(
               uiOutput("tabers")
             )
           
             
             
             
             # tabPanel("Download outputs",width = "100%",
             #         
             #          
             #         
             # 
             #            h4(
             #            "This tool contains tables and maps with advice on application rates of urea,
             #          NPK 15:15:15 fertilizer for cassava. Response to fertilizer depends on soil
             #          conditions and the time of planting. Tables are provided that specify the
             #          recommended fertilizer application rates by LGA and month of planting,
             #          as well as the expected root yield response. Maps are also provided
             #          to show how fertilizer rates vary across the LGAs"
             #          ),
             # 
             #          
             #          
             #                   sidebarLayout(
             #                     sidebarPanel(class = "mybg",
             #                       
             #                       span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
             #                       span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
             #                       
             #                       pickerInput("country", "Select Country:",
             #                                   choices = c("Tanzania", "Nigeria"),
             #                                   selected = NULL,
             #                                   multiple = TRUE,
             #                                   options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
             #                       
             #                       pickerInput("usecase", "Use case:",
             #                                   choices = c("FR", "SPHS"),
             #                                   selected = NULL,
             #                                   multiple = TRUE,
             #                                   options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
             #                       
             #                       pickerInput("landunit", "Unit of land:",
             #                                   choices = c("acre", "Ha"),
             #                                   selected = NULL,
             #                                   multiple = TRUE,
             #                                   options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
             #                       
             #                     ),
             #                     
             #                     mainPanel(
             #                       #downloadButton("downloadDatasp", "Download pdf"),
             #                      
             #                       
             #                     )
             #                   )
             #          )
                      
             
            
                      
             
  )          
)


