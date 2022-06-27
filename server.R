
#setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard")

library(dplyr)
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

library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
library(formattable)


# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
#######################################################################################
## Read the GIS layers
#######################################################################################

TownsNG <- readOGR(dsn = ".", layer = "Places_towns")
RiversNG <- readOGR(dsn = ".", layer = "Rivers")


boundaryNG <- readOGR(dsn=getwd(), layer="gadm36_NGA_1")
ngstate <- readOGR(dsn=getwd(), layer="gadm36_NGA_2")


boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")
tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")


###################################################################################################
## NG fertilizer recom for FCY 1:5
###################################################################################################
FR_NG_FCY1 <- readRDS("FRrecom_lga_level1_NG_2020.RDS")
FR_NG_FCY2 <- readRDS("FRrecom_lga_level2_NG_2020.RDS")
FR_NG_FCY3 <- readRDS("FRrecom_lga_level3_NG_2020.RDS")
FR_NG_FCY4 <- readRDS("FRrecom_lga_level4_NG_2020.RDS")
FR_NG_FCY5 <- readRDS("FRrecom_lga_level5_NG_2020.RDS")


###########################################################################
##  TZ fertilizer recom for FCY 1:5
###########################################################################
FR_TZ_FCY1 <- readRDS("FRrecom_lga_level1_TZ_2020.RDS")
FR_TZ_FCY2 <- readRDS("FRrecom_lga_level2_TZ_2020.RDS")
FR_TZ_FCY3 <- readRDS("FRrecom_lga_level3_TZ_2020.RDS")
FR_TZ_FCY4 <- readRDS("FRrecom_lga_level4_TZ_2020.RDS")
FR_TZ_FCY5 <- readRDS("FRrecom_lga_level5_TZ_2020.RDS")


###########################################################################
##  adding planting month
###########################################################################
addplm <- function(ds, country){
  ds$respY <- ds$TargetY - ds$CurrentY
  ds$groRev <- ds$NR + ds$TC
  ds$plm <- as.factor(ds$plw)
  levels(ds$plm)[levels(ds$plm) %in% 1:4]   <- "January"
  levels(ds$plm)[levels(ds$plm) %in% 5:8]   <- "February"
  levels(ds$plm)[levels(ds$plm) %in% 9:13]  <- "March"
  levels(ds$plm)[levels(ds$plm) %in% 14:17] <- "April"
  levels(ds$plm)[levels(ds$plm) %in% 18:22] <- "May"
  levels(ds$plm)[levels(ds$plm) %in% 23:26] <- "June"
  levels(ds$plm)[levels(ds$plm) %in% 27:30] <- "July"
  levels(ds$plm)[levels(ds$plm) %in% 31:35] <- "August"
  levels(ds$plm)[levels(ds$plm) %in% 36:39] <- "September"
  levels(ds$plm)[levels(ds$plm) %in% 40:43] <- "October"
  levels(ds$plm)[levels(ds$plm) %in% 44:48] <- "November"
  levels(ds$plm)[levels(ds$plm) %in% 49:53] <- "December"
  if(country=="NG"){
    ds$rateUrea <- ds$urea
    ds$rateNPK151515 <- ds$NPK15_15_15
  }else{
    ds$rateUrea <- ds$urea
    ds$rateNPK171717 <- ds$NPK17_17_17
    ds$rateDAP <- ds$DAP
  }
  return(ds)
}


FR_NG_FCY1_plm <- addplm(ds=FR_NG_FCY1, country = "NG") ## NG if user current yield is level 1
FR_NG_FCY2_plm <- addplm(ds=FR_NG_FCY2, country = "NG") ## NG if user current yield is level 2
FR_NG_FCY3_plm <- addplm(ds=FR_NG_FCY3, country = "NG") ## NG if user current yield is level 3
FR_NG_FCY4_plm <- addplm(ds=FR_NG_FCY4, country = "NG") ## NG if user current yield is level 4
FR_NG_FCY5_plm <- addplm(ds=FR_NG_FCY5, country = "NG") ## NG if user current yield is level 5


FR_TZ_FCY1_plm <- addplm(ds=FR_TZ_FCY1, country = "TZ") ## TZ if user current yield is level 1
FR_TZ_FCY2_plm <- addplm(ds=FR_TZ_FCY2, country = "TZ") ## TZ if user current yield is level 2
FR_TZ_FCY3_plm <- addplm(ds=FR_TZ_FCY3, country = "TZ") ## TZ if user current yield is level 3
FR_TZ_FCY4_plm <- addplm(ds=FR_TZ_FCY4, country = "TZ") ## TZ if user current yield is level 4
FR_TZ_FCY5_plm <- addplm(ds=FR_TZ_FCY5, country = "TZ") ## TZ if user current yield is level 5


###########################################################################
## select FCY and read the corresponding file 
## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
###########################################################################


### SHINY SERVER ###

server = function(input, output, session) {
  
  
  #state <- droplevels(ds[ds$STATE == input$state, ])
  
  
  output$btn_go <- renderUI({
    actionButton("btn_go", "Get Maps & Tables", icon("map"),
                 style="color: #fff; background-color: black; border-color: #2e6da4")
    
  })
  
  observeEvent(input$btn_go, {
    country <- input$country
    FCY <- as.numeric(input$FCY)
    Selection <- input$selection
    Selection2 <- input$selection2
    usecase <- input$usecase
    #state <- input$state
    if(country == 'Nigeria'){
      if(FCY == 11.25){
        ds <- FR_NG_FCY2_plm
        
      }else if(FCY == 3.75){
        ds <- FR_NG_FCY1_plm
      }else if(FCY == 18.75){
        ds <- FR_NG_FCY3_plm
      }else if(FCY == 26.25){
        ds <- FR_NG_FCY4_plm
      }else if(FCY == 33.75){
        ds <- FR_NG_FCY5_plm
      }
    } else if (country == 'Tanzania'){
      if(FCY == 11.25){
        ds=FR_TZ_FCY2_plm
      } else if(FCY == 3.75){
        ds=FR_TZ_FCY1_plm
      }else if(FCY == 18.75){
        ds=FR_TZ_FCY3_plm
      }else if(FCY == 26.25){
        ds <- FR_TZ_FCY4_plm
      }else if(FCY == 33.75){
        ds <- FR_TZ_FCY5_plm
      }
    }
  
    
      if(FCY < 7.5){
        yield_level = "a low yield level"
        
      }else if(FCY > 7.5 & FCY < 15 ){
        yield_level ="an average yield level"
      }else if(FCY > 15 & FCY < 22.5 ){
        yield_level ="a moderately high yield level"
      }else if(FCY > 22.5 & FCY < 30 ){
        yield_level = "a high yield level"
      } else if(FCY > 30 ){
        yield_level = "a very high yield level"
      }
    
    if(country == 'Nigeria'){
    lgaGroups <- input$lga_Groups
    }else if (country == "Tanzania"){
      lgaGroups <- input$region
    }
    
    if(country == 'Nigeria'){
      lgaGroups2 <- input$lga_Groups
    }else if (country == "Tanzania"){
      lgaGroups2 <- input$region
    }
    
    plantMonth <- input$plntmth
    cities <- input$city
    unit <- input$unit_loc
    
    # output$tabers<-renderUI({
    #   if(country == 'Nigeria') {
    #     tabsetPanel(
    #       id="tabC",
    #       type = "tabs",
    #       tabPanel("Download outputs",width = "100%",
    # 
    # 
    # 
    # 
    #                  h4(
    #                  "This tool contains tables and maps with advice on application rates of urea,
    #                NPK 15:15:15 fertilizer for cassava. Response to fertilizer depends on soil
    #                conditions and the time of planting. Tables are provided that specify the
    #                recommended fertilizer application rates by LGA and month of planting,
    #                as well as the expected root yield response. Maps are also provided
    #                to show how fertilizer rates vary across the LGAs"
    #                ),
    # 
    # 
    # 
    #                         sidebarLayout(
    #                           sidebarPanel(class = "mybg",
    # 
    #                             span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
    #                             span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
    # 
    #                             pickerInput("country", "Select Country:",
    #                                         choices = c("Tanzania", "Nigeria"),
    #                                         selected = NULL,
    #                                         multiple = TRUE,
    #                                         options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
    # 
    #                             pickerInput("usecase", "Use case:",
    #                                         choices = c("FR", "SPHS"),
    #                                         selected = NULL,
    #                                         multiple = TRUE,
    #                                         options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
    # 
    #                             pickerInput("landunit", "Unit of land:",
    #                                         choices = c("acre", "Ha"),
    #                                         selected = NULL,
    #                                         multiple = TRUE,
    #                                         options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
    # 
    #                           ),
    # 
    #                           mainPanel(
    #                             #downloadButton("downloadDatasp", "Download pdf"),
    # 
    # 
    #                           )
    #                         )
    #                ),
    #       tabPanel("Performance")
    #     )
    #   }
    #   else if(country == 'Tanzania'){
    #     tabsetPanel(
    #       id="tabA",
    #       type = "tabs",
    #       tabPanel("Constituents"),
    #       tabPanel("Clusters" ),
    #       tabPanel("Index")
    #     )
    #   }
    # })
    
    if(country == 'Nigeria'){
   
      Oyo <- droplevels(ds[ds$STATE == "Oyo", ])
      Oyolabel <- data.frame(state= c("Oyo"), lon=c(3.3), lat=c(9))
      
      Ogun <- droplevels(ds[ds$STATE == "Ogun", ])
      Ogunlabel <- data.frame(state= c("Ogun"), lon=c(3.4), lat=c(7.65))
      
      Kogi <- droplevels(ds[ds$STATE == "Kogi", ])
      Kogilabel <- data.frame(state= c("Kogi"), lon=c(6.63), lat=c(8.56))
      
      Kwara <- droplevels(ds[ds$STATE %in% c("Kwara"), ])
      Kwaralabel <- data.frame(state= c( "Kwara"), lon=c(4.9), lat=c(9.5))
      
      Taraba <- droplevels(ds[ds$STATE %in% c("Taraba"), ])
      Tarabalabel <- data.frame(state= c( "Taraba"), lon=c(10.2), lat=c(9.05))
      
      Benue_CR <- droplevels(ds[ds$STATE %in% c("Benue","Cross River"), ])
      Benue_CRlabel <- data.frame(state= c("Benue", "Cross River"), lon=c(8, 9.5), lat=c(8.2, 5.8))
      
      Edo <- droplevels(ds[ds$STATE %in% c("Edo"), ])
      Edolabel <- data.frame(state= c("Edo"), lon=c(5.3), lat=c(7))
      
      Delta_Edo <- droplevels(ds[ds$STATE %in% c("Delta","Edo"), ])
      Delta_Edolabel <- data.frame(state= c( "Delta","Edo"), lon=c(6, 5.3), lat=c(5,7))
      
      Akwa_Ibom <- droplevels(ds[ds$STATE %in% c("Akwa Ibom"), ])
      Akwa_Ibomlabel <- data.frame(state= c( "Akwa Ibom"), lon=c(8), lat=c(5.45))
      
      Imo <- droplevels(ds[ds$STATE %in% c("Imo"), ])
      Imolabel <- data.frame(state= c( "Imo"), lon=c(6.9), lat=c(5.9))
      
      Abia <- droplevels(ds[ds$STATE %in% c("Abia"), ])
      Abialabel <- data.frame(state= c( "Abia"), lon=c(7.7), lat=c(5.9))
      
      AkwaIbom_Imo_Abia <- droplevels(ds[ds$STATE %in% c("Akwa Ibom", "Imo", "Abia"), ])
      AkwaIbom_Imo_AbiAlabel <- data.frame(state= c( "Akwa Ibom", "Imo", "Abia"), lon=c(7.3, 7, 7.8), lat=c(4.4, 6, 6))
      
      Ondo <- droplevels(ds[ds$STATE %in% c("Ondo"), ])
      Ondolabel <- data.frame(state= c( "Ondo"), lon=c(5.3), lat=c(6.6))
      
      Ekiti <- droplevels(ds[ds$STATE %in% c("Ekiti"), ])
      Ekitilabel <- data.frame(state= c( "Ekiti"), lon=c(5.3), lat=c(8.1))
      
      Osun <- droplevels(ds[ds$STATE == "Osun", ])
      Osunlabel <- data.frame(state= c("Osun"), lon=c(4.2), lat=c(8.05))
      
      Ekiti_Ondo_Osun <- droplevels(ds[ds$STATE %in% c("Ekiti", "Ondo", "Osun"), ])
      Ekitil_Ondo_Osunabel <- data.frame(state= c( "Ekiti", "Ondo", "Osun"), lon=c(5.3, 5.3, 4.2), lat=c(8.1, 6.6, 8.05))
      
      Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
      Anambralabel <- data.frame(state= c( "Anambra"), lon=c(7.15), lat=c(6.45))
      
      Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
      Ebonyilabel <- data.frame(state= c( "Ebonyi"), lon=c(7.83), lat=c(6.67))
      
      Enugu <- droplevels(ds[ds$STATE %in% c("Enugu"), ])
      Enugulabel <- data.frame(state= c( "Enugu"), lon=c(7.1), lat=c(6.95))
      
      Anambra_Enugu_Ebonyi <- droplevels(ds[ds$STATE %in% c("Anambra", "Enugu", "Ebonyi"), ])
      Anambra_Enugu_Ebonyilabel <- data.frame(state= c( "Anambra", "Enugu", "Ebonyi"), lon=c(6.7, 7, 8.25), lat=c(5.9, 7.1, 6.9))
      
      
      if(lgaGroups == "Benue_Cross River"){
        LGApoints <- Benue_CR 
        stateLabel <- Benue_CRlabel
        textangle <- 0 
        couple = "Two"
      }else if(lgaGroups =="Delta"){
        LGApoints <- Delta 
        stateLabel <- Deltalabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Edo"){
        LGApoints  <-  Edo 
        stateLabel  <-  Edolabel
        textangle <- 0 
        couple  <-  "One"
      }  else if(lgaGroups == "Imo"){
        LGApoints  <-  Imo 
        stateLabel  <-  Imolabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Abia"){
        LGApoints  <-  Abia 
        stateLabel  <-  Abialabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Akwa Ibom"){
        LGApoints  <-  Akwa_Ibom 
        stateLabel  <-  Akwa_Ibomlabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ekiti"){
        LGApoints  <-  Ekiti 
        stateLabel  <-  Ekitilabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ondo"){ 
        LGApoints  <-  Ondo
        stateLabel  <-  Ondolabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Osun"){ 
        LGApoints  <-  Osun
        stateLabel  <-  Osunlabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Anambra"){ 
        LGApoints  <-  Anambra
        stateLabel  <-  Anambralabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ebonyi"){ 
        LGApoints  <-  Enugu
        stateLabel  <-  Enugulabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Taraba"){ 
        LGApoints  <-  Taraba
        stateLabel  <-  Tarabalabel
        textangle <- 0 
        couple  <-  "One"
        
      }else if(lgaGroups == "Kogi"){ 
        LGApoints  <-  Taraba
        stateLabel  <-  Tarabalabel
        textangle <- 0 
        couple  <-  "One"
        
      }else if(lgaGroups == "Kwara"){ 
        LGApoints  <-  Kwara
        stateLabel  <-  Kwaralabel
        textangle <- 0 
        couple  <-  "One" 
        
      }else if(lgaGroups == "Oyo"){ 
        LGApoints  <-  Oyo
        stateLabel  <-  Oyolabel
        textangle <- 0 
        couple  <-  "One" 
        
      }else{ 
        LGApoints == Ogun
        stateLabel  <-  Ogunlabel
        textangle <- 0 
        couple  <-  "One"
        
      }
      
      
      #filter by month and couple and state
      plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
      
      if(couple == "Two"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
      }
      
      if(couple == "Three"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
      }
      
      plotData <- droplevels(plotData[plotData$STATE %in% lgaGroups, ])
      
      AOI <- lgaGroups
      AOIMapS <- subset(boundaryNG, NAME_1 %in% AOI ) 
      
      AOIMap <- subset(ngstate, NAME_1 %in% AOI )
      AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
      LGAnames <- as.data.frame(AOIMap)
      LGAnames <- cbind(LGAnames, coordinates(AOIMap))
      colnames(LGAnames) <- c("STATE","LGA","long","lat"  )
      LGAnames <- LGAnames[!LGAnames$LGA %in% c("IbadanNorth-West","IbadanNorth-East","IbadanSouth-West", "IbadanSouth-East"),]
      LGAnames$LGA <- gsub("Egbado /", "", LGAnames$LGA )
      
      
      crop_ngstate <- subset(ngstate, NAME_1 %in% AOI )
      towns <- as.data.frame(TownsNG)
      towns <- towns[towns$name %in% cities & towns$fclass %in% c("town", "city"),]
      crop_RiversNG <-  crop(RiversNG, extent(crop_ngstate))
      crop_RiversNG <- crop_RiversNG[crop_RiversNG$fclass == "river", ]
      
      
      LGAaverage <- ddply(plotData, .(LGA, STATE), summarize,
                          LGAUrea = round(mean(rateUrea), digits=0),
                          LGANPK151515 = round(mean(rateNPK151515), digits=0),
                          LGAdY = round(mean(respY), digits=0))
      
      dss <- LGAaverage
      dss$LGAUrea <- dss$LGAUrea / 2.47105
      dss$LGANPK151515 <- dss$LGANPK151515 / 2.47105
      dss$LGAdY <- dss$LGAdY / 2.47105
      
      if(unit == 'acre'){
        LGAaverage <- dss
      }
      
      plotData <- merge(plotData, LGAaverage, by=c("LGA", "STATE"))
      
      if(unit == "ha"){
        plotData$Urea <- round(plotData$LGAUrea/25)*25
        plotData$NPK15_15_15 <- round(plotData$LGANPK151515/50)*50
        plotData$dY <- round(plotData$LGAdY/2)*2
      }else{
        plotData$Urea <- round(plotData$LGAUrea/10)*10
        plotData$NPK15_15_15 <- round(plotData$LGANPK151515/20)*20
        plotData$dY <- round(plotData$LGAdY/1)*1
      }
      
      fileNameCsv <- paste("tables", ".csv", sep="")
      
      AOIMap2 <- merge(AOIMap, unique(plotData[, c("LGA", "Urea", "NPK15_15_15","dY", "LGAdY")]),by.x="NAME_2" ,by.y="LGA")
      AOIMap2$month <- plantMonth
      AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
      plotData$month <- plantMonth
      tt <- unique(as.data.frame(plotData[, c("STATE","LGA", "Urea", "NPK15_15_15", "LGAdY", "month")]))
      tt <- tt[order(tt$STATE, tt$LGA), ]
      colnames(tt) <- c("STATE","LGA", "UREA", "NPK15_15_15", "YIELD", "MONTH")
      write.csv(tt, fileNameCsv, row.names = FALSE)
      
      AOIMap3 <- st_as_sf(AOIMap2)
      
      #generate static maps
      output$mytable = DT::renderDT({
        tt
      }, option=list(columnDefs=list(list(targets=3:5, class="dt-right"))),filter = "top")
      
      
      # output$tables = DT::renderDT(
      #   tt, options = list(pageLength = 20, width="100%", scrollX = TRUE
      #   ), filter = "top"
      # )
      
      tturea <- reactive({
        
        if(unit == "ha"){
          
          tturea <- paste("Urea (kg/ha)")
        }else {
          
          tturea <- paste("Urea (kg/acre)")
        }
      })
      
      
      
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      require(ggrepel)
      library(tmap)
      
      # #urea
      # observeEvent(tturea(),
      #              {
      #                
      #                output$ureaplot <- renderTmap({
      #                  
      #                  
      #                  sm1 = tm_shape(AOIMap3) +
      #                    tm_polygons(
      #                      col = "Urea",
      #                      title = tturea(),
      #                      #breaks = c(200, 175, 150, 125,100),
      #                      # labels = c("Low", "Medium", "High"),
      #                      palette = "Greens")+
      #                    tm_text(text = "NAME_2")
      #                  sm1
      #                  
      #                  
      #                  
      #                })
      #              })
      
      #urea2
      
      observeEvent(tturea(),
                   {
                     
                     output$ureaplot2 <- renderTmap({
                       
                       
                       sm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "Urea",
                           title = tturea(),
                           #breaks = c(200, 175, 150, 125,100),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Greens")+
                         tm_text(text = "NAME_2")
                       sm1
                       
                       
                       
                     })
                   })
      #NPK plot
      ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
      # ureacols <- reactive({
      #   
      #   if(unit == "ha"){
      #     ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
      #                   "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
      #     tturea <- "Urea (kg/ha)"
      #   }else {
      #     ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
      #                   "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
      #     tturea <- "Urea (kg/acre)"
      #   }
      # })
      
      ttnpk <- reactive({
        
        if(unit == "ha"){
          
          
          ttnpk <- paste("NPK 15:15:15 (kg/ha)")
        }else {
          
          ttnpk <- paste("NPK 15:15:15 (kg/acre)") 
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$NPK15_15_15)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
      levels(AOIMap3$NPK15_15_15) <- kev
      
      observeEvent(ttnpk(),
                   {
                     
                     output$npkplot <- renderTmap({
                       #                  sm2 = tm_shape(AOIMap3) +
                       # tm_polygons("NPK15_15_15", title = "tsz", showNA = TRUE,
                       #              border.col = "yellow", border.alpha = .5) + tm_shape(US_states) +
                       #  tm_borders(lwd = 1, col = "black", alpha = .5) +
                       #  tm_credits(paste0("Data @ Unites States Department of Agriculture\n",
                       #                      "Shape @ Unites States Census Bureau"),
                       #               position = c("right", "bottom")) +
                       # tm_layout(title = "2010 Adult Obesity by County, percent",
                       #             title.position = c("center", "top"),
                       #             legend.position = c("right", "bottom"), frame = FALSE,
                       #             inner.margins = c(0.1, 0.1, 0.05, 0.05))
                       #
                       #
                       #
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK15_15_15",
                           title = ttnpk(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "YlOrBr")+
                         tm_text(text = "NAME_2")+
                         # tm_credits(paste0("Data @ Unites States Department of Agriculture\n",
                         #                    "Shape @ Unites States Census Bureau"),
                         #             position = c("right", "bottom")) +
                         tm_layout(title = "2010 Adult Obesity by County, percent",
                                   title.position = c("center", "top"),
                                   legend.position = c("right", "bottom"), frame = FALSE,
                                   inner.margins = c(0.1, 0.1, 0.05, 0.05))
                       #set.zoom.limits = NA, legend.position = c("right", "top"), control.position = c("left", "top"), popup.all.data = NULL)
                       
                       
                       
                     })
                   })
      
      ttha <- reactive({
        
        if(unit == "ha"){
          
          ttha <- paste("Yield increase (t/ha)")
        }else {
          
          ttha <- paste("Yield increase (t/acre)")
        }
      })
      
      # if(unit == "ha"){
      #   ttha <- "Yield increase (t/ha)"
      #   Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
      #               "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
      #               "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
      # }else{
      #   ttha <- "Yield increase (t/acre)"
      #   Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
      #               "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
      #               "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
      # }
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      observeEvent(ttha(),
                   {
                     
                     
                     output$yieldplot <- renderTmap({
                       
                       
                       sm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "dY",
                           title = ttha(),
                           #breaks = c(3, 4, 5, 6),
                           #labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "NAME_2")
                       
                       
                       
                     })
                   })
      
      # Ureapalette <- brewer.pal(9,"Greens")
      # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])
      
      filt_select <- reactive({
        print(Selection)
        if (Selection == "Urea"){
          
          filt_select <- "Urea"
        }else if (Selection == "Yield"){
          filt_select <- "Yield"
        }else{
          filt_select <- "NPK 15:15:15"
        }
        
      })
      
      
      observeEvent(filt_select(), {
        if (filt_select() == "Urea"){
          
          ureacols <- reactive({
            
            if(unit == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          tturea <- reactive({
            
            if(unit == "ha"){
              
              tturea <- paste("Urea (kg/ha)")
            }else {
              
              tturea <- paste("Urea (kg/acre)")
            }
          })
          
          
          
          ureasclae <- unique(AOIMap3$Urea)
          keU <- as.character(ureasclae[order(ureasclae)])
          AOIMap3$Urea <- factor(AOIMap3$Urea)
          levels(AOIMap3$Urea) <- keU
          
          require(ggrepel)
          library(tmap)
          
          
          #urea
          observeEvent(tturea(),
                       {
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           sm1 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "Urea",
                               #title = tturea(),
                               #breaks = c(200, 175, 150, 125,100),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Greens")+
                             tm_text(text = "NAME_2")
                           sm1
                           
                           
                           
                         })
                       })
        }else if(filt_select() == "NPK 15:15:15"){
          
          ttnpk <- reactive({
            
            if(unit == "ha"){
              
              ttnpk <- paste("NPK 15:15:15 (kg/ha)")
            }else {
              
              ttnpk <- paste("NPK 15:15:15 (kg/acre)")
            }
          })
          
          
          
          mopsclae <- unique(AOIMap3$NPK15_15_15)
          kev <- as.character(mopsclae[order(mopsclae)])
          AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
          levels(AOIMap3$NPK15_15_15) <- kev
          
          observeEvent(ttnpk(),
                       {
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           
                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK15_15_15",
                               title = ttnpk(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")
                           # tm_credits(paste0("Data @ Unites States Department of Agriculture\n",
                           #                    "Shape @ Unites States Census Bureau"),
                           #             position = c("right", "bottom")) +
                           # tm_layout(title = "2010 Adult Obesity by County, percent",
                           #           title.position = c("center", "top"),
                           #           legend.position = c("right", "bottom"), frame = FALSE,
                           #           inner.margins = c(0.1, 0.1, 0.05, 0.05))
                           #set.zoom.limits = NA, legend.position = c("right", "top"), control.position = c("left", "top"), popup.all.data = NULL)
                           
                           sm2
                           
                         }) 
                       })
          
        }else{
          ttha <- reactive({
            
            if(unit == "ha"){
              
              ttha <- paste("Yield increase (t/ha)")
            }else {
              
              ttha <- paste("Yield increase (t/acre)")
            }
          })
          
          # if(unit == "ha"){
          #   ttha <- "Yield increase (t/ha)"
          #   Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
          #               "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
          #               "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
          # }else{
          #   ttha <- "Yield increase (t/acre)"
          #   Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
          #               "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
          #               "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
          # }
          
          Ysclae <- unique(AOIMap3$dY)
          keY <- as.factor(Ysclae[order(Ysclae)])
          AOIMap3$dY <- factor(AOIMap3$dY)
          levels(AOIMap3$dY) <- keY
          
          observeEvent(ttha(),
                       {
                         
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "dY",
                               title = ttha(),
                               #breaks = c(3, 4, 5, 6),
                               #labels = c("Low", "Medium", "High"),
                               palette = "YlGnBu")+
                             tm_text(text = "NAME_2")
                           
                           sm3
                           
                         })
                       })
          
          
          
          
        }
        
        #generate static maps
        # Ureapalette <- brewer.pal(9,"Greens")
        # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])
        if(unit == "ha"){
          ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                        "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
          tt <- "Urea (kg/ha)"
        }else {
          ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                        "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
          tt <- "Urea (kg/acre)"
        }
        ureasclae <- unique(AOIMap3$Urea)
        keU <- as.character(ureasclae[order(ureasclae)])
        AOIMap3$Urea <- factor(AOIMap3$Urea)
        levels(AOIMap3$Urea) <- keU
        
        require(ggrepel) 
        
        
        ggUrea <- ggplot(AOIMap3) +
          geom_sf(aes(fill=Urea), col="grey30") +
          scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          geom_text(data=stateLabel, aes(lon, lat, label=state, fontface=2), col='black', size=6)+
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          annotation_scale(location = "br", width_hint = 0.3, line_width = 0.4) +
          annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                                 style = north_arrow_fancy_orienteering) +
          # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
          #                        style = north_arrow_fancy_orienteering) +
          # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8)) 
        
        
        ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
        if(unit == "ha"){
          NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                       "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
          tt <- "NPK 15-15-15 (kg/ha)"
        }else{
          NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                       "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
          tt <- "NPK 15-15-15 (kg/acre)"
        }
        
        
        mopsclae <- unique(AOIMap3$NPK15_15_15)
        kev <- as.character(mopsclae[order(mopsclae)])
        AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
        levels(AOIMap3$NPK15_15_15) <- kev
        
        require(plotly)
        ggNPK <- ggplot(AOIMap3) +
          geom_sf(aes(fill=NPK15_15_15), col="grey30") +
          scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
          #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
        
        if(unit == "ha"){
          tt <- "Yield increase (t/ha)"
          Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
                      "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
                      "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
        }else{
          tt <- "Yield increase (t/acre)"
          Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
                      "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
                      "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
        }
        
        Ysclae <- unique(AOIMap3$dY)
        keY <- as.factor(Ysclae[order(Ysclae)])
        AOIMap3$dY <- factor(AOIMap3$dY)
        levels(AOIMap3$dY) <- keY
        
        ggYield <- ggplot(AOIMap3) +
          geom_sf(aes(fill=dY), col="grey30") +
          scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
          #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
        
        fileName <- paste("maps", ".pdf", sep="")
        pdf(fileName, onefile = TRUE, height = 14, width=12)
        #pdf.options(paper = "a4")
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5,0.8), "null"))))   
        grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
        print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
        #print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
        print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
        dev.off()
        
      })  
      
      
      if (usecase == "FR" & unit == "acre"){
        
        #download acre printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("FR Printable guides (acre)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
        
      }else if(usecase == "FR" & unit == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("FR Printable guides (ha)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        ) 
        
        
      }else if (usecase == "SPHS" & unit == "acre"){
        #download acre printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("SP Printable guides (acre)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
      }else if(usecase == "SPHS" & unit == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("SP Printable guides (ha)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        )  
        
      }   
      
    }else if(country == 'Tanzania'){
      
      
      
      Mara <- droplevels(ds[ds$REGION %in% c("Mara", "Simiyu"), ])
      Maralabel <- data.frame(REGION= c("Mara", "Simiyu"), lon=c(34.7, 33.7), lat=c(-1.2, -3.7))
      
      Kagera <- droplevels(ds[ds$REGION %in% c("Kagera", "Geita", "Kigoma"), ])
      Kageralabel <- data.frame(REGION= c("Kagera", "Geita", "Kigoma"), lon=c(30.25, 32.4, 31), lat=c(-2.1, -4, -6.1))
      
      Mwanza <- droplevels(ds[ds$REGION %in% c("Mwanza","Shinyanga"), ])
      Mwanzalabel <- data.frame(REGION= c("Mwanza","Shinyanga"), lon=c(33.65, 33.2), lat=c(-2.1, -4.1))
      
      Pwani <- droplevels(ds[ds$REGION %in% c("Tanga","Pwani"), ])
      Pwanilabel <- data.frame(REGION= c("Tanga","Pwani"), lon=c(37.6, 37.9), lat=c(-4.8, -7.3))
      
      Mtwara <- droplevels(ds[ds$REGION %in% c("Mtwara", "Lindi"), ])
      Mtwaralabel <- data.frame(REGION= c("Mtwara", "Lindi"), lon=c(37.5, 38.7), lat=c(-11.2,-8.1))
      
      Zanzibar <- droplevels(ds[ds$REGION %in% c("Zanzibar South and Central", "Zanzibar West", "Zanzibar North"), ])
      Zanzibarlabel <- data.frame(REGION= c("Zanzibar"), lon=c(39.5), lat=c(-5.95))
      
      Maracity <- data.frame(REGION = c("Mara", "Simiyu"),name=c("Musoma","Bariadi"), lat=c(-1.5,-2.8), lon = c(33.8, 33.98))
      
      Kageracity <- data.frame(REGION = c("Kagera", "Geita", "Kigoma"), name=c("Bukoba","Geita","Kigoma"), 
                               lat=c(-1.33, -2.87, -4.88), lon = c(31.82, 32.23,29.63))
      
      Pwaniacity <- data.frame(REGION = c("Pwani", "Tanga"),name=c("Kibaha","Tanga"), 
                               lat=c(-6.77, -5.07), lon = c(38.92, 39.01))
      
      Mwanzacity <- data.frame(REGION = c("Mwanza", "Shinyanga"),name=c("Mwanza", "Shinyanga"), 
                               lat=c(-2.52, -3.66), lon = c(32.9, 33.42))
      
      Mtwaraacity <- data.frame(REGION = c("Mtwara","Lindi"),name=c("Mtwara","Lindi"),
                                lat=c(-10.27, -9.99), lon = c(40.18, 39.71))
      Zanzibarcity <- data.frame(REGION = "Zanzibar",name="Zanzibar", lat=-6.17, lon = 39.2)
      
  
      
      
      if(lgaGroups =="Mtwara_Lindi"){
        LGApoints <- Mtwara
        stateLabel <- Mtwaralabel
        textangle<-0
        cities = Mtwaraacity
        couple <- "Two"
        
      }else if(lgaGroups =="Pwani_Tanga"){
          LGApoints <- Pwani 
          stateLabel <- Pwanilabel
          textangle<-0 
          cities = Pwaniacity
          couple <- "Two"
          
        }else if(lgaGroups =="Mwanza_Shinyanga"){
          LGApoints <- Mwanza 
          stateLabel <- Mwanzalabel
          textangle<-0 
          cities = Mwanzacity
          couple <- "Two"
          
        }else if(lgaGroups =="Mara_Simiyu"){
          LGApoints <- Mwanza 
          stateLabel <- Mwanzalabel 
          textangle<-0 
          cities = Maracity
          couple <- "Two"
          
        }else if(lgaGroups =="Kagera_Geita_Kigoma"){
          LGApoints <- Mwanza 
          stateLabel <- Mwanzalabel 
          textangle<-0 
          cities = Kageracity
          couple <- "Two"
          
        }else if(lgaGroups ==c("Zanzibar South and Central")){
          LGApoints <- Mwanza 
          stateLabel <- Mwanzalabel 
          textangle<-0 
          cities = Zanzibarcity
          couple <- "Two"
          
        }else if(lgaGroups ==c("Zanzibar West")){
          LGApoints <- Mwanza 
          stateLabel <- Mwanzalabel 
          textangle<-0 
          cities = Zanzibarcity
          couple <- "Two"
          
        }else if(lgaGroups ==c("Zanzibar North")){
          LGApoints <- Mwanza 
          stateLabel <- Mwanzalabel 
          textangle<-0 
          cities = Zanzibarcity
          couple <- "Two"
        }
      
      
      
      plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
      
      if(couple == "Two"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
      }
      
      if(couple == "Three"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
      }
      
      plotData <- droplevels(plotData[plotData$REGION %in% lgaGroups, ])
      
      AOI <- lgaGroups
      AOIMapS <- subset(boundaryTZ, NAME_1 %in% AOI ) 
      
      AOIMap <- subset(tzRegion, NAME_1 %in% AOI )
      AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
      LGAnames <- as.data.frame(AOIMap)
      LGAnames <- cbind(LGAnames, coordinates(AOIMap))
      colnames(LGAnames) <- c("REGION","DISTRICT","long","lat"  )
      crop_ngstate <- subset(tzRegion, NAME_1 %in% AOI )
      
      
      ## take REGION average
      LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                          LGAUrea = round(mean(rateUrea), digits=0),
                          LGANPK171717 = round(mean(rateNPK171717), digits=0),
                          LGADAP = round(mean(rateDAP), digits=0),
                          LGAdY = round(mean(respY), digits=0))
      
      
      dss <- LGAaverage
      dss$LGAUrea <- dss$LGAUrea / 2.47105
      dss$LGANPK171717 <- dss$LGANPK171717 / 2.47105
      dss$LGADAP <- dss$LGADAP / 2.47105
      dss$LGAdY <- dss$LGAdY / 2.47105
      
      if(unit == 'acre'){
        LGAaverage <- dss
      }
      
      plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
      
      if(unit == "ha"){
        plotData$Urea <- round(plotData$LGAUrea/25)*25
        plotData$NPK17_17_17 <- round(plotData$LGANPK171717/50)*50
        plotData$DAP <- round(plotData$LGADAP/25)*25
        plotData$dY <- round(plotData$LGAdY/2)*2
      }else{
        plotData$Urea <- round(plotData$LGAUrea/10)*10
        plotData$NPK17_17_17 <- round(plotData$LGANPK171717/20)*20
        plotData$DAP <- round(plotData$LGADAP/10)*10
        plotData$dY <- round(plotData$LGAdY/1)*1
      }
      
      fileNameCsv <- paste("tables", ".csv", sep="")
      
      AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","DAP","dY", "LGAdY")]),
                       by.x=c("NAME_1","NAME_2") ,by.y=c("REGION","DISTRICT"))
      AOIMap2$month <- plantMonth
      AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
      plotData$month <- plantMonth
      tt <- unique(as.data.frame(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","DAP", "LGAdY", "month")]))
      colnames(tt) <- c("REGION","DISTRICT", "UREA", "NPK17_17_17", "DAP","YIELD", "MONTH")
      tt <- tt[order(tt$REGION, tt$DISTRICT), ]
      write.csv(tt, fileNameCsv, row.names = FALSE)
      AOIMap3 <- st_as_sf(AOIMap2)
      
      # output$tables = DT::renderDT(
      #   tt, options = list(pageLength = 20, width="100%", scrollX = TRUE
      #   ), filter = "top"
      # )
      
      #generate static maps
      output$mytable = DT::renderDT({
        tt
      }, option=list(columnDefs=list(list(targets=3:5, class="dt-right"))),filter = "top")
      
      #side by side maps
      
      
      
      tturea <- reactive({
        
        if(unit == "ha"){
          
          tturea <- paste("Urea (kg/ha)")
        }else {
          
          tturea <- paste("Urea (kg/acre)")
        }
      })
      
      
      
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      require(ggrepel)
      library(tmap)
      
      
      #urea
      observeEvent(tturea(),
                   {
                     
                     output$ureaplot2 <- renderTmap({
                       
                       
                       sm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "Urea",
                           title = tturea(),
                           #breaks = c(200, 175, 150, 125,100),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Greens")+
                         tm_text(text = "NAME_2")
                       sm1
                       
                       
                       
                     })
                   })
   
      ttnpk <- reactive({
        
        if(unit == "ha"){
          
          ttnpk <- paste("NPK 17:17:17 (kg/ha)")
        }else {
          
          ttnpk <- paste("NPK 17:17:17 (kg/acre)")
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$NPK17_17_17)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
      levels(AOIMap3$NPK17_17_17) <- kev
      
      observeEvent(ttnpk(),
                   {
                     
                     output$npkplot <- renderTmap({
                       
                       
                       
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK17_17_17",
                           title = ttnpk(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Oranges")+
                         tm_text(text = "NAME_2")
                       # tm_credits(paste0("Data @ Unites States Department of Agriculture\n",
                       #                    "Shape @ Unites States Census Bureau"),
                       #             position = c("right", "bottom")) +
                       # tm_layout(title = "2010 Adult Obesity by County, percent",
                       #           title.position = c("center", "top"),
                       #           legend.position = c("right", "bottom"), frame = FALSE,
                       #           inner.margins = c(0.1, 0.1, 0.05, 0.05))
                       #set.zoom.limits = NA, legend.position = c("right", "top"), control.position = c("left", "top"), popup.all.data = NULL)
                       
                       sm2
                       
                     }) 
                   })
      
  
      ttdap <- reactive({
        
        if(unit == "ha"){
          
          ttdap <- paste("DAP (kg/ha)")
        }else {
          
          ttdap <- paste("DAP (kg/acre)")
        }
      })
      
      
      
      dapsclae <- unique(AOIMap3$DAP)
      kedap <- as.factor(dapsclae[order(dapsclae)])
      AOIMap3$DAP <- factor(AOIMap3$DAP)
      levels(AOIMap3$DAP) <- kedap
      
      observeEvent(ttdap(),
                   {
                     
                     output$dapplot <- renderTmap({
                       
                       
                       
                       sm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "DAP",
                           title = ttdap(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Blues")+
                         tm_text(text = "NAME_2")
                       sm4
                       
                     }) 
                   })
      
    
      ttha <- reactive({
        
        if(unit == "ha"){
          
          ttha <- paste("Yield increase (t/ha)")
        }else {
          
          ttha <- paste("Yield increase (t/acre)")
          
        }
      })
      
      # if(unit == "ha"){
      #   ttha <- "Yield increase (t/ha)"
      #   Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
      #               "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
      #               "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
      # }else{
      #   ttha <- "Yield increase (t/acre)"
      #   Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
      #               "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
      #               "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
      # }
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      observeEvent(ttha(),
                   {
                     
                     
                     output$yieldplot <- renderTmap({
                       
                       
                       sm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "dY",
                           title = ttha(),
                           #breaks = c(3, 4, 5, 6),
                           #labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "NAME_2")
                       
                       sm3
                       
                     })
                   })
      
      #generate downloadable maps
      
      if(unit == "ha"){
        ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                      "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
        ttz <- "Urea (kg/ha)"
      }else {
        ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                      "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
        ttz <- "Urea (kg/acre)"
      }
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      require(ggrepel) 
      
      ggUrea <- ggplot(AOIMap3) +
        geom_sf(aes(fill=Urea), col="darkgrey") +
        scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        geom_text(data=stateLabel, aes(lon, lat, label=REGION, fontface=2), col='black', size=6)+
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_fancy_orienteering) +
        # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
        #                        style = north_arrow_fancy_orienteering) +
        # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8)) 
      
      
      ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
      if(unit == "ha"){
        NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                     "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
        ttz <- "NPK 17-17-17 (kg/ha)"
      }else{
        NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                     "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
        ttz <- "NPK 17-17-17 (kg/acre)"
      }
      
      
      mopsclae <- unique(AOIMap3$NPK17_17_17)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
      levels(AOIMap3$NPK17_17_17) <- kev
      
      ggNPK <- ggplot(AOIMap3) +
        geom_sf(aes(fill=NPK17_17_17), col="darkgrey") +
        
        scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      
      
      DAPPpalette <- brewer.pal(9,"YlGnBu")
      if(unit == "ha"){
        DAPcols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                     "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
        ttz <- "DAP (kg/ha)"
      }else{
        DAPcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                     "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
        ttz <- "DAP (kg/acre)"
      }
      
      dapsclae <- unique(AOIMap3$DAP)
      kedap <- as.factor(dapsclae[order(dapsclae)])
      AOIMap3$DAP <- factor(AOIMap3$DAP)
      levels(AOIMap3$DAP) <- kedap
      
      ggDAP <- ggplot(AOIMap3) +
        geom_sf(aes(fill=DAP), col="darkgrey") +
        
        scale_fill_manual(values = DAPcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      
      # brewer.pal(9,"heat")
      Ydcols <- c("21"="#FF0000FF","20" = "#FF4600FF", "19"= "#FF8B00FF", "18"= "#FFD100FF", "17"= "#E8FF00FF",
                  "16"="#A2FF00FF", "15"= "#5DFF00FF", "14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                  "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                  "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                  "1"= "#FF008BFF", "0"= "#FFFFFF")
      
      if(unit == "ha"){
        ttz <- "Yield increase (t/ha)"
      }else{
        ttz <- "Yield increase (t/acre)"
      }
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      ggYield <- ggplot(AOIMap3) +
        geom_sf(aes(fill=dY), col="darkgrey") +
        
        scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      fileName <- paste("maps", ".pdf", sep="")
      pdf(fileName, onefile = TRUE, height = 14, width=12)
      #pdf.options(paper = "a4")
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5, 0.8), "null"))))   
      grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
      print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
      print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      print(ggDAP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
      print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
      dev.off()
      
     
      
      # Ureapalette <- brewer.pal(9,"Greens")
      # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])
      
      filt_select <- reactive({
        print(Selection2)
        if (Selection2 == "Urea"){
          
          filt_select <- "Urea"
        }else if (Selection2 == "Yield"){
          filt_select <- "Yield"
        }else if (Selection2 == "NPK 17:17:17"){
          filt_select <- "NPK 17:17:17"
        }else if (Selection2 == "NPK 15:15:15"){
          filt_select <- "NPK 15:15:15"
        }else{
          filt_select <- "DAP"
          
        }
        
      })
      
      
      observeEvent(filt_select(), {
        if (filt_select() == "Urea"){
          
          ureacols <- reactive({
            
            if(unit == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          tturea <- reactive({
            
            if(unit == "ha"){
              
              tturea <- paste("Urea (kg/ha)")
            }else {
              
              tturea <- paste("Urea (kg/acre)")
            }
          })
          
          
          
          ureasclae <- unique(AOIMap3$Urea)
          keU <- as.character(ureasclae[order(ureasclae)])
          AOIMap3$Urea <- factor(AOIMap3$Urea)
          levels(AOIMap3$Urea) <- keU
          
          require(ggrepel)
          library(tmap)
          
          
          #urea
          observeEvent(tturea(),
                       {
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           sm1 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "Urea",
                               title = tturea(),
                               #breaks = c(200, 175, 150, 125,100),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Greens")+
                             tm_text(text = "NAME_2")
                           sm1
                           
                           
                           
                         })
                       })
        }else if(filt_select() == "NPK 17:17:17"){
          ttnpk <- reactive({
            
            if(unit == "ha"){
              
              ttnpk <- paste("NPK 17:17:17 (kg/ha)")
            }else {
              
              ttnpk <- paste("NPK 17:17:17 (kg/acre)")
            }
          })
          
          
          
          mopsclae <- unique(AOIMap3$NPK17_17_17)
          kev <- as.character(mopsclae[order(mopsclae)])
          AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
          levels(AOIMap3$NPK17_17_17) <- kev
          
          observeEvent(ttnpk(),
                       {
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           
                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK17_17_17",
                               title = ttnpk(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")
                           # tm_credits(paste0("Data @ Unites States Department of Agriculture\n",
                           #                    "Shape @ Unites States Census Bureau"),
                           #             position = c("right", "bottom")) +
                           # tm_layout(title = "2010 Adult Obesity by County, percent",
                           #           title.position = c("center", "top"),
                           #           legend.position = c("right", "bottom"), frame = FALSE,
                           #           inner.margins = c(0.1, 0.1, 0.05, 0.05))
                           #set.zoom.limits = NA, legend.position = c("right", "top"), control.position = c("left", "top"), popup.all.data = NULL)
                           
                           sm2
                           
                         }) 
                       })
          
        }else if(filt_select() == "DAP"){
          ttdap <- reactive({
            
            if(unit == "ha"){
              
              ttdap <- paste("DAP (kg/ha)")
            }else {
              
              ttdap <- paste("DAP (kg/acre)")
            }
          })
          
          
          
          dapsclae <- unique(AOIMap3$DAP)
          kedap <- as.factor(dapsclae[order(dapsclae)])
          AOIMap3$DAP <- factor(AOIMap3$DAP)
          levels(AOIMap3$DAP) <- kedap
          
          observeEvent(ttdap(),
                       {
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           
                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "DAP",
                               title = ttdap(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Blues")+
                             tm_text(text = "NAME_2")
                           sm4
                           
                         }) 
                       })
          
        }else{
          ttha <- reactive({
            
            if(unit == "ha"){
              
              ttha <- paste("Yield increase (t/ha)")
            }else {
              
              ttha <- paste("Yield increase (t/acre)")
           
            }
          })
          
          # if(unit == "ha"){
          #   ttha <- "Yield increase (t/ha)"
          #   Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
          #               "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
          #               "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
          # }else{
          #   ttha <- "Yield increase (t/acre)"
          #   Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
          #               "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
          #               "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
          # }
          
          Ysclae <- unique(AOIMap3$dY)
          keY <- as.factor(Ysclae[order(Ysclae)])
          AOIMap3$dY <- factor(AOIMap3$dY)
          levels(AOIMap3$dY) <- keY
          
          observeEvent(ttha(),
                       {
                         
                         
                         output$ureaplot <- renderTmap({
                           
                           
                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "dY",
                               title = ttha(),
                               #breaks = c(3, 4, 5, 6),
                               #labels = c("Low", "Medium", "High"),
                               palette = "YlGnBu")+
                             tm_text(text = "NAME_2")
                           
                           sm3
                           
                         })
                       })
          
          ########
          
          
          
        } 
        
        
      })
      
     
     if (usecase == "FR" & unit == "acre"){
       
      #download acre printable guides
      output$downloadDatafr <- downloadHandler(
        filename <- function() {
          paste("FR Printable guides (acre)",  ".pdf", sep="")
        },
        
        content <- function(file) {
          file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Acre latest.pdf", file)
        },
        contentType = "application/pdf"
      )
      
     }else if(usecase == "FR" & unit == "ha"){
      #download hectare printable guides
      output$downloadDatafr <- downloadHandler(
        filename <- function() {
          paste("FR Printable guides (ha)",  ".pdf", sep="")
        },
        
        content <- function(file) {
          file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
        },
        contentType = "application/pdf"
      ) 
      
      
     }else if (usecase == "SPHS" & unit == "acre"){
       #download acre printable guides
       output$downloadDatafr <- downloadHandler(
         filename <- function() {
           paste("SP Printable guides (acre)",  ".pdf", sep="")
         },
         
         content <- function(file) {
           file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Acre latest.pdf", file)
         },
         contentType = "application/pdf"
       )
     }else if(usecase == "SPHS" & unit == "ha"){
       #download hectare printable guides
       output$downloadDatafr <- downloadHandler(
         filename <- function() {
           paste("SP Printable guides (ha)",  ".pdf", sep="")
         },
         
         content <- function(file) {
           file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Hectare latest.pdf", file)
         },
         contentType = "application/pdf"
       )  
       
     }   
      
      
      
      
    }
    
    output$text <- renderText({
      paste("Maps and tables below present fertilizer recommendations for cassava planted in", plantMonth, "in", lgaGroups2, "in a field with", yield_level, 
            ". Recommendations are optimized to obtain a maximal return on investment, assuming cassava will be harvested after 12 months, 
              and sold at a price of [price_roots] Naira per tonne; Fertilizer prices used in calculations are [price_urea] Naira per 50 kg bag of urea, 
                  and [price_NPK151515] Naira per 50 kg bag of NPK 15:15:15.")
    })
    
    #download maps
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
      },
      
      content <- function(file) {
        file.copy("maps.pdf", file)
      },
      contentType = "application/pdf"
    )
    
    #download tables
    output$downloadcsv <- downloadHandler(
      filename <- function() {
        paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
      },
      
      content <- function(file) {
        file.copy("tables.csv", file)
      },
      contentType = "application/csv"
    )
    
    
    
  })
  
}


#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
