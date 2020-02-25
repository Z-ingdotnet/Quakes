#Past 30 Days
#Updated every minute.


############################################################################
############################################################################
###                                                                      ###
###                                TITLE:                                ###
###                                R CODE                                ###
###                         AUTHOR: IVERSON (Zhuzheng) ZHOU              ###
###                           DATE: 2020-02-23                           ###
###                              VERSION 1                               ###
###                    TOPIC: Real Time Earthquakes Dashboard            ###
###                    DATASET: World EarthQuakes metadata               ###
###                                                                      ###
############################################################################
############################################################################


Packages <- c(
 "shiny","shinydashboard","rgdal","maptools","ggplot2","ggthemes","rgeos","dplyr"
)
#install.packages(Packages)
lapply(Packages, library, character.only = TRUE)


setwd("C:/Users/Zing/OneDrive/GitHub/R/Mapping_Quakes")



New_Quakes <- readOGR("all_month.geojson", encoding="OGRGeoJSON", stringsAsFactors=FALSE)


world <- readOGR("countries.geo.json", encoding="OGRGeoJSON", stringsAsFactors=FALSE)
plates <- readOGR("plates.json", encoding="OGRGeoJSON", stringsAsFactors=FALSE)
#quakes <- readOGR("quakes2(2014-2016).json", encoding="OGRGeoJSON", stringsAsFactors=FALSE)

   
world_map <- fortify(world)
plates_map <- fortify(plates)
quakes_dat <- data.frame(quakes)
New_Quakes_dat<-data.frame(New_Quakes)

#Convert epoch to human-readable date
New_Quakes_dat$date<-as.POSIXct(as.numeric(New_Quakes_dat[,c(4)])/1000, origin = "1970-01-01")
str(New_Quakes_dat)
New_Quakes_dat$date2<-as.Date(New_Quakes_dat$date)

#Convert epoch to human-readable date
#   quakes_dat$date<-as.POSIXct(as.numeric(quakes_dat[,c(4)])/1000, origin = "1970-01-01")
#   str(quakes_dat)
#   quakes_dat$date2<-as.Date(quakes_dat$date)

#quakes_dat$trans <- quakes_dat$mag %% 5      #integer remainer
#New_Quakes_dat$trans <- New_Quakes_dat$mag %% 5      #integer remainer

load("./quakes_master.Rdata")

quakes_master<-unique(rbind(quakes_master, New_Quakes_dat)) # rbind both data.frames
save(quakes_master, file = "./quakes_master.Rdata")



header <- dashboardHeader(title = "Basic Dashboard")  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Contact-me", icon = icon("barcode ",lib='glyphicon'), 
             href = "http://z-ing.net")
  )
)




frow1 <- fluidRow(
  column(3,
         dateRangeInput("dateRange", h3("Filter Earthquake Events by date")
                       , start = min(quakes_master$date), end =  max(quakes_master$date)
                       , min = NULL,max = NULL
                       , format = "yyyy-mm-dd",
                        autoclose = TRUE
          )
         
         )
  
 # column(3, 
 #        numericInput("mag", 
 #                     h3("Input earthquake magnitude scale"), 
 #                     value = 5))   

  , 
         sidebarPanel(sliderInput( "magslider", h3("Select earthquake magnitude scale")
                     ,min = 1,max= 10,value = c(5, 9) ))
  
  #valueBoxOutput("value1")
 # ,valueBoxOutput("value2")

)

#frow1_1<-  fluidRow(
#  column(4, verbatimTextOutput("range"))
#)

frow2 <- fluidRow(
  
#  box(
#    title = "Test"
#    ,status = "primary"
#    ,solidHeader = TRUE 
#    ,collapsible = TRUE 
#    ,plotOutput("test", height = "1500px",width = 4)
#  )
  plotOutput("test",height = "1000px")
)
body <- dashboardBody(frow1, frow2
                      )

ui <- dashboardPage(title = 'Real Time EarthQuakes from around the globe', header, sidebar, body, skin='red')


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
 
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  


  
  output$test <- renderPlot({
    
   quakes_master_shiny<- quakes_master %>% filter( (mag>=input$magslider[1] & mag<=input$magslider[2]) &
                                        (date2 >=input$dateRange[1] & date2 <= input$dateRange[2] ))

    gg <- ggplot()
    gg <- gg + geom_map(data=world_map, map=world_map,
                        aes(x=long, y=lat, map_id=id),
                        color="white", size=0.15, fill="#d8d8d6")
    gg <- gg + geom_map(data=plates_map, map=plates_map,
                        aes(x=long, y=lat, map_id=id),
                        color="black", size=0.1, fill="#00000000", alpha=0)
    gg <- gg + geom_point(data=quakes_master_shiny,
                          aes(x=coords.x1, y=coords.x2, size=mag),
                          shape=1, alpha=1/3, color="#d47e5d", fill="#00000000")
    gg <- gg + geom_point(data=subset(quakes_master_shiny, mag>7.5),
                          aes(x=coords.x1, y=coords.x2, size=mag),
                          shape=1, alpha=1, color="black", fill="#00000000")
    gg <- gg + geom_text(data=subset(quakes_master_shiny, mag>7.5),
                         aes(x=coords.x1, y=coords.x2, label=sprintf("Mag %2.1f", mag)),
                         color="black", size=3, fontface="bold")
    gg <- gg + scale_size(name="Magnitude",trans='exp'
                          ,breaks = c(5:8)
                          , labels = c(5:8)
                          ,range=c(1, 20),guide = "legend")
    #gg <- gg +scale_fill_manual(values = c(5,6,7,8), 
    #                  drop = FALSE,
    #                  name="test",
    #                  labels=c("5", "6", "7", "8"))
    gg <- gg + coord_map("mollweide")
    gg <- gg + theme_map()
    gg <- gg + theme(legend.position=c(0.05, 0.99))
    gg <- gg + theme(legend.direction="horizontal")
    gg <- gg + theme(legend.key=element_rect(color="#00000000"))
    gg
    
  })
}

shinyApp(ui, server)
