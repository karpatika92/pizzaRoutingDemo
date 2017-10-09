rm(list = ls())
library(shiny)
library(leaflet)
library(tmap)
library(tmaptools)
#library(OpenStreetMap)
library(maps)
library(sp)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(DT)
library(maptools)
source('~/pizzaRoutingDemo/pizza_demo_mapGen.R')
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Sliders"),
  sidebarPanel(
    
    # Specification of range within an interval
    sliderInput("timeRange", label = "Time range",
                min = as.POSIXct(Sys.time())-10000*60*60,
                max = as.POSIXct(Sys.time()),
                step=30*60,
                value = c(as.POSIXct(Sys.time())-10000*60*60,
                          as.POSIXct(Sys.time()))),
    actionButton("enterRPMode", "Enter Route Planning Mode"),
    actionButton("endRPMode", "Leave Route Planning Mode")
    
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    leafletOutput("mymap"),
    plotOutput(outputId = "waitPlot")
    
  )
))

server <- function(input, output, session) {
  #generation fake data
  data <- reactiveValues(clickedMarker=NULL)
  #this stores the clicked line
  clickedLine <- reactiveValues(clickedLine=NULL)
  manualRPMode=reactiveValues(value = FALSE)
  
  print("READING DATA")
  sampleData=mock_pizza_read_data()
  print("STARTING GEOCODING")
  sampleSDF=mock_pizza_geocoding(sampleData)
  depotAdress="Budapest IX. kerulet , Viola u. 48"
  depotCoords=rev(geocode_OSM(depotAdress)[[2]])
  #now solve the bin packing problem
  timeConstraint=60*60
  #convert time to km
  #v=s/t
  avgSpeed=20/3600
  
  sampleSDF=mock_pizza_assigner(sampleSDF,depotCoords,depotAdress,timeConstraint,avgSpeed)
  numRoutes=max(sampleSDF@data$routes)
  routes_final=mock_pizza_router(sampleSDF,depotAdress)
  sampleSDF=mock_piza_waitTime(sampleSDF,routes_final)
  print("HERE ARE THE ROUTES")
  print(routes_final)
  print("HERE ARE THE sampleSDF")
  print(sampleSDF)
  
  #The map
  displayMap=mock_pizza_mapGen(sampleSDF,routes_final,depotAdress,depotCoords)
  
      output$mymap <- renderLeaflet({
        displayMap

  })
      output$waitPlot <- renderPlot({
        
        x    <- as.numeric(sampleSDF$waitingTime)
        print("LOL")
        bins <- seq(min(x), max(x), length.out = 4 + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "Waiting time to pizza arrival",
             main = "Histogram of waiting times")
        
      })
      #event upon marker click
      observeEvent(input$mymap_marker_click,{
        data$clickedMarker <- input$mymap_marker_click
        print(data$clickedMarker)
        if(data$clickedMarker$id==depotAdress){
          print("You Clicked the Depot, Time to click on the first location!")
        }
          }
      )
      #POLYLINE CLICK
      
      observeEvent(input$mymap_shape_click,{
        clickedLine <- input$mymap_shape_click
        print(clickedLine)
        print(manualRPMode$value)
        proxy <- leafletProxy("mymap")
        if(manualRPMode$value==FALSE){
          sampleSDF@data$sectionID[sampleSDF@data$sectionID==clickedLine$id]=NA
          sampleSDF@data$section[sampleSDF@data$sectionID==clickedLine$id]=NA
          sampleSDF@data$sectionID[sampleSDF@data$sectionID==clickedLine$id]=NA
          removeShape(proxy,clickedLine$id)
        }
        
        })
      
      
      #MARKER CLICK
      observeEvent(input$mymap_click,{
        data$clickedMarker <- NULL
        print(data$clickedMarker)})
      #ENTER END LEAVE MANUAL ROUTE PLANNING MODE
      observeEvent(input$enterRPMode, {
        manualRPMode=TRUE
        print(manualRPMode)
      })
      observeEvent(input$endRPMode, {
        manualRPMode=FALSE
        print(manualRPMode)
      })
      

#histogram of waiting time of customers
}
shinyApp(ui, server)
