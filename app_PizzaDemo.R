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
    actionButton("update", "Update range")
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    leafletOutput("mymap"),
    plotOutput(outputId = "waitPlot")
    
  )
))

server <- function(input, output, session) {
  #generation fake data
  sampleData=mock_pizza_read_data()
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
      output$mymap <- renderLeaflet({
        mock_pizza_mapGen(sampleSDF,routes_final)

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
      print("lol")
    })
    #histogram of waiting time of customers
}
shinyApp(ui, server)
