rm(list = ls())
library(shiny)
library(leaflet)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(maps)
library(sp)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(DT)
library(maptools)
source('~/pizzaprojekt/1007 - V2.0/pizza_demo_mapGen.R')
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
    leafletOutput("mymap")
  )
))

server <- function(input, output, session) {
      output$mymap <- renderLeaflet({
        mock_pizza()

  })
      observeEvent(input$mymap_marker_click,{
      print("lol")
    })
    
}
shinyApp(ui, server)
