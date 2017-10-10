navbarPage("Mock Pizza Routing Demo", theme="Judge.css", responsive = TRUE,

           tabPanel("Map",
                    fluidRow(
                    column(4,
                      wellPanel(
                        
                        actionButton("enterRPMode", "Enter Route Planning Mode"),
                        actionButton("endRPMode", "Leave Route Planning Mode"),
                      
                      ##enter number of couriers
                      numericInput("cour", "Number of couriers:", 1, min = 1, max=1000),
                      ##number of adresses in a route
                      numericInput("plcs", "Max address number in 1 route:", 1, min = 1, max=1000),
                      ##maximum of km in a route
                      numericInput("kmrt", "Max distance for 1 route (km):", 1, min = 1, max=1000)
                      )
                      
                    ),
                    column(8,
                      leafletOutput("mymap"),
                      plotOutput(outputId = "waitPlot")
                    )
           )
),
tabPanel("Orders",
         fluidRow(
         tableOutput("table2")
         )
)


)

