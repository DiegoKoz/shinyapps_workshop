
histogram_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    plot_hist <- function(bins){
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    }

    output$distPlot <- renderPlot({plot_hist(input$bins)})
  
    })
}

histogram_ui <- function(id) {
  ns <- NS(id) #now we need to be explicit about the NameSpace of our inputs and outputs
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("bins"), # we wrap the input id with ns()
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput(ns("distPlot")) # we wrap the output id with ns()
    )
  )
}
