library(shiny)

ui <- fluidPage(
    titlePanel("Old Faithful Geyser Data"),
    histogram_ui('some_id')
)
server <- function(input, output) {
    histogram_server('some_id')
}
shinyApp(ui = ui, server = server)
