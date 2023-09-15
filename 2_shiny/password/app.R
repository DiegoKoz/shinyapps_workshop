library(shiny)
library(shinyauthr)

user_base <- data.frame(
    user = c("someuser", 'someotheruser'),
    password = c("super",'secure'),
    permissions = c("admin","standard"),
    name = c("someuser", 'someotheruser'),
    stringsAsFactors = FALSE,
    row.names = NULL
)



main_ui <- tagList(
    titlePanel("Old Faithful Geyser Data"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)



ui <- fluidPage(
    
    shinyjs::useShinyjs(),
    loginUI(id = "login"),
    
    uiOutput(outputId = "secured_ui")
    
)

server <- function(input, output) {
    
    # call the logout module with reactive trigger to hide/show
    logout_init <- callModule(shinyauthr::logout,id = "logout",active = reactive(credentials()$user_auth))
    credentials <- callModule(shinyauthr::login,
                              id = "login",
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              log_out = reactive(logout_init))
    user_data <- reactive({credentials()$info})
    
    #render UI for authorised users
    output$secured_ui <- renderUI({
        req(credentials()$user_auth)
        main_ui})
    

    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}
shinyApp(ui = ui, server = server)
