library(shiny)
library(tidyverse)
library(plotly)
library(palmerpenguins)

# We want to keep the server function as clean as possible, so its easy to debug. 
# For this, the best (in my opinion) is to wrap all the code in functions. 

# Let's take one of the plots from 1_dataviz/1_explanation.Rmd and turn it into a function. 
# The most straghit forward would be a function that just runs the piece of code we wrote before
# Let's add a simple filtering, so the user can select the observations by year & island.

plot_1 <- function(years, islands){

    penguins %>% 
        filter(year%in%years,island%in%islands) %>% 
        #be careful not to reuse the variables names, otherwise this will not filter!
    ggplot(., aes(x = flipper_length_mm,
                         y = body_mass_g)) +
        geom_point(aes(color = sex)) +
        facet_wrap(~species) +
        theme_minimal() +
        scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
        labs(title = "Penguin flipper and body mass",
             subtitle = "Dimensions for male and female Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
             x = "Flipper length (mm)",
             y = "Body mass (g)",
             color = "Penguin sex") +
        theme(legend.position = "bottom",
              legend.background = element_rect(fill = "white", color = NA),
              plot.title.position = "plot",
              plot.caption = element_text(hjust = 0, face= "italic"),
              plot.caption.position = "plot")
}

# try it out
# plot_1(c(2009),c('Biscoe','Dream'))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Palmer Penguins"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #let's select the propoer widgets for the inputs: 
            # https://shiny.rstudio.com/gallery/widget-gallery.html
            # As we might want many islands, I would select a checkboxGroupInput
            checkboxGroupInput("islands",
                        "Select the islands:",
                        choices = unique(penguins$island),
                        selected = unique(penguins$island)),
                                # we can define the choices with R code
            # for the years, we could use a slider range
            sliderInput("years", 
                        label = 'Select the years',
                        min = 2007, max = 2009, value = c(2007, 2009))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot_1")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot_1 <- renderPlot({
        # the slider returns 2 values, for the min and the max. 
        # Let's convert it to a sequence with all possible values for the filtering.
        years <- seq.int(input$years[1],input$years[2])

        plot_1(years = years, islands = input$islands)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
