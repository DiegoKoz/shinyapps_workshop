library(tidyverse)
library(shiny)
library(gapminder)
library(countrycode)
library(plotly)
library(scales)

gapminder <- gapminder


# we can do the preprocessing within the app
# If we put the preprocessing in the function we'll use for plotting, we would run this each time we 
# change a parameter! it's more efficient to leave it outside

#for example. As Kuwuait will break the scale for some years, let's remove it from the database

gapminder <- gapminder %>% 
  filter(country!='Kuwait')

plot_1 <- function(countries, var){
  
  plt <- gapminder %>%
    pivot_longer(lifeExp:gdpPercap) %>% 
    filter(country %in% countries, name==var) %>%  
    ggplot(aes(year, value, color = continent,group = country))+ 
    geom_line()+
    geom_point()
  
  ggplotly(plt)
}

plot_2 <- function(yr){
  
  plt <- gapminder %>%
    filter(year==yr) %>% 
    ggplot(aes(gdpPercap, lifeExp, color=continent,size=pop, text = paste(country,
                                                                        '<br>GDP per capita: ',number(gdpPercap, 1),
                                                                        '<br>Population: ', number(pop/10000,1),'M'))) +
    geom_point()+
    # scale_y_continuous(labels= percent)+
    theme_minimal()
  
  ggplotly(plt, tooltip = 'text')
}



#### Interfaz de usuario

ui <- fluidPage(
  titlePanel("Gapminder Data"),
  tabsetPanel(
    tabPanel(title = 'time series',
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(inputId ='countries', 
                                label='Choose the countries', 
                                choices = unique(gapminder$country), 
                                multiple = TRUE, 
                                selected = "United States") ,  ### noten que hay una coma que separa los diferentes inputs
                 selectizeInput(inputId ='inputvariable', 
                                label='Choose the variable to display', 
                                choices =  c("lifeExp","pop","gdpPercap"),
                                multiple = FALSE,
                                selected = "lifeExp")
                 
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", plotlyOutput("plot_1")), 
                             # note that plotly figures have a special plotlyOutput function
                             tabPanel("Table", tableOutput("table"))
                 )
                 
               )
             )
    ),
    tabPanel(title = 'distribution by year',
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId ='yr', 
                             label='Choose the year',
                             min = min(gapminder$year),
                             max = max(gapminder$year),
                             value = min(gapminder$year),
                             step = 5,
                             animate = TRUE,
                             dragRange = FALSE
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_2"), 
                 
               )
               
             )
    )
  )
)


#### Server

server <- function(input, output) {
  
  ## first plot
  output$plot_1 <- renderPlotly({ # we also need a special render for plotly figures
    plot_1(countries = input$countries, var = input$inputvariable)
  })
  
  ## table
  
  output$table <- renderTable({
    gapminder %>%
      filter(country %in% input$countries) %>% 
      select(country,continent, year, input$inputvariable )
  })
  
  ## second plot
  output$plot_2 <- renderPlotly({ plot_2(yr = input$yr)})
  
}

shinyApp(ui = ui, server = server)
