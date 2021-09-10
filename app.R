library(shiny)
library(shinydashboard)
library(plotly)

# First check whether all packages are installed! 
source("checkPackages.R")

ui <- dashboardPage(
  dashboardHeader(title = "A dashboard Demo"),
  dashboardSidebar(
    sliderInput("nsim",
                "Number of simulations:",
                min = 50, max = 500,
                value = 100),
    selectInput("dist",
                "Distributions",
                choices = c("Normal","Exponential"))
  ),
  dashboardBody(
    plotlyOutput("plot1")
  )
)

server <- function(input, output, session){
  
  dfun <- reactive({switch(input$dist,
                           Normal = dnorm,
                           Exponential = dexp)})
  rfun <- reactive({switch(input$dist,
                           Normal = rnorm,
                           Exponential = rexp)})
  simul <- reactive({
    rfun()(input$nsim)
  })
  
  output$plot1 <- renderPlotly({
    p <- ggplot(data.frame(x=simul()), aes(x=x)) +
      geom_histogram(mapping = aes(y = stat(density)),
                     bins = 20) +
      stat_function(fun=dfun(),
                    geom = "area",
                    fill = alpha("red", 0.2))
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)