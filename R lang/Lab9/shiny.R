dat <- rnorm(1000, 0, 1)

ui <- fluidPage(
  titlePanel("Sampling the Normal Distribution"),
  fluidRow(
    plotOutput("distPlot"),
    column(4,
           wellPanel(
             sliderInput("obs", "Number of observations:",
                         min = 1, max = 1000, value = 500
             )
    )
  )
))

server <- function(input, output) {
  output$distPlot <- renderPlot(hist(sample(dat, input$obs)))
}

shinyApp(ui, server)