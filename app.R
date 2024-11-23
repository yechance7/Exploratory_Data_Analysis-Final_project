library(shiny)

ui <- fluidPage(
    titlePanel("Shiny App Example"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("num", "Choose a number:", min = 1, max = 100, value = 50)
        ),
        mainPanel(
            plotOutput("hist")
        )
    )
)

server <- function(input, output) {
    output$hist <- renderPlot({
        hist(rnorm(input$num), main = "Histogram", col = "blue")
    })
}

shinyApp(ui = ui, server = server)
