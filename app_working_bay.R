library(shiny)



# colourpicker example app
shinyApp(
  ui = fluidPage(
    colourpicker::colourInput("col", "Select colour", "purple"),
    plotOutput("plot")
  ),
  server = function(input, output) {
    output$plot <- renderPlot({
      set.seed(1)
      plot(rnorm(50), bg = input$col, col = input$col, pch = 21)
    })
  }
)
