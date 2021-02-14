

function(input, output, session) {
  getExampleData <- reactive({
    test_data()
  })
  outVar <- reactive({
    test_data <- getExampleData()

    sort(unique(as.character(test_data$folder)))
  })

  observe({
    updateSelectInput(session, "tweetType", choices = outVar())
  })

  output$plot1 <- renderPlot({
    plotdata(test_data())
  })

}
