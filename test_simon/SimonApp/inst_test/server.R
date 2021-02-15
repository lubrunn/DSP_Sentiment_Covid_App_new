

function(input, output, session) {
  # getExampleData <- reactive({
  #   test_data()
  # })


  outVar <- reactive({
    test_data <- test_data()
    sort(unique(as.character(test_data$folder)))
  })

  observe({
    updateSelectInput(session, "tweetType", choices = outVar()) #data()$customerID #c(output$outVar)
  })


  output$plot1 <- renderPlot({
    TS_plot(test_data(),tweetType = input$tweetType, aggregation = input$aggregation)
  })

}
