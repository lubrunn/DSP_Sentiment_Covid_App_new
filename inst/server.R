server <- function(input, output, session) {

  stockdata_DE <- reactive({
    req(input$Stock)
    stock_dataset_DE(input$Stock,input$dates[1],input$dates[2])
  })

  observeEvent(input$reset,{
    updateSelectizeInput(session,"Stock",selected = "")
  })

  output$plot_DE <- renderPlot({
    req(input$Stock)
    ggplot(stockdata_DE(),aes(Date,Close.,color = name))+
      geom_line()+
      theme_classic()
  })

  output$hover_info_DE <- renderUI({
    create_hover_info_DE(input$plot_hover_DE,stockdata_DE())
  })

}
