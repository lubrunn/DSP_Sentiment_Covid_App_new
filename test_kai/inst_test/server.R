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
    if (!is.null(ranges$x)) {
      ranges$x <- as.Date(ranges$x, origin = "1970-01-01")
    }
    ggplot(stockdata_DE(),aes(Date,Close.,color = name))+
      geom_line()+
      theme_classic()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })

  output$hover_info_DE <- renderUI({
    req(input$hovering)
    create_hover_info_DE(input$plot_hover_DE,stockdata_DE())
  })

  ranges <- reactiveValues(x = NULL, y = NULL)

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })


}
