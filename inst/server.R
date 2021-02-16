function(input, output, session) {
  all <- reactive({
    req(input$Stock)
    symbols <- COMPONENTS_DE()[["Symbol"]][COMPONENTS_DE()[["Company.Name"]] %in% input$Stock]
    all <- NULL
    for (s in symbols) {
      plotting <- do.call(paste0(sub("\\..*", "", s)), list())
      plotting <- plotting[plotting$Close. != "-",]
      plotting[c("Open","High","Low","Close.","Adj.Close..")] <- sapply(plotting[c("Open","High","Low","Close.","Adj.Close..")],as.numeric)
      plotting$Date <- as.Date(plotting$Date, "%b %d, %Y")
      plotting["Volume"] <- as.numeric(gsub(",","",plotting$Volume))
      plotting$name <- s
      all <- rbind(all,plotting)
      plotting <- NULL
    }
    all %>%
      filter(Date >= input$dates[1] & Date <= input$dates[2])
  })

  observeEvent(input$reset,{
    updateSelectizeInput(session,"Stock",selected = "")
  })

  output$plot_DE <- renderPlot({
    req(input$Stock)
    ggplot(all(),aes(Date,Close.,color = name))+
      geom_line()+
      theme_classic()
  })

  output$hover_info_DE <- renderUI({
    hover <- input$plot_hover_DE
    point <- nearPoints(all(), hover, threshold = 100, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Company: </b>", point$name, "<br/>",
                    "<b> Date: </b>", point$Date, "<br/>",
                    "<b> Price: </b>", point$Close., "<br/>")))
    )
  })


}
