

function(input, output, session) {

    filtered_df <- reactive({
     res <- test_data()
     #res <- res[res$folder %in% input$tweetType, ]

     res <- res %>% filter( (date > input$timeWindow[1]) &
                             (date < input$timeWindow[2]))
    })

    filtered_df_min_retweet <- reactive({
      res <- filtered_df()
      res <- res %>% filter(retweets_count > input$minRetweet)
    })

    outVar <- reactive({
      test_data <- test_data()
      sort(unique(as.character(test_data$folder)))
    })

    observe({
      updateSelectInput(session, "tweetType", choices = outVar())
    })


   max_retweet <- reactive(({
    test_data <- filtered_df()
    max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
    min(max_val_vec$maxi)

    }))

   observe({
    updateSliderInput(session, "minRetweet", max = max_retweet())
    })

   output$plot1 <- renderPlot({
      TS_plot(filtered_df_min_retweet(), aggregation = input$aggregation)})

   output$plot2 <- renderPlot({
      density_plot(filtered_df_min_retweet(), aggregation = input$aggregation)})



}
