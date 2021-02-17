

function(input, output, session) {

    observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
    })

    dataset <- reactive({
      if (input$Sentiment_type == "NoFilter"){
        fil <- Range_input(input$minRetweet)
        res <- eval(parse(text = paste('En', '_NoFilter_',fil,'()', sep='')))

      }else{
        filename <- paste0(input$stock,"_",input$language,"()")
        res <- write(filename, stdout())
      }
    })
#input$language

    filtered_df <- reactive({ # subset pre-filtered dataset
     if (input$Sentiment_type == "NoFilter"){
        res <- dataset()
        res <- res %>% filter((retweet_filter == input$minRetweet) &
                              (likes_filter == input$minLikes) &
                              (long_tweet == input$tweet_length))
     }else{ # live filtering
        res <- dataset()
        res %>% filter((retweets_count > input$minRetweet)
                       (likes_filter > input$minLikes) &
                       (tweet_length > median(tweet_length) ))
     }
        #time just for range of plot
        #res <- res %>% filter( (date > input$timeWindow[1]) &
        #                     (date < input$timeWindow[2]))


   })




  # max_retweet <- reactive(({
  #  test_data <- filtered_df()
  #  max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
  #  min(max_val_vec$maxi)

   # }))

   #observe({
  #  updateSliderInput(session, "minRetweet_stocks", max = max_retweet())
  #  })




   output$plot1 <- renderPlot({
      TS_plot(filtered_df(), aggregation = input$aggregation,input$Sentiment_type)})

   output$plot2 <- renderPlot({
      density_plot(filtered_df(), aggregation = input$aggregation,input$Sentiment_type)})



}
