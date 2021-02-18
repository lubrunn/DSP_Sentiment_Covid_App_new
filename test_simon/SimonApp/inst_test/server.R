

function(input, output, session) {

    observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    req(input$Sentiment_type)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
    })

    observeEvent(input$reset,{
    updateSelectizeInput(session,"aggregation",selected = "")
    })

    dataset <- reactive({
      req(input$Sentiment_type)
      if (input$Sentiment_type == "NoFilter"){
        req(input$minRetweet)
        fil <- Range_input(input$minRetweet)
        res <- eval(parse(text = paste('En', '_NoFilter_',fil,'()', sep='')))
                                  #input$language
      }else{
        filename <- paste0(input$stock,"_",input$language,"()")
        res <- write(filename, stdout())
      }
    })


    filtered_df <- reactive({ # subset pre-filtered dataset
      req(input$Sentiment_type)

      if (input$Sentiment_type == "NoFilter"){
        req(input$minRetweet)
        req(input$minLikes)
        res <- dataset()
        res <- res %>% filter((retweet_filter == input$minRetweet) &
                               (likes_filter == input$minLikes) &
                               ((long_tweet == "yes")|(long_tweet == "no")))
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
     req(input$aggregation)
     req(input$Sentiment_type)
      TS_plot(filtered_df(), aggregation = input$aggregation,input$Sentiment_type,
              input$facet,input$tweet_length)})

   output$plot2 <- renderPlot({
     req(input$aggregation)
     req(input$Sentiment_type)
      density_plot(filtered_df(), aggregation = input$aggregation,input$Sentiment_type,
                   input$facet,input$tweet_length)})

   output$plot3 <- renderPlot({
     req(input$aggregation)
     req(input$Sentiment_type)
     box_plot(filtered_df(), aggregation = input$aggregation,input$Sentiment_type,
              input$facet,input$tweet_length)})

}
