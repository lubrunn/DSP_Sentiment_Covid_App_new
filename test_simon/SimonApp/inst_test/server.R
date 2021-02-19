
function(input, output, session) {

  observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    req(input$Sentiment_type)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
  })

  observeEvent(input$reset,{
    updateSelectizeInput(session,"aggregation",selected = "")
  })
  observeEvent(input$reset1,{
    updateSelectizeInput(session,"aggregation1",selected = "")
  })

  dataset <- reactive({
    req(input$Sentiment_type)
    if (input$Sentiment_type == "NoFilter"){
      req(input$minRetweet)
      fil <- Range_input(input$minRetweet)
      res <- eval(parse(text = paste('En', '_NoFilter_',fil,'()', sep='')))
      #input$language
    }else{
      req(input$Stock)
      ticker <- ticker_dict(input$Stock)
      res <- eval(parse(text = paste(ticker,'()', sep='')))
        }


  })


  filtered_df <- reactive({ # subset pre-filtered dataset
    req(input$Sentiment_type)
    req(input$minRetweet)
    req(input$minLikes)
    req(input$tweet_length)
    req(input$minRetweet_stocks)
    req(input$tweet_length_stock)

    if (input$Sentiment_type == "NoFilter"){

      res <- dataset()
      res <- res %>% filter((retweet_filter == input$minRetweet) &
                              (likes_filter == input$minLikes) &
                              ((long_tweet == "yes")|(long_tweet == "no")))
    }else{ # live filtering

        res <- dataset()
         if (input$tweet_length_stock == "yes"){

           res <- res %>% filter((retweets_count > input$minRetweet_stocks) &
                       #(likes_count > input$minLikes) &
                       (tweet_length > median(tweet_length)))
        }else{

          res <- res %>% filter(retweets_count > input$minRetweet_stocks)
                                #(likes_count > input$minLikes) &
          }

    #req(input$industry)

    #if(input$industry == "Himmel hilf") {
     #  res <- get_industry_sentiment(COMPONENTS_EN(),COMPONENTS_DE(),input$industry)
      #  }
    }


  })


  #time just for range of plot
  #res <- res %>% filter( (date > input$timeWindow[1]) &
  #                     (date < input$timeWindow[2]))



    # max_retweet <- reactive(({
    #   test_data <- filtered_df()
    #   max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
    #   min(max_val_vec$maxi)
    # }))
    #
    # observe({
    #   updateSliderInput(session, "minRetweet_stocks", max = max_retweet())
    # })




  output$plot1 <- renderPlot({
    req(input$aggregation)
    req(input$Sentiment_type)
    req(input$industry_sentiment)
    req(input$language)
    req(input$aggregation1)

    TS_plot(filtered_df(),input$aggregation,input$aggregation1,input$Sentiment_type,
            input$facet,input$tweet_length,input$industry_sentiment,input$language)})

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

