
function(input, output, session) {

  observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    req(input$Sentiment_type)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
  })

  observeEvent(input$industry_sentiment, {                         #Observe event from input (model choices)
    req(input$industry_sentiment)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment)
  })


  observeEvent(input$reset,{
    updateSelectizeInput(session,"aggregation",selected = "")
  })
  observeEvent(input$reset1,{
    updateSelectizeInput(session,"aggregation1",selected = "")
  })
  observeEvent(input$reset2,{
    updateSelectizeInput(session,"aggregation2",selected = "")
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

   components_de <- reactive({
     res <- COMPONENTS_DE()
   })
   components_en <- reactive({
     res <- COMPONENTS_EN()
   })

  filtered_df <- reactive({ # subset pre-filtered dataset
    req(input$Sentiment_type)
    req(input$minRetweet)
    req(input$minLikes)
    req(input$tweet_length1)
    req(input$tweet_length2)
    req(input$minRetweet_stocks1)
    req(input$minRetweet_stocks2)
    req(input$tweet_length_stock1)
    req(input$tweet_length_stock2)

    if (input$Sentiment_type == "NoFilter"){

      res <- dataset()
      res <- res %>% filter((retweet_filter == input$minRetweet) &
                              (likes_filter == input$minLikes) &
                              ((long_tweet == "yes")|(long_tweet == "no")))
    }else{ # live filtering

          res <- dataset()
          if (input$tweet_length_stock1 == "yes"){
              res <- res %>% filter((retweets_count > input$minRetweet_stocks1) &
                       (tweet_length > median(tweet_length1)))
          }else{
              res <- res %>% filter(retweets_count > input$minRetweet_stocks1)
          }

          if(input$tweet_length_stock2 == "yes"){
              res <- res %>% filter((retweets_count > input$minRetweet_stocks2) &
                                    (tweet_length > median(tweet_length2)))
          }else{
              res <- res %>% filter(retweets_count > input$minRetweet_stocks2)
          }
    }
  })



    max_retweet <- reactive({
      req(input$industry)
      req(input$industry_sentiment)

      if(input$industry_sentiment == "no"){
      test_data <- filtered_df()
      max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
      min(max_val_vec$maxi)}
      else if(input$industry_sentiment == "yes"){
        test_data <- filtered_df()
        test_data <- get_industry_sentiment_nofiltering(COMPONENTS_DE(),input$industry)
        max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
        min(max_val_vec$maxi)}
    })

    observe({
      updateSliderInput(session, "minRetweet_stocks1", max = max_retweet())
    })

    observe({
      updateSliderInput(session, "minRetweet_stocks2", max = max_retweet())
    })



  output$plot1 <- renderPlot({
    #req(input$aggregation)
    #req(input$aggregation2)
    #req(input$Sentiment_type)
    #req(input$industry_sentiment)
    #req(input$language1)
    #req(input$language2)
   # req(input$aggregation1)
  #  req(input$industry)
##    req(input$minRetweet_stocks2)

    TS_plot(filtered_df(),input$aggregation,input$aggregation1,input$aggregation2,input$Sentiment_type,
            input$facet,input$tweet_length,input$industry_sentiment,input$language1,input$language2,
            components_de(),input$industry,input$minRetweet_stocks2)
    })

  output$plot2 <- renderPlot({
    req(input$aggregation)
    req(input$Sentiment_type)
    req(input$industry_sentiment)
    req(input$language)
    req(input$aggregation1)

    density_plot(filtered_df(),input$aggregation,input$aggregation1,input$Sentiment_type,
                 input$facet,input$tweet_length,input$industry_sentiment,input$language)})

  output$plot3 <- renderPlot({
    req(input$aggregation)
    req(input$Sentiment_type)
    req(input$industry_sentiment)
    req(input$language)
    req(input$aggregation1)

    box_plot(filtered_df(),input$aggregation,input$aggregation1,input$Sentiment_type,
             input$facet,input$tweet_length,input$industry_sentiment,input$language)})

}

