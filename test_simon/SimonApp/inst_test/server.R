
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
    if(input$Sentiment_type == "NoFilter"){

      res <- En_NoFilter_0_0_yes()
      #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
      #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
      #input$language
    }else{
      req(input$Stock)
      ticker <- ticker_dict(input$Stock)
      res <- eval(parse(text = paste(ticker,'()', sep='')))
        }


  })


  filtered_df <- reactive({
    req(input$Sentiment_type)
    req(input$minRetweet_stocks1)
    req(input$aggregation)
    req(input$aggregation1)
    req(input$language1)
    req(input$aggregation2)
    req(input$language2)
    req(input$industry)
    req(input$minRetweet_stocks2)
    req(input$industry_sentiment)
    req(input$tweet_length_stock2)

  listi = c("Mean weighted by likes","Mean weighted by length","Mean weighted by retweets","Mean")

  if(input$Sentiment_type == "NoFilter"){
    listio = listi[which(listi %in% input$aggregation)]
    res <- dataset()
    res <- Multiple_input(res,input$aggregation,listio,key())

  }else{ # live filtering
    res <- dataset()
      if(input$industry_sentiment == "no"){
        if(input$tweet_length_stock1 == "yes"){
          res <- dataset()
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)) &
                                  (tweet_length > 81))}
        else{
          res <- dataset()
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)))
        }
        listi1 = listi[which(listi %in% input$aggregation1)]
        res <- aggregate_sentiment(res)
        res <- res %>% filter(language == input$language1)
        res <- Multiple_input(res,input$aggregation1,listi1,key())

      }else{
        listi2 = listi[which(listi %in% input$aggregation2)]
        res <- get_industry_sentiment(COMPONENTS_DE(),input$industry,input$minRetweet_stocks2,
                                      input$tweet_length_stock2)
        res <- res %>% filter(language == input$language2)
        res <- Multiple_input(res,input$aggregation2,listi2,key())
      }
    }
  })



   #    max_retweet <- reactive({
   #      req(input$industry)
   #      req(input$industry_sentiment)
   #    if(input$industry_sentiment == "no"){
   #      test_data <- filtered_df()
   #      max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
   #      min(max_val_vec$maxi)}
   #    else{
   #        test_data <- filtered_df()
   #        test_data <- get_industry_sentiment_nofiltering(COMPONENTS_DE(),input$industry)
   #        max_val_vec <- test_data %>% group_by(date) %>%  summarise(maxi = max(retweets_count))
   #        min(max_val_vec$maxi)}
   #
   # })

      # observe({
      #   updateRadioButtons(session, "minRetweet_stocks1", choices = max_retweet())
      # })
      #
      # observe({
      #   updateRadioButtons(session, "minRetweet_stocks2", choices = c("0",max_retweet()))
      # })

  avg_tweets_per_day <- reactive({
      res <- filtered_df()
      avg_per_day <- mean(res$tweets_used_daily)
  })


  output$text_avg_tweet <- renderText({
    paste("The sentiment is calculated with an average of", round(avg_tweets_per_day()),
          "tweets per day.")
  })




  output$plot1 <- renderPlot({

    TS_plot(filtered_df(),input$Sentiment_type,input$industry_sentiment,
            input$facet,input$tweet_length,input$reg_line)
    })

  output$plot2 <- renderPlot({

    density_plot(filtered_df(),input$Sentiment_type,input$industry_sentiment,
                 input$facet,input$tweet_length)
    })

  output$plot3 <- renderPlot({


    box_plot(filtered_df(),input$Sentiment_type,input$industry_sentiment,
             input$facet,input$tweet_length)
    })

}

