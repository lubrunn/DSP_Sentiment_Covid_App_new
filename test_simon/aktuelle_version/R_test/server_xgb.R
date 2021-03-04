###flexible input for stocks: show either german or us companies
output$stock_regression_xgb <- renderUI({
  if (input$country_regression_xgb == "Germany"){
    input <- selectizeInput("Stock_Regression_xgb","Choose dependent variable:",
                            c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                            selected = "Bayer ",multiple = FALSE)
  } else {
    input <- selectizeInput("Stock_Regression_xgb","Choose dependent variable:",
                            c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                            selected = "Apple ",multiple = FALSE)
  }
})


output$Controls_xgb <- renderUI({
  if (input$country_regression_xgb == "Germany"){
    input <- selectizeInput("Controls_xgb","Choose control variables:",
                            c(colnames(global_controls_test_DE())[-1],"DAX"),multiple = TRUE)
    #c(colnames(res[3:length(res)])),multiple = TRUE
  }else{
    input <- selectizeInput("Controls_xgb","Choose control variables:",
                            c(colnames(global_controls_test_US())[-1],"DOW"),multiple = TRUE)
  }

})

dataset_xgb <- reactive({
  if (input$country_regression_xgb == "Germany"){
    data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression_xgb]) &
                         .data$Dates >= .env$input$date_regression_xgb[1] & .data$Dates <= .env$input$date_regression_xgb[2])[c("Dates",input$regression_outcome_xgb,"name")] #hier später noch CLose flexibel machen
  } else {
    data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression_xgb]) &
                         .data$Dates >= .env$input$date_regression_xgb[1] & .data$Dates <= .env$input$date_regression_xgb[2])[c("Dates",input$regression_outcome_xgb,"name")] #hier später noch CLose flexibel machen
  }

  if (input$country_regression_xgb == "Germany"){
    global_controls <- global_controls_test_DE()   #load controls
    global_controls$Date <- as.Date(global_controls$Date) #transform date
    dax <- GDAXI()  #load dax
    dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
    dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
    colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
    global_controls <- left_join(dax,global_controls,by = c("Date")) #join final

  }else {
    global_controls <- global_controls_test_US() #same procedure as above
    global_controls$Date <- as.Date(global_controls$Date)
    dow <- DOW()
    dow$Date <- as.Date(dow$Date, "%d %b %Y")
    dow <- missing_date_imputer(dow,"Close.")
    colnames(dow)[2] <- "DOW"
    global_controls <- left_join(dow,global_controls,by = c("Date"))
  }
  names(global_controls)[1] <- "Dates"
  data_reg2 <- left_join(data_reg,global_controls,by = c("Dates")) #hierdurch kommt die varible "global" in den datensatz
  ##diesen datensatz filtern wir dann nochmal mit dem sliderinput für die kontrollvariablen(eine/keine/mehrere möglich)
  data_reg2
})


df_selected_controls_xgb <- reactive({
  #req(input$Controls_var)
  res <- dataset_xgb()
  res <- res[c("Dates",input$regression_outcome_xgb,input$Controls_xgb)]
  res
})

observeEvent(input$Sentiment_type_xgb, {                         #Observe event from input (model choices)
  req(input$Sentiment_type_xgb)
  updateTabsetPanel(session, "params", selected = input$Sentiment_type_xgb)
})

observeEvent(input$industry_sentiment_xgb, {                         #Observe event from input (model choices)
  req(input$industry_sentiment_xgb)
  updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment_xgb)
})

dataset_senti_xgb <- reactive({
  req(input$Sentiment_type_xgb)
  if(input$Sentiment_type_xgb == "NoFilter"){

    res <- En_NoFilter_0_0_yes()   # still fix as it is not clear yet if sql or csv
    #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
    #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
    #input$language
  }else{
    req(input$Stock_reg)
    ticker <- ticker_dict(input$Stock_reg) # dict for a few stock
    res <- eval(parse(text = paste(ticker,'()', sep=''))) # example: ADS.DE()

  }


})
# filter
filtered_df_xgb <- reactive({
  req(input$Sentiment_type_xgb)
  req(input$minRetweet_stocks1_xgb)
  req(input$minRetweet_stocks2_xgb)

  if(input$Sentiment_type_xgb == "NoFilter"){

    res <- dataset_senti_xgb()
  }else{ # live filtering
    req(input$industry_sentiment_xgb)
    res <- dataset_senti_xgb()
    if(input$industry_sentiment_xgb == "no"){
      res <- dataset_senti_xgb()
      if(input$tweet_length_stock1_xgb == "yes"){

        res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_xgb)) &
                                (tweet_length > 81))}
      else{
        res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_xgb)))
      }
    }#else{
    #res <- dataset_senti()
    #if(input$tweet_length_stock2 == "yes"){
    # res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
    #                          (tweet_length > 81))
    #}else{
    #  res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
    #}
    #}
  }
})

# aggregate dataset to get one sentiment per day
aggri_select_xgb <- reactive({

  if(input$Sentiment_type_xgb == "NoFilter"){ # NoFilter files already aggregated
    res <- filtered_df_xgb()
    aggregation <- key(input$aggregation_xgb)  # select aggregation type: Mean, mean weighted by,...
    res <- res %>% tidyr::gather("id", "aggregation", aggregation)
    res <- res[c("date","aggregation")]
  }else{
    if(input$industry_sentiment_xgb == "no"){
      res <- filtered_df_xgb()
      res <- aggregate_sentiment(res) # function to aggregate sentiment per day
      res <- res %>% filter(language == input$language1_xgb)
      aggregation <- key(input$aggregation1_xgb)
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      res <- get_industry_sentiment(COMPONENTS_DE(),input$industry_xgb,input$minRetweet_stocks2_xgb,
                                    input$tweet_length_stock2_xgb)      #function to gather all stock in certain industry
      aggregation <- key(input$aggregation2_xgb)                          #--> also calculates aggregation inside function
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }
  }

})

observeEvent(input$reset_regression_xgb,{
  updateSelectizeInput(session,"Controls",selected = "")
})



#merge sentiment with control+dep vars
final_regression_df_xgb <- reactive ({
  res <- aggri_select_xgb()
  res$date <- as.Date(res$date)
  res_c <- df_selected_controls_xgb()
  res <- left_join(res_c,res, by=c("Dates" = "date"))
  res <- res[-1]
  res
})
