###flexible input for stocks: show either german or us companies
output$stock_regression_var <- renderUI({
  if (input$country_regression_var == "Germany"){
    input <- selectizeInput("Stock_Regression_var","Choose dependent variable:",
                            c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                            selected = "Bayer ",multiple = FALSE)
  } else {
    input <- selectizeInput("Stock_Regression_var","Choose dependent variable:",
                            c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                            selected = "Apple ",multiple = FALSE)
  }
})


output$Controls_var <- renderUI({
  if (input$country_regression_var == "Germany"){
    input <- selectizeInput("Controls_var","Choose control variables:",
                            c(colnames(global_controls_test_DE())[-1],"DAX"),multiple = TRUE)
    #c(colnames(res[3:length(res)])),multiple = TRUE
  }else{
    input <- selectizeInput("Controls_var","Choose control variables:",
                            c(colnames(global_controls_test_US())[-1],"DOW"),multiple = TRUE)
  }

})

dataset_var <- reactive({
  if (input$country_regression_var == "Germany"){
    data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression_var]) &
                         .data$Dates >= .env$input$date_regression_var[1] & .data$Dates <= .env$input$date_regression_var[2])[c("Dates",input$regression_outcome_var,"name")] #hier später noch CLose flexibel machen
  } else {
    data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression_var]) &
                         .data$Dates >= .env$input$date_regression_var[1] & .data$Dates <= .env$input$date_regression_var[2])[c("Dates",input$regression_outcome_var,"name")] #hier später noch CLose flexibel machen
  }

  if (input$country_regression_var == "Germany"){
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


df_selected_controls_var <- reactive({
  #req(input$Controls_var)
  res <- dataset_var()
  res <- res[c("Dates",input$regression_outcome_var,input$Controls_var)]
  res
})

observeEvent(input$Sentiment_type_var, {                         #Observe event from input (model choices)
  req(input$Sentiment_type_var)
  updateTabsetPanel(session, "params", selected = input$Sentiment_type_var)
})

observeEvent(input$industry_sentiment_var, {                         #Observe event from input (model choices)
  req(input$industry_sentiment_var)
  updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment_var)
})

dataset_senti_var <- reactive({
  req(input$Sentiment_type_var)
  if(input$Sentiment_type_var == "NoFilter"){

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
filtered_df_var <- reactive({
  req(input$Sentiment_type_var)
  req(input$minRetweet_stocks1_var)
  req(input$minRetweet_stocks2_var)

  if(input$Sentiment_type_var == "NoFilter"){

    res <- dataset_senti_var()
  }else{ # live filtering
    req(input$industry_sentiment_var)
    res <- dataset_senti_var()
    if(input$industry_sentiment_var == "no"){
      res <- dataset_senti_var()
      if(input$tweet_length_stock1_var == "yes"){

        res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_var)) &
                                (tweet_length > 81))}
      else{
        res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_var)))
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
aggri_select_var <- reactive({

  if(input$Sentiment_type_var == "NoFilter"){ # NoFilter files already aggregated
    res <- filtered_df_var()
    aggregation <- key(input$aggregation_var)  # select aggregation type: Mean, mean weighted by,...
    res <- res %>% tidyr::gather("id", "aggregation", aggregation)
    res <- res[c("date","aggregation")]
  }else{
    if(input$industry_sentiment_var == "no"){
      res <- filtered_df_var()
      res <- aggregate_sentiment(res) # function to aggregate sentiment per day
      res <- res %>% filter(language == input$language1_var)
      aggregation <- key(input$aggregation1_var)
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      res <- get_industry_sentiment(COMPONENTS_DE(),input$industry_var,input$minRetweet_stocks2_var,
                                    input$tweet_length_stock2_var)      #function to gather all stock in certain industry
      aggregation <- key(input$aggregation2_var)                          #--> also calculates aggregation inside function
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }
  }

})

observeEvent(input$reset_regression_var,{
  updateSelectizeInput(session,"Controls",selected = "")
})



#merge sentiment with control+dep vars
final_regression_df <- reactive ({
  res <- aggri_select_var()
  res$date <- as.Date(res$date)
  res_c <- df_selected_controls_var()
  res <- left_join(res_c,res, by=c("Dates" = "date"))
  res <- res[-1]
  res
})
