server <- function(input, output, session) {

  ############################################################# Stocks
  # load stock dataset
  stockdata_DE <- reactive({
    #req(input$Stock)
    #stock_dataset_DE(input$Stock,input$dates[1],input$dates[2])
    load_all_stocks_DE()
  })

  stockdata_US <- reactive({
    #req(input$Stock)
    #stock_dataset_DE(input$Stock,input$dates[1],input$dates[2])
    load_all_stocks_US()
  })


  # reset button for stock selection
  observeEvent(input$reset,{
    updateSelectizeInput(session,"Stock",selected = "")
  })
  # plot of the stocks
  output$plot_DE <- renderPlot({
    req(input$Stock)
    plotdata <- filter(stockdata_DE(),
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]],"GDAXI")[c(COMPONENTS_DE()[["Company.Name"]],"GDAXI") %in% .env$input$Stock]) &
                         .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    if (!is.null(ranges$x)) {
      ranges$x <- as.Date(ranges$x, origin = "1970-01-01")
    }
    ggplot(plotdata,aes(Dates,Close,color = name))+
      geom_line()+
      theme_classic()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  # hover info box
  output$hover_info_DE <- renderUI({
    req(input$hovering)
    create_hover_info_DE(input$plot_hover_DE,stockdata_DE())
  })
  # zoom functionality
  ranges <- reactiveValues(x = NULL, y = NULL)
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
  #####################################################################

  ##################################################################### Corona

  corona_data <- reactive({
    CORONA(input$CoronaCountry,input$dates_corona[1],input$dates_corona[2])
  })

  output$corona_plot <- renderPlot({
    if (!is.null(ranges2$x)) {
      ranges2$x <- as.Date(ranges2$x, origin = "1970-01-01")
    }

    ggplot(corona_data(), aes_string("date",input$corona_measurement,color = "location"))+
      geom_line() +
      theme_classic() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })

  # hover info box
  output$hover_info_corona <- renderUI({
    req(input$hovering_corona)
    create_hover_info_corona(input$plot_hover_corona, corona_data(),input$corona_measurement)
  })

  # zoom functionality
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_corona_dblclick, {
    brush <- input$plot_corona_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

  ##################################################################################### Granger

  # both <- reactive({
  #   req(input$Stock_Granger)
  #   granger_data1 <-  stock_dataset_DE(input$Stock_Granger,input$date_granger[1],input$date_granger[2])[c("Dates",input$Granger_outcome)]
  #   granger_data1["zweitevariable"] <- stock_dataset_DE("adidas ",input$date_granger[1],input$date_granger[2])[["Open"]]
  #   granger_data1
  #   #cbind(granger_data1[input$Granger_outcome],granger_data2["Open"])
  # })

  granger_data <- reactive({
    granger1 <- filter(stockdata_DE(),
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Granger]) &
                         .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]
    granger1["zweitevariable"] <- filter(stockdata_DE(),
                                         .data$name == "ADS.DE" &
                                           .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[["Open"]]
    granger1
  })

  optlags <- reactive({
    #library(vars)
    VARselect(granger_data()[-1],lag.max = 10, type = "const")$selection[["AIC(n)"]]
  })

  dickey_fuller <- reactive({
    data <- granger_data()
    while (adf.test(data[[2]],k=optlags())$p.value > 0.1 | adf.test(data[[3]],k=optlags())$p.value > 0.1){
      data[2] <- c(diff(data[[2]],1),NA)
      data[3] <- c(diff(data[[3]],1),NA)
      data <- drop_na(data)
    }
    data
  })

  granger_result <- reactive({
    varobject <- VAR(dickey_fuller()[-1], p = optlags(), type = "const")
    cause <- NULL
    ifelse(input$direction_granger == TRUE,cause <- "zweitevariable",cause <- input$Granger_outcome)
    granger <- causality(varobject, cause = cause)
    granger$Granger
  })

  output$granger_result <- renderPrint({
    granger_result()})

  output$stocks_granger <- renderPlot({
    ggplot(granger_data(),aes_string("Dates",input$Granger_outcome))+
      geom_line()
  })
  output$dickey <- renderUI({
    str1 <- paste("The optimal lag order for the VAR model is ",optlags()," lags")
    if (nrow(dickey_fuller()) != nrow(granger_data())){
      str2 <- paste("The Dickey Fuller test found one of the timeseries to be non-stationary.")
      str3 <- paste("Differencing the series ",nrow(granger_data()) - nrow(dickey_fuller()),"times achieved stationarity")
    } else {
      str2 <-paste("The Dickey Fuller test found both timeseries to be stationary.")
      str3 <-paste("Hence, the granger causality analysis can be performed without tranformations")
    }
    HTML(paste(str1,str2,str3, sep = '<br/>'))
  })

  output$granger_satz <- renderUI({
    if(input$direction_granger == TRUE){
      if (granger_result()["p.value"] < 0.1){
        str1 <- paste("Zweitevariable granger causes ",input$Granger_outcome,"of",input$Stock_Granger)
        } else {
          str1 <- paste("Zweitevariable does not granger cause ",input$Granger_outcome,"of",input$Stock_Granger)
        }
    } else {
      if (granger_result()["p.value"] < 0.1){
        str1 <- paste(input$Granger_outcome,"of",input$Stock_Granger, "granger causes Zweitevariable")
        } else {
          str1 <- paste(input$Granger_outcome,"of",input$Stock_Granger, "does not granger cause Zweitevariable")
        }
      }
    HTML(paste(str1))
  })

  output$info_granger <- renderUI({
    str1 <- paste("In this section, the user is able to perform a Granger causality test, which is a statistical hypothesis test for determining whether one time series is useful in forecasting another.
                  The term 'causality' in this context means nothing more than predictive causality and should not be mistaken for
                  'true causality'. It rather measures the ability of past values of one time series to predict future values of another time series.
                  ","<br/>")
    str2 <- paste("The following steps are automatically performed after the user selects two time series : ","<br/>",
                  "1. The optimal number of lags is calculated","<br/>",
                  "2. Stationarity is repeatedly tested and the series are differenced until sationarity is achieved","<br/>",
                  "3. A VAR model is estimated with the optimal number of lags and the (if necessary) transformed series","<br/>",
                  "4. A granger causality test is performed.")
    HTML(paste(str1,str2,sep = '<br/>'))
  })

  ################################################################################################### Regression

  ###flexible input for stocks: show either german or us companies
  output$stock_regression <- renderUI({
    if (input$country_regression == "Germany"){
           input <- selectizeInput("Stock_Regression","Choose dependent variable:",
                                 c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                                 selected = "Bayer ",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression","Choose dependent variable:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = FALSE)
    }
  })

  dataset_rec <- reactive({
    res <- dataset()
  })
  
  output$controls <- renderUI({
    res <- dataset_rec()
    res$name <- NULL
    input <- selectizeInput("Controls","Choose control variables:",
                   c(colnames(res[2:length(res)])),multiple = TRUE
    )
    
  })
  
  dataset <- reactive({
    if (input$country_regression == "Germany"){
    data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression]) &
                         .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates","Close","name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression]) &
                           .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates","Close","name")] #hier später noch CLose flexibel machen
  }

  if (input$country_regression == "Germany"){
    global_controls <- global_controls_test_DE()
    global_controls$Date = as.Date(global_controls$Date)
    dax <- GDAXI()
    dax$Date = as.Date(dax$Date, "%d %b %Y")
    dax <- missing_date_imputer(dax,"Close.")
    global_controls <- left_join(dax,global_controls,by = c("Date"))
    names(global_controls)[2] <- "DAX"
    
  }else {
    global_controls <- global_controls_test_US()
    global_controls$Date = as.Date(global_controls$Date)
    dow <- DOW()
    dow$Date = as.Date(dow$Date, "%d %b %Y")
    dow <- missing_date_imputer(dow,"Close.")
    global_controls <- left_join(dow,global_controls,by = c("Date"))
    names(global_controls)[2] <- "DOW"
  }
  names(global_controls)[1] <- "Dates"
  data_reg2 <- left_join(data_reg,global_controls,by = c("Dates")) #hierdurch kommt die varible "global" in den datensatz
      ##diesen datensatz filtern wir dann nochmal mit dem sliderinput für die kontrollvariablen(eine/keine/mehrere möglich)
  data_reg2
  })


  df_selected_controls <- reactive({
    req(input$Controls)
    res <- dataset_rec()
    res <- res[c("Dates",input$Controls)]
    
  }) # add sentiment to this dataframe

  observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    req(input$Sentiment_type)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
  })
  
  observeEvent(input$industry_sentiment, {                         #Observe event from input (model choices)
    req(input$industry_sentiment)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment)
  })
  
  dataset_senti <- reactive({
    req(input$Sentiment_type)
    if(input$Sentiment_type == "NoFilter"){
      
      res <- En_NoFilter_0_0_yes()
      #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
      #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
      #input$language
    }else{
      req(input$Stock)
       #ticker <- ticker_dict(input$Stock)
       #res <- eval(parse(text = paste(ticker,'()', sep='')))
        res <- ADS.DE()
    }
    
    
  })
  
  filtered_df <- reactive({
    req(input$Sentiment_type)
    req(input$minRetweet_stocks1)
    req(input$minRetweet_stocks2)
    
    if(input$Sentiment_type == "NoFilter"){
      
      res <- dataset_senti()
    }else{ # live filtering
      req(input$industry_sentiment)
      res <- dataset_senti()
      if(input$industry_sentiment == "no"){
        res <- dataset_senti()
        if(input$tweet_length_stock1 == "yes"){
          
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)) &
                                  (tweet_length > 81))}
        else{
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)))
        }
      }else{
        res <- dataset_senti()
        if(input$tweet_length_stock2 == "yes"){
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
                                  (tweet_length > 81))
        }else{
          res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
        }
      }
    }
  })
  
  
  aggri_select <- reactive({
    
    listi = c("Mean weighted by likes","Mean weighted by length","Mean weighted by retweets","Mean")
    
    if(input$Sentiment_type == "NoFilter"){
      listio = listi[which(listi %in% input$aggregation)]
      res <- filtered_df()
      res <- Multiple_input(res,input$aggregation,listio,key())
    }else{
      if(input$industry_sentiment == "no"){ 
        listi1 = listi[which(listi %in% input$aggregation1)]
        res <- filtered_df()
        res <- aggregate_sentiment(res)
        res <- res %>% filter(language == input$language1)
        res <- Multiple_input(res,input$aggregation1,listi1,key())
       }else{
        listi2 = listi[which(listi %in% input$aggregation2)]
        res <- get_industry_sentiment(COMPONENTS_DE(),input$industry,input$minRetweet_stocks2)
        res <- res %>% filter(language == input$language2)
        res <- Multiple_input(res,input$aggregation2,listi2,key())
        }
    }
    
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
  
  
  
  #merge
  final_regression_df <- reactive ({
    res <- aggri_select()
    res$date <- as.Date(res$date)
    res_c <- df_selected_controls()
    res <- left_join(res,res_c, by=c("date" = "Dates"))
    res
  })
  
  
  output$testi_table <- renderPrint ({
    head(aggri_select())
  })
  
  output$senti <- renderPrint ({
    head(df_selected_controls())
  })

  output$senti_agg <- renderPrint ({
    head(final_regression_df())
  })
}
