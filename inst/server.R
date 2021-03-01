server <- function(input, output, session) {

  ############################################################# Stocks
  # load stock dataset
  stockdata_DE <- reactive({
    #req(input$Stock)
    #stock_dataset_DE(input$Stock,input$dates[1],input$dates[2])
  aa <- load_all_stocks_DE()
  })

  stockdata_US <- reactive({
    #req(input$Stock)
    #stock_dataset_DE(input$Stock,input$dates[1],input$dates[2])
    load_all_stocks_US()
  })

  output$stock_choice <- renderUI({
    if (input$country_stocks == "Germany"){
      input <- selectizeInput("Stock","Choose Companies:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = TRUE)
    } else {
      input <- selectizeInput("Stock","Choose Companies:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = TRUE)
    }
  })



  # reset button for stock selection
  observeEvent(input$reset,{
    updateSelectizeInput(session,"Stock",selected = "")
  })
  # plot of the stocks
  output$plot_DE <- renderPlot({
    req(input$Stock)
    if (input$country_stocks == "Germany"){
      plotdata <- filter(stockdata_DE(),
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]],"GDAXI")[c(COMPONENTS_DE()[["Company.Name"]],"GDAXI") %in% .env$input$Stock]) &
                         .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    } else {
      plotdata <- filter(stockdata_US(),
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock]) &
                           .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    }

    if (!is.null(ranges$x)) {
      ranges$x <- as.Date(ranges$x, origin = "1970-01-01")
    }
    ggplot(plotdata,aes_string("Dates",input$stock_outcome,color = "name"))+
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




  output$Stock_Granger <- renderUI({
    if (input$country_granger == "Germany"){
      input <- selectizeInput("Stock_Granger","Choose dependent variable:",
                              c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              selected = "Bayer ",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Granger","Choose dependent variable:",
                              c(COMPONENTS_US()[["Company.Name"]],"DOW"),
                              selected = "Apple ",multiple = FALSE)
    }
  })


  granger_data <- reactive({
    req(input$Stock_Granger)
    if (input$country_granger == "Germany"){
    granger1 <- filter(stockdata_DE(),
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Granger]) &
                         .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]
    } else {
      granger1 <- filter(stockdata_US(),
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Granger]) &
                           .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]

    }
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
    req(input$Granger_outcome)
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

  # dataset_rec <- reactive({
  #   res <- dataset()
  # })

  output$Controls <- renderUI({
    #res <- dataset()
    #res$name <- NULL
    if (input$country_regression == "Germany"){
     input <- selectizeInput("Controls","Choose control variables:",
                        c(colnames(global_controls_test_DE())[-1],"DAX"),multiple = TRUE)
                   #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
        input <- selectizeInput("Controls","Choose control variables:",
                                c(colnames(global_controls_test_US())[-1],"DOW"),multiple = TRUE)
       }

  })

  dataset <- reactive({
    if (input$country_regression == "Germany"){
    data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                       .data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression]) &
                         .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates",input$regression_outcome,"name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DOW")[c(COMPONENTS_US()[["Company.Name"]], "DOW") %in% .env$input$Stock_Regression]) &
                           .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates",input$regression_outcome,"name")] #hier später noch CLose flexibel machen
   }

  if (input$country_regression == "Germany"){
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


  df_selected_controls <- reactive({
    req(input$Controls)
    res <- dataset()
    res <- res[c("Dates",input$regression_outcome,input$Controls)]
    res
  })

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
  aggri_select <- reactive({

    if(input$Sentiment_type == "NoFilter"){ # NoFilter files already aggregated
      res <- filtered_df()
      aggregation <- key(input$aggregation)  # select aggregation type: Mean, mean weighted by,...
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      if(input$industry_sentiment == "no"){
        res <- filtered_df()
        res <- aggregate_sentiment(res) # function to aggregate sentiment per day
        res <- res %>% filter(language == input$language1)
        aggregation <- key(input$aggregation1)
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
       }else{
        res <- get_industry_sentiment(COMPONENTS_DE(),input$industry,input$minRetweet_stocks2,
                                      input$tweet_length_stock2)      #function to gather all stock in certain industry
        aggregation <- key(input$aggregation2)                          #--> also calculates aggregation inside function
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
        }
    }

  })

  observeEvent(input$reset_regression,{
    updateSelectizeInput(session,"Controls",selected = "")
  })



  #merge sentiment with control+dep vars
  final_regression_df <- reactive ({
    res <- aggri_select()
    res$date <- as.Date(res$date)
    res_c <- df_selected_controls()
    res <- left_join(res_c,res, by=c("Dates" = "date"))
    res <- res[-1]
    res
  })

  #regression
  regression_result <- reactive({
    model <- lm(reformulate(".",input$regression_outcome), data = final_regression_df())
    summary(model)
  })

  #Qregression
  regression_result_Qreg <- reactive({
    model <- rq(reformulate(".",input$regression_outcome),tau = 0.5,data = final_regression_df())
    summary(model)
  })


  output$testi_table <- renderPrint ({
    head(dataset())
  })

  output$senti <- renderPrint ({
    head(df_selected_controls())
  })

  output$senti_agg <- renderPrint ({
    head(final_regression_df())
  })

  output$regression_result <- renderPrint({
    regression_result()})

  output$regression_equation <- renderUI({
    req(input$Controls)
    str1 <- paste("Linear regression: ",input$regression_outcome,"of ",input$Stock_Regression,"~",paste(input$Controls,collapse = " + "),"<br/>")
    HTML(paste(str1,sep = '<br/>'))
  })


  output$plot_dens_Qreg <- renderPlot({

    density_plot_reg(dataset())
  })

  output$regression_result_Qreg <- renderPrint({
    regression_result_Qreg()})



  ############################################################################
  ################# Directory ###############################################
  ###########################################################################
  # selecting directory
  # find home direcoty of user
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  # allow for searching directories
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  observe({
    cat("\ninput$directory value:\n\n")
    print(input$directory)
  })
  path_setter <- reactive({
    #browser()
    if (is.integer(input$directory)) {
      setwd(volumes)

      cat(glue("No directory has been selected. Current directory {getwd()})"))

    } else {

      path <- shinyFiles::parseDirPath(volumes, input$directory)
      setwd(path)
      con <- DBI::dbConnect(RSQLite::SQLite(), "SQLiteStudio/databases/test.db")

      file_needed <- "SQLiteStudio"
      if(dir.exists(file_needed)) {
        #setwd(file_path)
        output_str <- glue("Current path {getwd()}")
      } else {
        #setwd(file_path)
        output_str <- "Current path selection does not seem correct. \n
                Are you sure it is set correctly?"
      }

      path_outputs <- list(a = con, b = output_str)
      path_outputs


    }
  })
  output$directorypath <- renderText({
    path_outputs <- path_setter()
    path_outputs[[2]][1]
  })





  ###############################################################################
  ############################### twitter descriptive ###########################
  ###############################################################################
  ### for histogram less choices
  observeEvent(input$plot_type,{

    if (input$plot_type == "histo"){
      if (input$value %in% c("sentiment_rt", "sentiment_likes", "sentiment_length")){
        selected_value <-"sentiment"
      } else {
        selected_value <- input$value
      }

      updateSelectInput(session = session, "value",
                        choices = c("Sentiment" = "sentiment",
                                    "Retweets" = "rt",
                                    "Likes"="likes",
                                    "Tweet Length" = "tweet_length"
                        ), selected = selected_value)
    } else {

      updateSelectInput(session = session, "value",
                        choices = c(
                          "Sentiment" = "sentiment",
                          "Retweets Weighted Sentiment" = "sentiment_rt",
                          "Likes Weighted Sentiment" = "sentiment_likes",
                          "Length Weighted Sentiment" = "sentiment_tweet_length",
                          "Retweets" = "rt",
                          "Likes"="likes",
                          "Tweet Length" = "tweet_length",
                          "Number of Tweets" = "N"
                        ), selected = input$value)
    } #if clause
  }) #observeevent

  # avoid that date range upper value can be lower than lower value
  # Update the dateRangeInput if start date changes
  observeEvent(input$dates[1], {
    end_date = input$dates[2]
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if (input$dates[2] < input$dates[1]) {
      end_date = input$dates[1]
    }
    updateDateRangeInput(session,"dates", start=input$dates[1], end=end_date, min=input$dates[1] )
  })


  ######## disconnect from database after exit
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })




  querry <- reactive({

    if (input$long == T){
      long <- 81
    } else{
      long <- 0
    }
    #browser()
    if (input$plot_type == "sum_stats"){

      if (input$value == "N"){
        metric <- "N"
      } else if (input$value == "tweet_length"){
        metric <- glue("{input$metric}_length")
      } else{
        metric <- glue("{input$metric}_{input$value}")
      }



      table_name <- glue("{input$plot_type}_{tolower(input$lang)}_all")
      # browser()
      glue("SELECT created_at, {metric} as value FROM {table_name}  WHERE created_at >= '{input$dates[1]}'
      and created_at <= '{input$dates[2]}'
         and retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long}" )


    } else if (input$plot_type == "histo"){
      #browser()
      if (input$value == "length"){
        tb_metric <- "len"
        col_value <- input$value

      } else if(input$value == "rt") {

        tb_metric <- input$value
        col_val <- "retweets_count"

      } else if(input$value == "likes") {
        tb_metric <- input$value
        col_val <- "likes_count"
      } else if(input$value == "sentiment"){
        tb_metric <- input$value
        col_val <- "sentiment_rd"
      } else{
        Sys.sleep(0.2)
      }





      table_name <- glue("{input$plot_type}_{tb_metric}_{tolower(input$lang)}")

      if (table_name %in% c("histo_rt_en", "histo_likes_en", "histo_len_en")){
        date_col <- "date"
      } else{
        date_col <- "created_at"
      }

      glue("SELECT {col_val}, sum(N) as  n  FROM {table_name}  WHERE {date_col} >=  '{input$dates[1]}'
      and {date_col} <= '{input$dates[2]}'
      and retweets_count_filter = {input$rt} and likes_count_filter = {input$likes} and
      tweet_length_filter = {long}
      group by {col_val}")

    }
    #browser()


  })

  data <- reactive({


    con <- path_setter()
    con <- con[[1]]
    df_need <- DBI::dbGetQuery(con, querry())
    df_need
  })




  output$sum_stats_plot <- renderPlot({

    df <- data()


    if(input$plot_type == "sum_stats"){

      df$created_at <- as.Date(df$created_at)
      df %>%
        ggplot(aes(x = created_at,
                   y = value)) +
        geom_line()
    }

  })


  observeEvent(input$value, {
    #browser()
    if (input$value == "sentiment") {
      shinyWidgets::updateSwitchInput(session = session,
                                      "log_scale",
                                      disabled = T)
    } else {
      shinyWidgets::updateSwitchInput(session = session,
                                      "log_scale",
                                      disabled = F)
    }
  })



  output$histo_plot <- renderPlot({
    df <- data()

    #freezeReactiveValue(input, "plot_type")

    # if sentiment then disable log button because has negative values



    if (input$plot_type == "histo"){

      df %>%
        # {if (input$log_scale == T) {} else {
        #          mutate(bins = cut_interval(.[[1]], n = input$bins))
        #        }
        # }

        mutate(metric = case_when(input$log_scale == T ~ log(as.numeric(.[[1]])+ 0.0001),
                                  input$log_scale == F ~ as.numeric(.[[1]])),
               bins = cut_interval(metric, n = input$bins))%>%

        ggplot(aes(bins, n)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    }
  })


}
