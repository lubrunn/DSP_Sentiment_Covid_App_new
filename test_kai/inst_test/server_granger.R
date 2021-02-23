server <- function(input, output, session) {
  
  ############################################################# Stocks
  # load stock dataset
  stockdata_DE <- reactive({
    #req(input$Stock)
    #stock_dataset_DE(input$Stock,input$dates[1],input$dates[2])
    load_all_stocks_DE()
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
  
  
  
  
}