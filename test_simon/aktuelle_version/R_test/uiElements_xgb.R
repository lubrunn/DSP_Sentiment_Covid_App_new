parameter_tabsi_xgb <- tabsetPanel(
  id = "industry_tab_xgb",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks_reg(COMPONENTS_DE(),COMPONENTS_US()),
           radioButtons("language1_xgb","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           selectizeInput("aggregation1_xgb", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                         "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset1_xgb", "clear selected"),
           radioButtons("minRetweet_stocks1_xgb", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock1_xgb","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)


  ),
  tabPanel("yes",
           selectInput("industry_xgb", "Industry", choices = c("Consumer Cyclical","Financial Services")),

           radioButtons("language2_xgb","Language of tweets ?",
                        choices = c("en","de"),inline=T),

           selectizeInput("aggregation2_xgb", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                         "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset2_xgb", "clear selected"),
           radioButtons("minRetweet_stocks2_xgb", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock2_xgb","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)

  )

)


parameter_tabs_xgb <- tabsetPanel(
  id = "params_xgb",
  type = "hidden",
  tabPanel("NoFilter",
           radioButtons("language_xgb","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation_xgb", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                        "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           radioButtons("minRetweet_xgb", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("minLikes_xgb", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("tweet_length_xgb","Tweet larger than median length:",
                        choices = c("yes","no"),inline=T)


  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment_xgb","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no",inline=T),
           parameter_tabsi_xgb

  )

)


tabs_custom_xgb <- tabsetPanel(
  id = "regression_tabs_xgb",
  tabPanel("Model specifcation",
           radioButtons("country_regression_xgb","Which country?",c("Germany","USA"),selected = "Germany"),
           uiOutput("stock_regression_xgb"),
           radioButtons("regression_outcome_xgb","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #switchInput("senti_yesno","Include Sentiment?",onLabel="Yes",offLabel="No"),
           uiOutput("Controls_xgb"),
           actionButton("reset_regression_xgb", "clear selected"),
           #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
           sliderInput("date_regression_xgb",label = "Timeseries",
                       value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                       min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       step = 1,timeFormat = "%F")



  ),
  tabPanel("Filter sentiment input",
           selectInput("Sentiment_type_xgb", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                       selected = "NoFilter"),
           parameter_tabs_xgb

  )

)


