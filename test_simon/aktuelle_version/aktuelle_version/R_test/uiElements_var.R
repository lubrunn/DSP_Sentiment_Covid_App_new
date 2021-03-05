parameter_tabsi_var <- tabsetPanel(
  id = "industry_tab_var",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks_reg(COMPONENTS_DE(),COMPONENTS_US()),
           radioButtons("language1_var","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           selectizeInput("aggregation1_var", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset1_var", "clear selected"),
           radioButtons("minRetweet_stocks1_var", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock1_var","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)


  ),
  tabPanel("yes",
           selectInput("industry_var", "Industry", choices = c("Consumer Cyclical","Financial Services")),

           radioButtons("language2_var","Language of tweets ?",
                        choices = c("en","de"),inline=T),

           selectizeInput("aggregation2_var", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset2_var", "clear selected"),
           radioButtons("minRetweet_stocks2_var", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock2_var","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)

  )

)


parameter_tabs_var <- tabsetPanel(
  id = "params_var",
  type = "hidden",
  tabPanel("NoFilter",
           radioButtons("language_var","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation_var", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                    "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           radioButtons("minRetweet_var", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("minLikes_var", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("tweet_length_var","Tweet larger than median length:",
                        choices = c("yes","no"),inline=T)


  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment_var","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no",inline=T),
           parameter_tabsi_var

  )

)


tabs_custom_var <- tabsetPanel(
  id = "regression_tabs_var",
  tabPanel("Model specifcation",
           radioButtons("country_regression_var","Which country?",c("Germany","USA"),selected = "Germany"),
           uiOutput("stock_regression_var"),
           radioButtons("regression_outcome_var","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           switchInput("senti_yesno","Include Sentiment?",onLabel="Yes",offLabel="No"),
           uiOutput("Controls_var"),
           actionButton("reset_regression_var", "clear selected"),
           #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
           sliderInput("date_regression_var",label = "Timeseries",
                       value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                       min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       step = 1,timeFormat = "%F")



  ),
  tabPanel("Filter sentiment input",
           selectInput("Sentiment_type_var", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                       selected = "NoFilter"),
           parameter_tabs_var

  )

)
