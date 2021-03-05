Sys.setlocale("LC_TIME", "English")



parameter_tabsi <- tabsetPanel(
  id = "industry_tab",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks_reg(COMPONENTS_DE(),COMPONENTS_US()),
           radioButtons("language1","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           selectizeInput("aggregation1", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset1", "clear selected"),
           radioButtons("minRetweet_stocks1", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock1","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)


  ),
  tabPanel("yes",
           selectInput("industry", "Industry", choices = c("Consumer Cyclical","Financial Services")),

           radioButtons("language2","Language of tweets ?",
                        choices = c("en","de"),inline=T),

           selectizeInput("aggregation2", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset2", "clear selected"),
           radioButtons("minRetweet_stocks2", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock2","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)

  )

)


parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("NoFilter",
           radioButtons("language","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                    "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           radioButtons("minRetweet", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("minLikes", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("tweet_length","Tweet larger than median length:",
                        choices = c("yes","no"),inline=T)


  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no",inline=T),
           parameter_tabsi

  )

)





tabs_custom <- tabsetPanel(
  id = "regression_tabs",
  tabPanel("Model specifcation",
           radioButtons("country_regression","Which country?",c("Germany","USA"),selected = "Germany"),
           uiOutput("stock_regression"),
           radioButtons("regression_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           uiOutput("Controls"),
           actionButton("reset_regression", "clear selected"),
           #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
           sliderInput("date_regression",label = "Timeseries",
                       value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                       min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       step = 1,timeFormat = "%F")



  ),
  tabPanel("Filter sentiment input",
           selectInput("Sentiment_type", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                       selected = "NoFilter"),
           parameter_tabs

  )

)




Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(
  #theme = shinythemes::shinytheme("cosmo"),
  shinythemes::themeSelector(),
  #titlePanel("Sentiment_Covid_App"),
  navbarPage("APP",
             tabPanel("Twitter"),
             tabPanel("Sentiment"),
             tabPanel("Stocks",
                      sidebarPanel(
                        radioButtons("country_stocks","Which country?",c("Germany","USA"),selected = "Germany"),
                        #selectize_Stocks(COMPONENTS_DE()),
                        uiOutput("stock_choice"),
                        radioButtons("stock_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
                        actionButton("reset", "clear selected"),
                        checkboxInput("hovering","Enable hover",value = FALSE),
                        sliderinput_dates()
                      ),
                      mainPanel(
                        plot_stocks_DE(),
                        hover_info_DE()
                      ),#close MainPanel
             ),#close tabPanel stock
             tabPanel("Corona",
                      sidebarPanel(
                        selectize_corona(),
                        checkboxGroupInput("CoronaCountry","Country",c("Germany","United States"),selected = "Germany"),
                        sliderinput_dates_corona(),
                        checkboxInput("hovering_corona","Enable hover",value = FALSE)
                      ),
                      mainPanel(
                        plot_corona(),
                        hover_info_corona()
                      )
             ),#close tabPanel Corona
             navbarMenu("Model",
                        tabPanel("Granger",
                                 sidebarPanel(
                                   radioButtons("country_granger","Which country?",c("Germany","USA"),selected = "Germany"),
                                   uiOutput("Stock_Granger"),
                                   # selectizeInput("Stock_Granger","Choose first argument: Company or Index",
                                   #                c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                                   #                selected = "Bayer ",multiple = FALSE),
                                   radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
                                   selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
                                   sliderInput("date_granger",label="Timeseries",
                                               value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                                               min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                                               max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                                               step = 1,timeFormat = "%F"),
                                   checkboxInput("direction_granger","Second variable causes first?",value = TRUE)
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information Granger",
                                              htmlOutput("info_granger")),
                                     tabPanel("Visualize",
                                              plotOutput("stocks_granger")),
                                     tabPanel("Background-steps",
                                              htmlOutput("dickey")),
                                     tabPanel("Results",
                                              verbatimTextOutput("granger_result"),
                                              htmlOutput("granger_satz"))))),
                        tabPanel("Regression Analysis",
                                 sidebarPanel(
                                   tabs_custom
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("regression",
                                              verbatimTextOutput("testi_table"),
                                              verbatimTextOutput("senti"),
                                              verbatimTextOutput("senti_agg"),
                                              htmlOutput("regression_equation"),
                                              verbatimTextOutput("regression_result")),
                                     tabPanel("Quantile Regression",
                                              plotOutput("plot_dens_Qreg"),
                                              verbatimTextOutput("regression_result_Qreg")

                                     )
                                   )
                                 )
                         ),
                        tabPanel("VAR-forecasting",
                                  sidebarPanel(

                                    tabs_custom_var,
                                    selectInput("correlation_type", "Chose type of correlation:", choices = c("ACF","PACF")),
                                    uiOutput("correlation_plot_choice"),
                                    numericInput("number_of_vars","Select number of variables which add AR/MA parts:",min = 1, value=1),
                                    numeric_features
                                  #  numericInput("numberARLags", "Number of autoregressive lags:",min=0),
                                  #  numericInput("numberMA", "Number of autoregressive lags:",min=0)
                                  ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Variable selection",
                                               verbatimTextOutput("datensatz_var"),
                                               verbatimTextOutput("summary"),
                                              conditionalPanel(
                                                condition = "input.correlation_type == 'ACF'", plotOutput("acf_plot_xgb")),
                                              conditionalPanel(
                                                condition = "input.correlation_type == 'PACF'", plotOutput("pacf_plot_xgb"))
                                              )
                                   )
                                 ))
                        )#close Navbarmenu
  )#close Navbarpage
)#close fluidpage
