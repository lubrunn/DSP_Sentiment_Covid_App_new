parameter_tabsi <- tabsetPanel(
  id = "industry_tab",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks(COMPONENTS_DE(),COMPONENTS_EN()),
           radioButtons("language1","Language of tweets ?",
                        choices = c("en","de")),
           selectizeInput("aggregation1", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          multiple = T, select = "Mean"),
           actionButton("reset1", "clear selected"),
           sliderInput("minRetweet_stocks1", "Select minimum number of retweets", min = 0,value = 0,
                       max = 1,step = 1),
           radioButtons("tweet_length_stock1","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no")


  ),
  tabPanel("yes",
           selectInput("industry", "Industry", choices = c("Consumer Cyclical","Financial Services")),

           radioButtons("language2","Language of tweets ?",
                        choices = c("en","de")),

           selectizeInput("aggregation2", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          multiple = T, select = "Mean"),
           actionButton("reset2", "clear selected"),
           sliderInput("minRetweet_stocks2", "Select minimum number of retweets", min = 0,value = 0,
                       max = 1,step = 1),
           radioButtons("tweet_length_stock2","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no"))

)






parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("NoFilter",
           #radioButtons("language","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                          "Mean weighted by likes", "Mean weighted by length"),
                           multiple = T, select = "Mean"),
           actionButton("reset", "clear selected"),
           sliderInput("minRetweet", "Select minimum number of retweets", min = 0,value = 0,
                       max = 300,step = 1),
           sliderInput("minLikes", "Select minimum number of likes", min = 0,value = 0,
                       max = 300,step = 1),
           radioButtons("tweet_length","Tweet larger than median length:",
                        choices = c("yes","no")),
           selectInput("facet","Facet by column",
                        choices = c("No Faceting","Long-Short tweet"))


  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no"),
           parameter_tabsi

  )

)






ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("Sentiment_type", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                  selected = "NoFilter"),

      selectInput("plotType", "Plot", choices = c("Time Series","Density","Box Plot"),
                  selected = "Time Series"),

      parameter_tabs
  ),
    mainPanel(
      conditionalPanel(
        condition = "input.plotType == 'Time Series'", plotOutput("plot1")),
      conditionalPanel(
        condition = "input.plotType == 'Density'", plotOutput("plot2")),
      conditionalPanel(
        condition = "input.plotType == 'Box Plot'", plotOutput("plot3"))
    )
  )
)
