parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("NoFilter",
           selectizeInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                          "Mean weighted by likes", "Mean weighted by length"),
                           multiple = T, select = "Mean"),
           sliderInput("minRetweet", "Select minimum number of retweets", min = 0,value = 1,
                       max = 300,step = 1),
           sliderInput("minLikes", "Select minimum number of likes", min = 0,value = 1,
                       max = 300,step = 1),
           radioButtons("tweet_length","Tweet larger than median length:",
                        choices = c("yes","no")),
           selectInput("facet","Facet by column",
                        choices = c("No Faceting","Long-Short tweet"))


  ),
  tabPanel("Stocks",
           selectizeInput("stock", "Choose a stock", choices = "",multiple = T,selected  = "Allianz",
                          options = list(placeholder = 'select a stock')),
           # for stock only faceting
           selectInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                 "Mean weighted by likes",
                                                                 "Mean weighted by length")),
           sliderInput("minRetweet_stocks", "Select minimum number of retweets", min = 0,value = 1,
                       max = 1,step = 1),
           radioButtons("tweet_length","Tweet larger than median length:",
                        choices = c("yes","no"))
  )

)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("Sentiment_type", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                  selected = "NoFilter"),

      selectInput("plotType", "Plot", choices = c("Time Series","Density","Box Plot"),
                  selected = "Time Series"),
      radioButtons("language","Choose Langugage of Tweets:",choices = c("En","De")),

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
