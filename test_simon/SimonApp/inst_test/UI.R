fluidPage(

  selectInput("plotType", "Plot", choices = c("Time Series","Density")), # here choices of models
  selectInput("tweetType", "Type", choices = "" ),
  selectInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                        "Mean weighted by likes",
                                                        "Mean weighted by length")),

  plotOutput("plot1")


)
