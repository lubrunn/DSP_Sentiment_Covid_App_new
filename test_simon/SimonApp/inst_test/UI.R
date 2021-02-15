fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Plot", choices = c("Time Series","Density"),
                  selected = "Time Series"),

      selectizeInput("tweetType", "Type", choices = "",multiple = T,selected  = "Allianz"),

      dateRangeInput("timeWindow", label = "Time Span",
                 start = "2018-11-30", end = "2018-12-07"),
                        #as.character(format(Sys.time(),'%Y-%m-%d'))
      selectInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                        "Mean weighted by likes",
                                                        "Mean weighted by length")),

      sliderInput("minRetweet", "Select minimum number of retweets", min = 0,value = 1,
                                                               max = 1,step = 1),

    ),
    mainPanel(
      conditionalPanel(
        condition = "input.plotType == 'Time Series'", plotOutput("plot1")),
      conditionalPanel(
        condition = "input.plotType == 'Density'", plotOutput("plot2"))
    )
  )
)
