fluidPage(

  selectInput("plotType", "Plot", choices = c("Time Series","Density")), # here choices of models
  selectInput("tweetType", "Type", choices = "" ),
  #selectInput("ordernumber", "Order number", choices = NULL),

  plotOutput("plot1")


)
