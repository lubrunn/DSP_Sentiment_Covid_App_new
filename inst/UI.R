fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Stock","Chose Company",
                     c(COMPONENTS_DE()[["Company.Name"]]),
                     multiple=TRUE,
                     selected = ""),
      actionButton("reset","clear selected"),
      sliderInput("dates",label="Timeseries",
                  value = c(median(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                  min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                  max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                  step = 1,timeFormat = "%F")
    ),
    mainPanel(plotOutput("plot_DE",hover = hoverOpts("plot_hover_DE", delay = 10, delayType = "debounce")),
              uiOutput("hover_info_DE"),
              plotOutput("plot_US",hover = hoverOpts("plot_hover_US", delay = 10, delayType = "debounce")),
              uiOutput("hover_info_US"))
  ))
