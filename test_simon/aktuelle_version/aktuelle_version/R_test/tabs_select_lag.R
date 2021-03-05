
numeric_features <- tabsetPanel(
  id = "tabs_for_var",
  type = "hidden",
  tabPanel("1",
             selectInput("var_1", "Select varaible", choices = ""),
            numericInput("num_1","Chose length of moving average",min=1,value = 1),
             numericInput("num_2","Chose Autoregressive lags for",min=1,value = 1)


  ),
  tabPanel("2",
           selectInput("var_2", "Select varaible", choices = ""),
           numericInput("num_3","Chose length of moving average",min=1,value = 1),
           numericInput("num_4","Chose Autoregressive lags for",min=1,value = 1),
           selectInput("var_3", "Select varaible", choices = ""),
           numericInput("num_5","Chose length of moving average",min=1,value = 1),
           numericInput("num_6","Chose Autoregressive lags for",min=1,value = 1)
  )

)

