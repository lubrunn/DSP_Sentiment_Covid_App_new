
numeric_features <- tabsetPanel(
  id = "tabs_for_var",
  type = "hidden",
  tabPanel("1",
           selectInput("var_1", "Select varaible", choices = ""),
           numericInput("num_1","Chose length of moving average",min=1,value = 1),
           numericInput("num_2","Chose Autoregressive lags for",min=1,value = 1),
           actionButton("addButton", "UPLOAD!"),
           actionButton("finish", "Finish!"),
           actionButton("reset_cus", "Reset!")
           
           
   )
  # tabPanel("2",
  #          selectInput("var_2", "Select varaible", choices = ""), #could I use var_1 here?
  #          numericInput("num_3","Chose length of moving average",min=1,value = 1),
  #          numericInput("num_4","Chose Autoregressive lags for",min=1,value = 1),
  #          selectInput("var_3", "Select varaible", choices = ""),
  #          numericInput("num_5","Chose length of moving average",min=1,value = 1),
  #          numericInput("num_6","Chose Autoregressive lags for",min=1,value = 1)
  # )
  
)


model_specification <- tabsetPanel(
  id = "mod_spec",
  type = "hidden",
  tabPanel("default"),
  tabPanel("custom",
           numericInput("mtry","number of predictors that will be randomly sampled",min = 2,max=30,step = 1,value = 20),
           numericInput("trees","number of trees contained in the ensemble",min = 50,max=1000,step = 10,value = 200),
           numericInput("min_n","minimum number of data points in a node",min = 1,max=20,step = 1,value = 3),
           numericInput("tree_depth","maximum depth of the tree",min = 1,max=50,step = 1,value = 8),
           numericInput("learn_rate","rate at which the boosting algorithm adapts",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("loss_reduction","reduction in the loss function required to split further",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("sample_size","amount of data exposed to the fitting routine",min = 0.1,max=1,step = 0.1,value = 0.7)
           
  ),
  tabPanel("hyperparameter_tuning",
           numericInput("trees_hyp","number of predictors that will be randomly sampled",min = 50,max=1000,step = 10,value = 200),
           numericInput("grid_size","size of tuning grid",min = 10,max=100,step = 5,value = 30)
           
           
  )
  
)


model_specification_for <- tabsetPanel(
  id = "mod_spec_for",
  type = "hidden",
  tabPanel("default"),
  tabPanel("custom",
           numericInput("mtry1","number of predictors that will be randomly sampled",min = 2,max=30,step = 1,value = 20),
           numericInput("trees1","number of trees contained in the ensemble",min = 50,max=1000,step = 10,value = 200),
           numericInput("min_n1","minimum number of data points in a node",min = 1,max=20,step = 1,value = 3),
           numericInput("tree_depth1","maximum depth of the tree",min = 1,max=50,step = 1,value = 8),
           numericInput("learn_rate1","rate at which the boosting algorithm adapts",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("loss_reduction1","reduction in the loss function required to split further",min = 0.005,max=0.1,step = 0.001,value = 0.01),
           numericInput("sample_size1","amount of data exposed to the fitting routine",min = 0.1,max=1,step = 0.1,value = 0.7)
           
  ),
  tabPanel("hyperparameter_tuning",
           numericInput("trees_hyp1","number of predictors that will be randomly sampled",min = 50,max=1000,step = 10,value = 200),
           numericInput("grid_size1","size of tuning grid",min = 10,max=100,step = 5,value = 30)
           
           
  )
  
)


custom_lag_tab <- tabsetPanel(
  id = "lag_tab",
  type = "hidden",
  tabPanel("default"),
  tabPanel("custom",
           selectInput("correlation_type", "Chose type of correlation:", choices = c("ACF","PACF")),
           uiOutput("correlation_plot_choice"),
           numeric_features
           # actionButton("reset_arma", "clear selected")
           
  )
  
)
















