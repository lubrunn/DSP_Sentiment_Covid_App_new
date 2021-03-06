
numeric_features <- tabsetPanel(
  id = "tabs_for_var",
  type = "hidden",
  tabPanel("1",
             selectInput("var_1", "Select varaible", choices = ""),
            numericInput("num_1","Chose length of moving average",min=1,value = 1),
             numericInput("num_2","Chose Autoregressive lags for",min=1,value = 1)


  ),
  tabPanel("2",
           selectInput("var_2", "Select varaible", choices = ""), #could I use var_1 here?
           numericInput("num_3","Chose length of moving average",min=1,value = 1),
           numericInput("num_4","Chose Autoregressive lags for",min=1,value = 1),
           selectInput("var_3", "Select varaible", choices = ""),
           numericInput("num_5","Chose length of moving average",min=1,value = 1),
           numericInput("num_6","Chose Autoregressive lags for",min=1,value = 1)
  )

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
           numericInput("sample_size","amount of data exposed to the fitting routine",min = 0.1,max=1,step = 0.1,value = 0.7),
           numericInput("cv","number of cross validation folds",min = 1,max=10,step = 1,value = 3)
           
),
 tabPanel("hyperparameter_tuning",
           numericInput("trees_hyp","number of predictors that will be randomly sampled",min = 50,max=1000,step = 10,value = 200),
           numericInput("cv_hyp","number of cross validation folds",min = 1,max=10,step = 1,value = 3),
           numericInput("grid_size","size of tuning grid",min = 10,max=100,step = 5,value = 30)
           
           
           )
  
)