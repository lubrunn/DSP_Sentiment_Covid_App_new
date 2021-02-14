fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),

  verbatimTextOutput("summary"),
  #verbatimTextOutput("sum_model"),
  textOutput("path"),
  verbatimTextOutput("test_table"),
  DT::DTOutput("test_table_dt"),
  tableOutput("test_table_df"),
  tableOutput("table"),
  plotOutput("plot1")


)
