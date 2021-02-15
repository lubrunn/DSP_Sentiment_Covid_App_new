fluidPage(
  selectInput("stock", label = "Chose COmpany", choices = ls("package:datasets")),
  plotOutput("plot")


)

