library(shiny)
library(ggplot2)

ui <- fluidPage(
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

server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    #summary(dataset)
    sum_func(dataset)
  })

  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })

  # output$sum_model <- renderPrint({
  #   model_func(get(input$dataset, "package:datasets"))
  # })

 output$test_table <-  renderPrint({
    data.frame(getData(test_data()))
 })

  output$path <- renderText(getwd())

  output$test_table_dt <- DT::renderDT({
    DT::datatable(getData(test_data()),
                  options = list(scrollX = TRUE))
  })

  output$test_table_df <- renderTable({
    getData(test_data())
  })

  output$plot1 <- renderPlot({
    plotdata(test_data())
  })
}
myApp <- function(...) {
  # Plus other stuff that was previously in app.R or globals.R
  # Hopefully refactored in a few function calls

  shinyApp(ui, server, ...)
}


#usethis::use_description()
