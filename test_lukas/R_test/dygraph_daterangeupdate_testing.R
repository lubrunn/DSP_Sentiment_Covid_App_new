library(xts)
library(shiny)
library(dygraphs)
library(lubridate)




ui <- fluidPage(
  titlePanel("Dygraph & date range input"),
  sidebarLayout(
    sidebarPanel(
      shinyWidgets::airDatepickerInput("dates_desc", "Date range:",
                                       range = TRUE,
                                       value = c("2018-11-30", "2021-02-19"),
                                       maxDate = "2021-02-19", minDate = "2018-11-30",
                                       clearButton = T, update_on = "close")
    ),
    mainPanel(
      dygraphOutput("sum_stats_plot")
    )
  )
)

server <- function(input, output,session) {

  r <- reactiveValues(
    change_datewindow = 0,
    change_dates = 0,
    change_datewindow_auto = 0,
    change_dates_auto = 0,
    dates = c( as.Date("2018-11-30"), as.Date("2021-02-19"))
  )


  observeEvent(input$sum_stats_plot_date_window, {
    message(crayon::blue("observeEvent_input_sum_stats_plot_date_window"))
    r$change_datewindow <- r$change_datewindow + 1
    if (r$change_datewindow > r$change_datewindow_auto) {

      r$change_dates_auto <- r$change_dates_auto + 1
      r$change_datewindow_auto <- r$change_datewindow

      start <- as.Date(ymd_hms(input$sum_stats_plot_date_window[[1]])+ days(1))
      stop  <- as.Date(ymd_hms(input$sum_stats_plot_date_window[[2]])+ days(1))
      updateAirDateInput(session = session,
                           inputId = "dates_desc",
                         value = c(start, stop),
      )
    } else {
      if (r$change_datewindow >= 10) {
        r$change_datewindow_auto <- r$change_datewindow <- 0
      }
    }
  })

  observeEvent(input$dates, {
    message("observeEvent_input_dates")
    r$change_dates <- r$change_dates + 1
    if (r$change_dates > r$change_dates_auto) {
      message("event input_year update")

      r$change_datewindow_auto <- r$change_datewindow_auto
      r$change_dates_auto <- r$change_dates

      r$dates <- input$dates

    }
  })

  output$sum_stats_plot <- renderDygraph({
    message("renderDygraph")
    test_func(r, input$dates_desc[1], input$dates_desc[2])[[1]]
  })
}

# Run the application
shinyApp(ui = ui, server = server)


test_func <- function(r, input_dates1, input_dates2){

  df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/sum_stats_En_NoFilter_rt_200_li_200_lo_long_only.csv")








  df$created_at <- as.Date(df$created_at)

  #df <- df %>% filter(between(created_at, as.Date(input_dates1), as.Date(input_dates2)))



  ### time dons
  df_values <- df %>% select(mean_rt) %>% scale()

  don <- xts(x = df_values, order.by = df$created_at)


  p <- dygraph(don) %>%
    dyRangeSelector(
      dateWindow = r$dates) # +1 parce que voila...


  a <- list(a = p, b = r)


  return(a)
}
