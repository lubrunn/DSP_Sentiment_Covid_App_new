Sys.setlocale("LC_TIME", "English")
fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectize_Stocks(),
      actionButton("reset","clear selected"),
      sliderinput_dates()
    ),
    mainPanel(plot_stocks_DE(),
              hover_info_DE(),
              plot_stocks_US(),
              hover_info_US()
    )

  ))
