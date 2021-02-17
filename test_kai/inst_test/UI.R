Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(
  #theme = shinythemes::shinytheme("cosmo"),
  shinythemes::themeSelector(),
  titlePanel("Sentiment_Covid_App"),
  tabsetPanel(
    tabPanel("Twitter"),
    tabPanel("Sentiment"),
    tabPanel("Stocks",
             sidebarPanel(
               selectize_Stocks(COMPONENTS_DE()),
               actionButton("reset", "clear selected"),
               checkboxInput("hovering","Enable hover",value = FALSE),
               sliderinput_dates()
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Germany",
                          plot_stocks_DE(),
                          hover_info_DE()
                 ),#close tabPanel Germany
                 tabPanel("USA")
               )#close tabsetPanel
             )#close mainpanel
    ),#close tabPanel stock
    tabPanel("Corona",
             sidebarPanel(
               selectize_corona(),
               checkboxGroupInput("CoronaCountry","Country",c("Germany","United States"),selected = "Germany"),
               sliderinput_dates_corona(),
               checkboxInput("hovering_corona","Enable hover",value = FALSE)
             ),
             mainPanel(
               plot_corona(),
               hover_info_corona()
             )
             )#close tabPanel Corona
  )#close tabsetPanel
)#close fluidpage
