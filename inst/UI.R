Sys.setlocale("LC_TIME", "English")

fluidPage(
  theme = shinythemes::shinytheme("slate"),
  #shinythemes::themeSelector(),
  titlePanel("Sentiment_Covid_App"),
  tabsetPanel(
    tabPanel("Twitter"),
    tabPanel("Sentiment"),
    tabPanel("Stocks",
             sidebarPanel(
               selectize_Stocks(COMPONENTS_DE()),
               actionButton("reset", "clear selected"),
               sliderinput_dates()
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Germany",
                          plot_stocks_DE(),
                          hover_info_DE(),
                          plot_stocks_US(),
                          hover_info_US()
                          ),#close tabPanel Germany
                 tabPanel("USA")
                 )#close tabsetPanel
               )#close mainpanel
             )#close tabPanel stock
  )#close tabsetPanel
)#close fluidpage

