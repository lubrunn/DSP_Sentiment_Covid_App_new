Sys.setlocale("LC_TIME", "English")

fluidPage(
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
    )#close tabPanel stock
  )#close tabsetPanel
)#close fluidpage

