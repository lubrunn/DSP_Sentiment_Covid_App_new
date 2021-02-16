ui <- fluidPage(
  tabsetPanel(
  tabPanel("Twitter"),
  tabPanel("Sentiment"),
  tabPanel("Stocks",
           tabsetPanel(
             tabPanel("Germany",
              sidebarLayout(
               sidebarPanel(
                 selectize_Stocks(COMPONENTS_DE()),
                 actionButton("reset", "clear selected"),
                 sliderinput_dates()
               ),
               mainPanel(
                 plot_stocks_DE(),
                 hover_info_DE(),
                 plot_stocks_US(),
                 hover_info_US()
                 )#close mainpanel
               )#close sidebarlayout
             ),#close tabPanel Germany
             tabPanel("USA")
             )#close tabsetPanel
           )#close tabPanel stock
  )#close tabsetPanel
  )#close fluidpage

