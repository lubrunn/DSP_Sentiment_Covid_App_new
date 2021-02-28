library(glue)
library(shinyFiles)


Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(
  #theme = shinythemes::shinytheme("cosmo"),
  shinythemes::themeSelector(),
  #titlePanel("Sentiment_Covid_App"),
  navbarPage("APP",
    tabPanel("Select Working Directory",
             sidebarPanel(

               tags$p(),
               tags$p("Please choose the directory containing the folder containig \n
               the data called 'Data'."),
              shinyFiles::shinyDirButton("directory", "Folder select", "Please select a folder"
                              ),

             ),
             mainPanel(
               tags$h4("Selected folder"),
               tags$p(HTML("Please check that you picked the correct folder otherwise \n
                           the App will be not work.")),
               textOutput("directorypath"),
               tags$hr()
             )),
    navbarMenu("Twitter",
               tabPanel("Descriptives",
                        sidebarPanel(
                          selectInput("comp", "Select a Company or look at unfiltered tweets",
                                    choices = c("Adidas", "Nike"))


                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Descriptives"),
                            tabPanel("Exploratory")
                            ))),
               tabPanel("Sentiment"),
                tabPanel("Daily Analysis"),
                tabPanel("Going deeper")),
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
             ),#close tabPanel Corona
    navbarMenu("Model",
               tabPanel("Granger",
                        sidebarPanel(),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visualize"),
                            tabPanel("Background-steps"),
                            tabPanel("Results")))),
               tabPanel("Quantile Regression"))
  )#close tabsetPanel
)#close fluidpage
