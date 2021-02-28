setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq")
library(readr)
library(glue)
library(tidyverse)


folders <- list.files()
folder <- "En_Nofilter"


subfolders <- list.files(folder)

file <- files[1]


### connect to database
old_wd <- getwd()
setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases")
con <- DBI::dbConnect(RSQLite::SQLite(), "test.db")
setwd(old_wd)

ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("term frequencies"),

  sidebarLayout(

    sidebarPanel(

      radioButtons("lang", "Select Language", choices = c("EN", "DE")),
      selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),

      dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2021-02-13",
                     min = "2018-11-30", max = "2021-02-13", format = "yyyy-mm-dd"),
      radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                   inline = T),
      radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                   inline = T),
      #switchInput(inputId = "long", value = TRUE),
      shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),

      selectInput("plot_type", "What kind of plot would you like to see?", choices = c("Histogram" = "histo", "Time Series"="sum_stats"), selected = "Histogram"),


      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "input.plot_type == 'histo'",
        numericInput("bins", "Adjust the number of bins for the histogram", min = 30, max = 10000, value = 30)
      ),
      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "input.plot_type == 'sum_stats'",
        radioButtons("metric", "Select a metric", choiceNames = c("Mean", "Standard deviation", "Median"), choiceValues = c("mean", "std", "median")),
        selectInput("value", "Which value would you like to show", choices = c("Retweets" = "rt", "Likes"="likes", "Tweet Length" = "length"), selected = "Retweets")
      )

    ),

    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'histo'",
        plotOutput("histo")

      ),
      conditionalPanel(
        condition = "input.plot_type == 'sum_stats'",
        "text",
        plotOutput('sum_stats_plot')#, height = "800px")
      )

    )
  )
)

server <- function(session, input, output){




  ######## disconnect from database after exit
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })




  querry <- reactive({

    if (input$long == T){
      long <- 81
    } else{
      long <- 0
    }

    if (input$plot_type == "sum_stats"){
      table_name <- glue("{input$plot_type}_{tolower(input$lang)}")
    } else{
      table_name <- glue("{input$plot_type}_{input$value}_{tolower(input$lang)}")
    }
   #browser()
    glue("SELECT {input$metric}_{input$value} as value FROM {table_name}  WHERE date >= '{input$dates[1]}' and date <= '{input$dates[2]}'
         and retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long}" )

  })

  data <- reactive({


    df_need <- DBI::dbGetQuery(con, querry())
    df_need
    })



output$sum_stats_plot <- renderPlot({

  df <- data()

  if (input$plot_type == "sum_stats"){
  df %>%
    ggplot(aes(x = 1:dim(df)[1],
               y = value)) +
      geom_line()
  }
})

  output$num_tweets <- renderPrint({

  })

}

shinyApp(ui, server)







