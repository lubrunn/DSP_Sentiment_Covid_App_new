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

      selectInput("plot_type", "What kind of plot would you like to see?", choices = c("Time Series"="sum_stats", "Histogram" = "histo"), selected = "Histogram"),


      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "input.plot_type == 'histo'",
        numericInput("bins", "Adjust the number of bins for the histogram", min = 30, max = 10000, value = 100),

      ),
      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "input.plot_type == 'sum_stats'",
        radioButtons("metric", "Select a metric", choiceNames = c("Mean", "Standard deviation", "Median"), choiceValues = c("mean", "std", "median")),
      ),
      selectInput("value", "Which value would you like to show", choices = c("Retweets" = "rt", "Likes"="likes", "Tweet Length" = "length",
                                                                             "Number of Tweets" = "N"), selected = "Retweets")


    ),

    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'histo'",
        plotOutput("histo_plot")

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
    #browser()
    if (input$plot_type == "sum_stats"){

      if (input$value == "N"){
        metric <- "N"
      } else{
        metric <- glue("{input$metric}_{input$value}")
      }



      table_name <- glue("{input$plot_type}_{tolower(input$lang)}")
      glue("SELECT date, {metric} as value FROM {table_name}  WHERE date >= '{input$dates[1]}' and date <= '{input$dates[2]}'
         and retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long}" )


    } else if (input$plot_type == "histo"){
        browser()
      if (input$value == "length"){
        tb_metric <- "len"
        col_value <- input$value

      } else if(input$value == "rt") {

        tb_metric <- input$value
        col_val <- "retweets_count"

      } else if(input$value == "likes") {
        tb_metric <- input$value
        col_val <- "likes_count"
      }



      table_name <- glue("{input$plot_type}_{tb_metric}_{tolower(input$lang)}")

      querry_str <- glue("SELECT {col_val}, sum(N) as  n  FROM {table_name}  WHERE date >=  '{input$dates[1]}' and date <= '{input$dates[2]}'
      and retweets_count_filter = {input$rt} and likes_count_filter = {input$likes} and
      tweet_length_filter = {long}
      group by {col_val}")

    }
   #browser()


  })

  data <- reactive({


    df_need <- DBI::dbGetQuery(con, querry())
    df_need
    })



output$sum_stats_plot <- renderPlot({

  df <- data()


  if(input$plot_type == "sum_stats"){
    df$date <- as.Date(df$date)
  df %>%
    ggplot(aes(x = date,
               y = value)) +
      geom_line()
  }

})

  output$histo_plot <- renderPlot({
    df <- data()

    #freezeReactiveValue(input, "plot_type")

    if (input$plot_type == "histo"){

    df %>%
      mutate(log_metric = log(.[[1]]+ 0.0001),
             bins = cut_interval(log_metric, n = input$bins)) %>%
      ggplot(aes(bins, n)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    }
  })






}

shinyApp(ui, server)







