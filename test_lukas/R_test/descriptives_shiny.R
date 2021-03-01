setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq")
library(readr)
library(glue)
library(tidyverse)





### connect to database
old_wd <- getwd()
setwd("C:/Users/lukas/OneDrive - UT Cloud/Data")
con <- DBI::dbConnect(RSQLite::SQLite(), "SQLiteStudio/databasestest.db")
setwd(old_wd)

ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("term frequencies"),

  sidebarLayout(

    sidebarPanel(

      radioButtons("lang", "Select Language", choices = c("EN", "DE")),
      selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),

      dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2021-02-19",
                     min = "2018-11-30", max = "2021-02-19", format = "yyyy-mm-dd"),
      radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                   inline = T),
      radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                   inline = T),
      #switchInput(inputId = "long", value = TRUE),
      shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),


      selectInput("plot_type", "What kind of plot would you like to see?", choices = c("Time Series"="sum_stats",
                                                                                       "Histogram" = "histo"),
                  selected = "sum_stats"),



      selectInput("value", "Which value would you like to show",
                  choices = c(
                    "Sentiment" = "sentiment",
                    "Retweets Weighted Sentiment" = "sentiment_rt",
                    "Likes Weighted Sentiment" = "sentiment_likes",
                    "Length Weighted Sentiment" = "sentiment_tweet_length",
                    "Retweets" = "rt",
                    "Likes"="likes",
                    "Tweet Length" = "tweet_length",
                    "Number of Tweets" = "N"
                  ),
                  selected = "rt"),


      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "input.plot_type == 'histo'",

        sliderInput("bins", "Adjust the number of bins for the histogram", min = 5, max = 1000, value = 100),


        # add switch whether to use logarithmic scale
        shinyWidgets::switchInput(inputId = "log_scale", label = "Logarithmic Scale",
                                  value = F,
                                  size = "small",
                                  handleWidth = 100
                                     )


      ),

      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "input.plot_type == 'sum_stats'",
        radioButtons("metric", "Select a metric",
                     choiceNames = c("Mean", "Standard deviation", "Median"),
                     choiceValues = c("mean", "std", "median"))
      )




    ),

    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'histo'",
        plotOutput("histo_plot") %>%
          withSpinner()

      ),
      conditionalPanel(
        condition = "input.plot_type == 'sum_stats'",
        "text",
        plotOutput('sum_stats_plot') %>%
          withSpinner()#, height = "800px")
      )

    )
  )
)

server <- function(session, input, output){


  ### for histogram less choices
  observeEvent(input$plot_type,{

    if (input$plot_type == "histo"){
      if (input$value %in% c("sentiment_rt", "sentiment_likes", "sentiment_length")){
        selected_value <-"sentiment"
      } else {
          selected_value <- input$value
      }

    updateSelectInput(session = session, "value",
      choices = c("Sentiment" = "sentiment",
                  "Retweets" = "rt",
                  "Likes"="likes",
                  "Tweet Length" = "tweet_length"
      ), selected = selected_value)
    } else {

      updateSelectInput(session = session, "value",
                        choices = c(
                          "Sentiment" = "sentiment",
                          "Retweets Weighted Sentiment" = "sentiment_rt",
                          "Likes Weighted Sentiment" = "sentiment_likes",
                          "Length Weighted Sentiment" = "sentiment_tweet_length",
                          "Retweets" = "rt",
                          "Likes"="likes",
                          "Tweet Length" = "tweet_length",
                          "Number of Tweets" = "N"
                          ), selected = input$value)
    } #if clause
  }) #observeevent

  # avoid that date range upper value can be lower than lower value
  # Update the dateRangeInput if start date changes
  observeEvent(input$dates[1], {
    end_date = input$dates[2]
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if (input$dates[2] < input$dates[1]) {
      end_date = input$dates[1]
    }
    updateDateRangeInput(session,"dates", start=input$dates[1], end=end_date, min=input$dates[1] )
  })


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
      } else if (input$value == "tweet_length"){
        metric <- glue("{input$metric}_length")
      } else{
        metric <- glue("{input$metric}_{input$value}")
      }



      table_name <- glue("{input$plot_type}_{tolower(input$lang)}_all")
     # browser()
      glue("SELECT created_at, {metric} as value FROM {table_name}  WHERE created_at >= '{input$dates[1]}'
      and created_at <= '{input$dates[2]}'
         and retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long}" )


    } else if (input$plot_type == "histo"){
      #browser()
      if (input$value == "length"){
        tb_metric <- "len"
        col_value <- input$value

      } else if(input$value == "rt") {

        tb_metric <- input$value
        col_val <- "retweets_count"

      } else if(input$value == "likes") {
        tb_metric <- input$value
        col_val <- "likes_count"
      } else if(input$value == "sentiment"){
        tb_metric <- input$value
        col_val <- "sentiment_rd"
      } else{
        Sys.sleep(0.2)
      }





      table_name <- glue("{input$plot_type}_{tb_metric}_{tolower(input$lang)}")

      if (table_name %in% c("histo_rt_en", "histo_likes_en", "histo_len_en")){
        date_col <- "date"
      } else{
        date_col <- "created_at"
      }

      glue("SELECT {col_val}, sum(N) as  n  FROM {table_name}  WHERE {date_col} >=  '{input$dates[1]}'
      and {date_col} <= '{input$dates[2]}'
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

    df$created_at <- as.Date(df$created_at)
  df %>%
    ggplot(aes(x = created_at,
               y = value)) +
      geom_line()
  }

})


observeEvent(input$value, {
  #browser()
  if (input$value == "sentiment") {
    shinyWidgets::updateSwitchInput(session = session,
                               "log_scale",
                               disabled = T)
  } else {
    shinyWidgets::updateSwitchInput(session = session,
                                    "log_scale",
                                    disabled = F)
  }
})



  output$histo_plot <- renderPlot({
    df <- data()

    #freezeReactiveValue(input, "plot_type")

    # if sentiment then disable log button because has negative values



    if (input$plot_type == "histo"){

    df %>%
      # {if (input$log_scale == T) {} else {
      #          mutate(bins = cut_interval(.[[1]], n = input$bins))
      #        }
      # }

      mutate(metric = case_when(input$log_scale == T ~ log(as.numeric(.[[1]])+ 0.0001),
                                input$log_scale == F ~ as.numeric(.[[1]])),
             bins = cut_interval(metric, n = input$bins))%>%

      ggplot(aes(bins, n)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    }
  })






}

shinyApp(ui, server)







