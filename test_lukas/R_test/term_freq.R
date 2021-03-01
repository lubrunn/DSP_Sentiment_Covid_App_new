setwd("C:/Users/lukas/OneDrive - UT Cloud/Data")
library(readr)
library(glue)
library(tidyverse)









ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("term frequencies"),

  sidebarLayout(

    sidebarPanel(

      radioButtons("lang", "Select Language", choices = c("EN", "DE")),
      selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),

      dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2021-02-13",
                     min = "2018-11-30", max = "2018-12-09", format = "yyyy-mm-dd"),
      radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                   inline = T),
      radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                  inline = T),
      #switchInput(inputId = "long", value = TRUE),
    shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),
    shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F),
      selectInput("plot_type", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")),
      sliderInput("n", "Number of words to show", min = 5, max = 5000, value = 15),

      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        # keep for both because bigram also makes senese with wordcloud
        condition = "true == true",
        radioButtons("ngram_sel", "Would like to to see single words or bigrams?", choices = c("Unigram", "Bigram"))
      )

      ),

    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'Frequency Plot'",
        plotOutput("Plot")
        #uiOutput("plot.ui")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Word Cloud'",
        "text",
        wordcloud2::wordcloud2Output('wordcloud', height = "800px")
      )

    )
  )
)

server <- function(session, input, output){




  output$Plot <- renderPlot(
    # dynamically change height of plot
   height = function() input$n * 30 + 400,

    {
      df <- data()


        if (input$plot_type == "Frequency Plot"){
          df %>%
            #filter(X1 != "num_tweets") %>%
            top_n(input$n) %>%
            arrange(desc(n)) %>%
            ggplot(aes(reorder(x = words, n), y = n)) +
            geom_col() +
            coord_flip()


        }
  })

  data <- reactive({
    if (input$lang == "EN"){
      lang <- "En"
    } else {
      lang <- "De"
    }

    if (input$comp != "") {
      folder <- file.path("Companies", input$comp)
    } else {
      folder <- glue("{lang}_NoFilter")
    }


    if (input$long == T){
      long <- "long_only"
      tweet_length_filter <- 81
    } else{
      long <- "all"
      tweet_length_filter <- 0
    }

    # go into specified folder and load dataframe
    file_name <- glue("term_freq_{lang}_NoFilter_{input$dates[1]}_rt_{input$rt}_li_{input$likes}_lo_{long}.csv")


    if (input$ngram_sel == "Unigram"){
      subfolder <- "uni"
    } else {
      subfolder <- "bi"
    }

    file_path <- file.path("Twitter/term_freq",folder, subfolder, file_name)
    #browser()
    df <- readr::read_csv(file_path, col_types = cols(date_variable = "D"))
    #%>%
     # filter(between(date_variable, input$dates[1], input$dates[2]))



    df <- df %>%

      filter(
        emo == F | emo == T &
          retweets_count >= input$rt &
          likes_count >= input$likes &
          tweet_length >= tweet_length_filter) %>%
      {if (input$emo == T) filter(., emo == F) else .} %>%
      select(-emo) %>%
      pivot_wider(names_from = word, values_from = N) %>%
      select(-c(date_variable, language_variable, retweets_count,
                likes_count, tweet_length)) %>%
      colSums(na.rm = T) %>%
      data.frame() %>%
      rename("n" = ".") %>%
      rownames_to_column("words")




  })

  output$wordcloud <- wordcloud2::renderWordcloud2({
    df <- data()
    if (input$plot_type == "Word Cloud"){
      df %>% top_n(input$n) %>%
      wordcloud2::wordcloud2(size = 1,shape = 'star',
                 color = "random-light", backgroundColor = "grey")
    }
  })

  # output$Plot <- renderPlot({
  #
  #   df <- data()
  #
  #   if (input$plot_type == "Frequency Plot"){
  #   df %>%
  #     #filter(X1 != "num_tweets") %>%
  #     top_n(input$n) %>%
  #     arrange(desc(n)) %>%
  #     ggplot(aes(reorder(x = words, n), y = n)) +
  #     geom_col() +
  #     coord_flip()
  #     }
  #
  # })

  output$num_tweets <- renderPrint({

  })

}

shinyApp(ui, server)







