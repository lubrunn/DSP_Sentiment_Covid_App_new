setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq")
library(readr)
library(glue)
library(tidyverse)


folders <- list.files()
folder <- "En_Nofilter"


subfolders <- list.files(folder)

file <- files[1]


# # got through all files and extract the info it stores from the filename
# for (file in files){
# # split file name into dataframe containing info about which dataframestring contains which filters
# rt <- as.numeric(stringr::str_match(file, "rt_\\s*(.*?)\\s*_li")[2])
# likes <- as.numeric(stringr::str_match(file, "li_\\s*(.*?)\\s*_lo")[2])
# long <- as.numeric(stringr::str_match(file, "lo_\\s*(.*?)\\s*.csv")[2])
#
# # setup df, if already exists append new row
# if (is.null(file_info)){
# file_info <- data.frame(file = file, rt = rt, likes = likes, long = long)
# } else{
#   file_info_new <- data.frame(file = file, rt = rt, likes = likes, long = long)
#   file_info <- rbind(file_info, file_info_new)
# }
# }
#
# # save info_df
# readr::write_csv(file_info, file.path("info_dfs",path, folder))


#file_path <- file.path(folder, file)

#df <- readr::read_csv(file_path,  col_types = cols(date = "D"))


ui <- fluidPage(

  titlePanel("term frequencies"),

  sidebarLayout(

    sidebarPanel(

      radioButtons("lang", "Select Language", choices = c("EN", "DE")),
      selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),

      dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2018-12-09",
                     min = "2018-11-30", max = "2018-12-09", format = "yyyy-mm-dd"),
      radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                   inline = T),
      radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                  inline = T),
      #switchInput(inputId = "long", value = TRUE),
      materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),
      materialSwitch(inputId = "emo", label = "Include Emoji Words?", value = F),
      selectInput("plot_type", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")),
      sliderInput("n", "Number of words to show", min = 5, max = 500, value = 15),

      conditionalPanel(

        #condition = "input.plot_type == 'Frequency Plot'",
        condition = "true == true",
        radioButtons("ngram_sel", "Would like to to see single words or bigrams?", choices = c("Unigram", "Bigram"))
      )

      ),

    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'Frequency Plot'",
        plotOutput("Plot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Word Cloud'",
        wordcloud2Output('wordcloud')
      ),


      verbatimTextOutput("num_tweets")
    )
  )
)

server <- function(session, input, output){
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

    file_path <- file.path(folder, subfolder, file_name)
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
      {if (input$emo == F) filter(., emo == F) else .} %>%
      select(-emo) %>%
      pivot_wider(names_from = word, values_from = N) %>%
      select(-c(date_variable, language_variable, retweets_count,
                likes_count, tweet_length)) %>%
      colSums(na.rm = T) %>%
      data.frame() %>%
      rename("n" = ".") %>%
      rownames_to_column("words")




  })

  output$wordcloud <- renderWordcloud2({
    df <- data()
    if (input$plot_type == "Word Cloud"){
      df %>% top_n(input$n) %>%
      wordcloud2(size = 1,shape = 'star',
                 color = "random-light", backgroundColor = "grey")
    }
  })

  output$Plot <- renderPlot({

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

  output$num_tweets <- renderPrint({

  })

}

shinyApp(ui, server)







