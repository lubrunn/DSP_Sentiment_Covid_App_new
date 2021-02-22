setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq")
library(readr)
library(glue)
library(tidyverse)

source <- ""
folders <- list.files(source)
folder <- "En_Nofilter"
file_info <- NULL

files <- list.files(file.path(path, folder))

file <- files[1]


# got through all files and extract the info it stores from the filename
for (file in files){
# split file name into dataframe containing info about which dataframestring contains which filters
rt <- as.numeric(stringr::str_match(file, "rt_\\s*(.*?)\\s*_li")[2])
likes <- as.numeric(stringr::str_match(file, "li_\\s*(.*?)\\s*_lo")[2])
long <- as.numeric(stringr::str_match(file, "lo_\\s*(.*?)\\s*.csv")[2])

# setup df, if already exists append new row
if (is.null(file_info)){
file_info <- data.frame(file = file, rt = rt, likes = likes, long = long)
} else{
  file_info_new <- data.frame(file = file, rt = rt, likes = likes, long = long)
  file_info <- rbind(file_info, file_info_new)
}
}

# save info_df
readr::write_csv(file_info, file.path("info_dfs",path, folder))


file_path <- file.path(folder, file)

df <- readr::read_csv(file_path,  col_types = cols(date = "D"))


ui <- fluidPage(
  
  titlePanel("term frequencies"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("lang", "Select Language", choices = c("EN", "DE")),
      selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),
      
      dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2018-12-09",
                     min = "2018-11-30", max = "2018-12-09", format = "yyyy-mm-dd"),
      radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0),
      radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0),
      radioButtons("long", "long tweet?", choiceNames = c("Short Tweets only", "Long Tweets only", "Both"), choiceValues = c(0, 1, 2))
    ),
    
    mainPanel(
      plotOutput("Plot"),
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
    
    # go into specified folder and load dataframe
    file_name <- glue("term_freq_{lang}_NoFilter_rt_{input$rt}_li_{input$likes}_lo_{input$long}.csv")
    file_path <- file.path(folder, file_name)
    
    df <- readr::read_csv(file_path, col_types = cols(date = "D")) %>%
      filter(between(date, input$dates[1], input$dates[2])) 
    
    
    
    df <- df %>% 
      select(-date) %>%
      colSums(na.rm = T) %>%
      data.frame() %>%
      rename("n" = ".") %>%
      rownames_to_column("X1")
    
    
    
    
  })
  
  output$Plot <- renderPlot({
    
    df <- data()
    df %>%
      filter(X1 != "num_tweets") %>%
      top_n(20) %>% 
      arrange(desc(n)) %>%
      ggplot(aes(x = X1, y = n)) +
      geom_col() +
      coord_flip()
  })
  
  output$num_tweets <- renderPrint({
   df <-  data()
    num_tweets <- df %>% filter(X1 == "num_tweets") %>% select(n)
    print(num_tweets)
  })
  
}

shinyApp(ui, server)
