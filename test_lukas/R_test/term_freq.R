setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq")


path <- "En_Nofilter"
file.info <- NULL

files <- list.files(path)

file <- files[1]



# split file name into dataframe containing info about which dataframestring contains which filters
rt <- as.numeric(stringr::str_match(file, "rt_\\s*(.*?)\\s*_li")[2])
likes <- as.numeric(stringr::str_match(file, "li_\\s*(.*?)\\s*_lo")[2])
long <- as.numeric(stringr::str_match(file, "lo_\\s*(.*?)\\s*.csv")[2])

# setup df
if (is.null(file_info)){
file_info <- data.frame(file = file, rt = rt, likes = likes, long = long)
} else{
  file_info_new <- data.frame(file = file, rt = rt, likes = likes, long = long)
  rbind(file_info, file_info_new)
}

file_path <- file.path(path, file)

df <- readr::read_csv(file_path)


ui <- fluidPage(
  
  titlePanel("term frequencies"),
  
  sidebarLayout(
    
    sidebarPanel(
      dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2018-12-09",
                     min = "2018-11-30", max = "2018-12-09", format = "yyyy-mm-dd")
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(session, input, output){
  
}

shinyApp(ui, server)