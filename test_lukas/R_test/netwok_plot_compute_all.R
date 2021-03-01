###################################################
######################## live #####################
###################################################
library(networkD3)
# controllable
library(shiny)
library(igraph)
library(tidyverse)
library(shinyFiles) # set wd in shiny file directly
#install.packages("shinyjs")
#install.packages("shinyhelper")
library(glue)
library(shinyjs)
library(shinyhelper)
library(htmltools)

all_files <- list.files("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned/En_NoFilter")




ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),

  sidebarLayout(

    sidebarPanel(
      shinyDirButton("directory", "Folder select", "Please select a folder"),
      useShinyjs(),
      selectInput("dataset_load", "Choose dataset", choices = all_files),
      selectInput("lang", "Choose a language", choices = c("EN", "DE")),
      textInput("search_term", "Select filtering term"),
      textInput("username", "Select filtering a username"),
      numericInput("rt", "Minimum Retweets", 0),
      numericInput("likes", "Minimum Likes", 0),
      numericInput("long", "Minimum Tweet Length", 0),
      numericInput("n_all", "Miimum Number of Tweets that have word pairs",
                   min = 50, value = 50),
      numericInput("n_subset", "Miimum Number of Times words need to appear in subsample",
                   min = 0, value = 0),
      numericInput("min_corr", "Minimum Word Correlation", value = 0.15, min = 0.15, max = 1,
                   step = 0.01),
      actionButton("button", "Render Plot") %>%
        helper(type = "markdown",
               title = "Inline Help",
               content = "network_plot_button",
               buttonLabel = "Got it!",
               easyClose = FALSE,
               fade = TRUE,
               size = "s")




    ),

    mainPanel(
      forceNetworkOutput("plot"),
      verbatimTextOutput("directorypath")
    )
  )
)


server <- function(session, output, input){
  observe_helpers(help_dir = "C:/Users/lukas/Documents/GitHub/DSP_Sentiment_Covid_App/test_lukas/helpers")
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  observe({
    cat("\ninput$directory value:\n\n")
    print(input$directory)
  })

  output$directorypath <- renderPrint(
    path_setter()

  )

  path_setter <- reactive({
    #browser()
    if (is.integer(input$directory)) {
      setwd(volumes)

      cat(glue("No directory has been selected. Current directory {getwd()})"))

    } else {

      path1 <- parseDirPath(volumes, input$directory)
      if (input$lang == "EN"){
        path2 <- "Twitter/cleaned/En_NoFilter"
      } else {
        path2 <- "Twitter/cleaned/De_NoFilter"
      }
      file_path <- file.path(path1, path2)
      if(dir.exists(file_path)) {
        setwd(file_path)
        cat(glue("Current path {getwd()}"))
      } else {
        cat(glue("Current path selection does not contain needed data. \n
                 Resetting directory to {getwd()}"))
      }




    }
  })

  # if no directory selected disable button
  # this is for the inital disabling if the file does not exist in the home folder
  observe({
    #browser()
    if(!file.exists(input$dataset_load)){

      disable("button")
    }


  })



  # then if the file exists witch the button on, when it does not, turn it off again
  observeEvent(input$directory,{
    #browser()
    toggleState("button", file.exists(input$dataset_load))
  })




  # if button is clicked compute correlations und plot the plot
  observeEvent(input$button,{

    # disable the button after computation started so no new computation can
    # be startedd
    disable("button")

    # read in the data
    df <- readr::read_csv(input$dataset_load,
                          col_types = cols(.default = "c",created_at = "c",
                                           retweets_count = "i",
                                           likes_count = "i", tweet_length = "i"))

    # unneest the words
    network <-  df %>%
      select(doc_id, text, created_at) %>%
      tidytext::unnest_tokens(word, text) %>%
      left_join(subset(df, select = c(doc_id, text, retweets_count, likes_count, long_tweet,
                                      tweet_length, username))) %>%

    # filter out uncommon words
      group_by(word) %>%
      filter(n() >= input$n_all) %>%
      ungroup() %>%

      # filter out according to user
      filter(
        retweets_count >= input$rt &
          likes_count >= input$likes &
          tweet_length >= input$long
      ) %>%
      # if list provided to specify tweets to look at then extract only those tweets
      { if (input$search_term!= "") filter(., grepl(paste(input$search_term, collapse="|"), text)) else . } %>%
      { if (input$username != "") filter(., grepl(paste(input$username, collapse="|"), username)) else . } %>%
      # count number of words
      group_by(word) %>%
      filter(n() >= input$n_subset) %>%

      # compute word correlations
      widyr::pairwise_cor(word, doc_id, sort = TRUE) %>%

      # create network
      # filter out words with too low correaltion as baseline and even more if user
      # want it
      filter(correlation > 0.15) %>% # fix in order to avoid overcrowed plot
      filter(correlation > input$min_corr) %>% # optional
      graph_from_data_frame(directed = FALSE)






    # Create networkD3 object.
    network.D3 <- igraph_to_networkD3(g = network)
    # Define node size.
    # network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
    # Degine color group (I will explore this feature later).
    network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)

    # degree is number of adjacent edges --> here we set the size of nodes proportional to the degree
    # i.e. the more adjacent words a node has the bigger it will appear
    deg <- degree(network, mode="all")
    network.D3$nodes$size <- deg * 3






    # render the network plot
    output$plot <- renderForceNetwork({

      if (is.null(network.D3)) return()

      # adjust colors of nodes, first is rest, second is main node for word (with group 2)
      ColourScale <- 'd3.scaleOrdinal()
            .range(["#ff2a00" ,"#694489"]);'

      # doc: https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/forceNetwork
      forceNetwork(
        Links = network.D3$links,
        Nodes = network.D3$nodes,
        Source = 'source',
        Target = 'target',
        NodeID = 'name',
        Group = 'Group',
        opacity = 0.8,
        #Value = 'Width',
        #Nodesize = 'Degree',
        Nodesize = "size", # size of nodes, is column name or column number of network.D3$nodes df
        radiusCalculation = JS("Math.sqrt(d.nodesize)+2"), # radius of nodes (not sure whats difference to nodesize but has different effect)
        # We input a JavaScript function.
        #linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
        linkWidth = 1, # width of the linkgs
        fontSize = 30, # font size of words
        zoom = TRUE,
        opacityNoHover = 100,
        linkDistance = 100, # length of links
        charge =  -70, # the more negative the furher away nodes,
        linkColour = "red", #color of links
        bounded = F, # if T plot is limited and can not extend outside of box
        # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
        colourScale = JS(ColourScale),
        width = 200,
        height = 350
      )



    })
    enable("button")
  })

}

shinyApp(ui, server)


####### term freq plots

'
here show words that frequently appear together with words of interest
e.g.
tweets:
1. this is a tweet about trump and ivanka
2. this is also a tweet about trump and his daughter


--> process here:
all possible bigrams:
1.  this is, is this, ..., trump and, trump ivanka,...., ivnaka trump
2. ...

- keep only bigrams with trump in place 1 of bigram
- compute correlation of appearances of bigrams were trump is in place 1
- plot according to coor



'

######### controls
number_words <- 20000

# here only combinations were words of interest appear in item1
# so bigrams without words are ommitted --> in network plot they are kept

word_cors %>%
  filter(item1 %in% tomatch) %>%
  group_by(item1) %>%
  top_n(number_words) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
