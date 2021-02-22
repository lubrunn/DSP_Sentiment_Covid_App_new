###################################################
######################## live #####################
###################################################
library(networkD3)
# controllable
library(shiny)
setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/network_plot/En_NoFilter")
all_files <- list.files()




ui <- fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("dataset_load", "Choose dataset", choices = all_files),
      textInput("search_term", "Select filtering term"),
      numericInput("rt", "rt", 0),
      numericInput("likes", "rt", 0),
      numericInput("long", "rt", 0)
    ),
    
    mainPanel(
      forceNetworkOutput("plot")
    )
  )
)


server <- function(session, output, input){
  output$plot <- renderForceNetwork({
    browser()
    df <- readr::read_csv(input$dataset_load)
    
    tomatch <- input$search_term
    threshold <- 0
    min_corr <- 0.2
    
    retweets <- input$rt
    likes <- input$likes
    long <- input$long
    
    word_cors_pre <- df %>%
      filter(
        retweets_count >= retweets,
        likes_count >= likes,
        long_tweet == long
      ) %>%
      # if list provided to specify tweets to look at then extract only those tweets
      { if (tomatch != "") filter(., grepl(paste(tomatch, collapse="|"), text)) else . } %>%
      
      group_by(word) %>%
      filter(n() >= threshold)
    
    
    ###############################################
    
    word_cors <- word_cors_pre %>%
      widyr::pairwise_cor(word, doc_id, sort = TRUE) 
    
    
    network_pre <-  word_cors %>%
      #filter(item1 %in% c("covid", "trump", "china")) %>%
      filter(correlation > 0.2) %>% # fix in order to avoid overcrowed plot
      filter(correlation > min_corr) # optional
    
    
    network <- network_pre %>%
      graph_from_data_frame(directed = FALSE)
    
    
    
    # Store the degree.
    V(network)$degree <- strength(graph = network)
    
    
    # Create networkD3 object.
    network.D3 <- igraph_to_networkD3(g = network)
    # Define node size.
    network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
    # Degine color group (I will explore this feature later).
    network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)
    
    
    deg <- degree(network, mode="all")
    network.D3$nodes$size <- deg * 3
    
    
    
    
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
      height = 200
    )
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