#'@export
#'@rdname network_plot
#'


network_plot_datagetter <- function(input_lang, input_date1, input_date2, input_company){


   # create date range
  if (!is.na(input_date2)){
   date_list <- seq(as.Date(input_date1), as.Date(input_date2), "days")
  } else{
     date_list <- input_date1
   }
   # set up empty df
   df_all <- NULL

   # source to files for no filter
   if (is.null(input_company)){

   path_source <- glue("Twitter/cleaned_sentiment/{input_lang}_NoFilter")



   }
   else { # source to files for companies
     path_source <- glue("Twitter/cleaned_sentiment/Companies2/{input_company}")
   }


   # get list of alle files we need --> dates in date list and feather files
   all_files <- list.files(path_source)[grepl(".feather", list.files(path_source)) &
                                          grepl(paste(date_list, collapse = "|"), list.files(path_source))]


  # read in all the files
   for (file in all_files){

     df <- arrow::read_feather(file.path(path_source, file))

   # if its the first file set it up as df_all
   if (is.null(df)){

        df_all <- df

     } else { # if df_all already filled --> append

       df_all <- bind_rows(df_all, df)

     }
   }




  return(df_all)

}




#'@export
#'@rdname network_plot
network_plot_filterer <- function(df, input_rt, input_likes, input_tweet_length,
                                  input_sentiment, input_search_term,
                                  input_username) {



  # unneest the words
  network <-  df %>%

    # if list provided to specify tweets to look at then extract only those tweets
    { if (input_search_term != "") filter(., grepl(paste(input_search_term, collapse="|"), text)) else . } %>%
    { if (input_username != "") filter(., grepl(paste(input_username, collapse="|"), username)) else . } %>%
    #select(doc_id, text, created_at) %>%

    filter(
      retweets_count >= input_rt &
        likes_count >= input_likes &
        tweet_length >= input_tweet_length &
        sentiment >= input_sentiment
      )

  return(network)
}




network_unnester <- function(network, df){
  network <- network %>%



    tidytext::unnest_tokens(word, text) %>%
    left_join(subset(df, select = c(doc_id, text, username)), by = "doc_id")

  return(network)

}



network_word_corr <- function(network, input_n,
                              input_corr){
  network <- network %>%

    # filter out uncommon words
    group_by(word) %>%
    filter(n() >= input_n) %>%
    ungroup()

    if (dim(network)[1] == 0){
      return()
    }

 network <- network %>%
    # compute word correlations
    widyr::pairwise_cor(word, doc_id, sort = TRUE) %>%

   na.omit() %>%

    # create network
    # filter out words with too low correaltion as baseline and even more if user
    # want it
    filter(correlation > 0.15) %>% # fix in order to avoid overcrowed plot
    filter(correlation > input_corr)

 if (dim(network)[1] == 0){
   return()
 }

  #### remove duplicates ( where item1 & item2 == item2 & item1)
  network <- network[!duplicated(t(apply(network,1,sort))),]

  if (dim(network)[1] == 0){
    return()
  }

  network <- network %>% # optional
    igraph::graph_from_data_frame(directed = FALSE)



  return(network)

}








#'@export
#'@rdname network_plot
network_plot_plotter <- function(network){


  # Create networkD3 object.
  network.D3 <- networkD3::igraph_to_networkD3(g = network)
  # Define node size.
  # network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
  # Degine color group (I will explore this feature later).
  network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)

  # degree is number of adjacent edges --> here we set the size of nodes proportional to the degree
  # i.e. the more adjacent words a node has the bigger it will appear
  deg <- igraph::degree(network, mode="all")
  network.D3$nodes$size <- deg * 3





    # adjust colors of nodes, first is rest, second is main node for word (with group 2)
    ColourScale <- 'd3.scaleOrdinal()
            .range(["#ff2a00" ,"#694489"]);'

    # doc: https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/forceNetwork
    networkD3::forceNetwork(
      Links = network.D3$links,
      Nodes = network.D3$nodes,
      Source = 'source',
      Target = 'target',
      NodeID = 'name',
      Group = 'Group',
      opacity = 0.8,
      Value = 'value',
      #Nodesize = 'Degree',
      Nodesize = "size", # size of nodes, is column name or column number of network.D3$nodes df
      radiusCalculation = networkD3::JS("Math.sqrt(d.nodesize)+2"), # radius of nodes (not sure whats difference to nodesize but has different effect)
      # We input a JavaScript function.
      #linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
     # linkWidth = 1, # width of the linkgs
      linkWidth = networkD3::JS("function(d) { return d.value * 5; }"),

      fontSize = 30, # font size of words
      zoom = TRUE,
      opacityNoHover = 100,
      linkDistance = 100, # length of links
      charge =  -70, # the more negative the furher away nodes,
      linkColour = "red", #color of links
      bounded = F, # if T plot is limited and can not extend outside of box
      # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
      colourScale = networkD3::JS(ColourScale),
      width = 200,
      height = 350
    )



}









