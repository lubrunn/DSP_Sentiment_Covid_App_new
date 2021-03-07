time1 <- Sys.time()

  df <- arrow::read_feather("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter/En_NoFilter_2021-02-19.feather")

  input_n_all <- 50

  input_rt = 0
  input_likes = 0
  input_length = 0

  input_search_term <- NULL
  input_username <- NULL

  input_n_subset <- 50

  input_min_corr <- 0.15

  # unneest the words
  network <-  df %>%
    select(doc_id, text, created_at) %>%
    tidytext::unnest_tokens(word, text) %>%
    left_join(subset(df, select = c(doc_id, text, retweets_count, likes_count,
                                    tweet_length, username, sentiment))) %>%

    # filter out uncommon words
    group_by(word) %>%
    filter(n() >= input_n_all) %>%
    ungroup() %>%

    # filter out according to user
    filter(
      retweets_count >= input_rt &
        likes_count >= input_likes &
        tweet_length >= input_length
    ) %>%
    # if list provided to specify tweets to look at then extract only those tweets
    { if (!is.null(input_search_term)) filter(., grepl(paste(input_search_term, collapse="|"), text)) else . } %>%
    { if (is.null(input_username)) filter(., grepl(paste(input_username, collapse="|"), username)) else . } %>%
    # count number of words
    group_by(word) %>%
    filter(n() >= input_n_subset) %>%

    # compute word correlations
    widyr::pairwise_cor(word, doc_id, sort = TRUE) %>%

    # create network
    # filter out words with too low correaltion as baseline and even more if user
    # want it
    filter(correlation > 0.15) %>% # fix in order to avoid overcrowed plot
    filter(correlation > input_min_corr) %>% # optional
    igraph::graph_from_data_frame(directed = FALSE)






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
 fn <- networkD3::forceNetwork(
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
    radiusCalculation = networkD3::JS("Math.sqrt(d.nodesize)+2"), # radius of nodes (not sure whats difference to nodesize but has different effect)
    # We input a JavaScript function.
    #linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
    linkWidth = 1, # width of the linkgs
    fontSize = 30, # font size of words
    zoom = TRUE,
    opacityNoHover = 100,
    #linkDistance = 100, # length of links
    charge =  -70, # the more negative the furher away nodes,
    #linkColour = "red", #color of links
    bounded = F, # if T plot is limited and can not extend outside of box
    # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),# change color scheme
    colourScale = networkD3::JS(ColourScale),
    linkColour = "lightblue",
    linkDistance =
      JS('function(){d3.select("body").style("background-color", "#313632");return 50;}')

    )
### working with changing font color
  htmlwidgets::onRender(
    fn,
    'function(el, x) {
    d3.select("body").style("background-color", "#313632");
    d3.selectAll(".legend text").style("fill", "white");
    d3.selectAll("text").style("fill", "#ff2a00");
    d3.selectAll("text").style("stroke", "white");

  }'
  )

### working with changing color on click
  htmlwidgets::onRender(
    fn,
    'function(el, x) {
    d3.select("body").style("background-color", "#313632");
    d3.selectAll(".legend text").style("fill", "white");
    d3.selectAll("text").style("fill", "#ff2a00");
    d3.selectAll("text").style("stroke", "white");
    d3.selectAll("circle").style("fill", "green");
    d3.selectAll("circle").on("click", function(d,i){
    d3.select(this).style("fill", "orange");
    });

  }'
  )

### working with changing color on hover
  htmlwidgets::onRender(
    fn,
    'function(el, x) {
    d3.select("body").style("background-color", "#313632");
    d3.selectAll(".legend text").style("fill", "white");
    d3.selectAll("text").style("fill", "#ff2a00");
    d3.selectAll("text").style("stroke", "white");
    d3.selectAll("circle").style("fill", "green");
    d3.selectAll("circle").on("mouseover", function(d,i){
    d3.select(this).style("fill", "orange");
    });
     d3.selectAll("circle").on("mouseout", function(d,i){
    d3.select(this).style("fill", "green");
    });

  }'
  )



  htmlwidgets::onRender(
    fn,
    'function(el, x) {
    d3.select("body").style("background-color", "#313632");
    d3.selectAll(".legend text").style("fill", "white");
    d3.selectAll("text").style("fill", "#ff2a00");
    d3.selectAll("text").style("stroke", "white");
    d3.selectAll("circle").style("fill", "green");
    d3.selectAll("links").on("mouseover", function(d,i){
    d3.select(this).style("fill", "orange");
    });
     d3.selectAll("links").on("mouseout", function(d,i){
    d3.select(this).style("fill", "green");
    });

  }'
  )


