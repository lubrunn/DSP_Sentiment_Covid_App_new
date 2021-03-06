

### feather
df <- arrow::read_feather("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter/En_NoFilter_2021-02-19.feather")
#### csv
df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter/En_NoFilter_2021-02-19.csv")



time1 <- Sys.time()
arrow_read <- function(){
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
    networkD3::forceNetwork(
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
      linkDistance = 100, # length of links
      charge =  -70, # the more negative the furher away nodes,
      linkColour = "red", #color of links
      bounded = F, # if T plot is limited and can not extend outside of box
      # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
      colourScale = networkD3::JS(ColourScale)
    )
}





time1 <- Sys.time()
fread_read <- function(){
  df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter/En_NoFilter_2021-02-19.csv")

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

  # how to calculates manually :
  # e.g. for word "trump":
    # count number of word pairs that contain trump eihter item1 or item2

  deg <- igraph::degree(network, mode="all")
  network.D3$nodes$size <- deg * 3

  ### add word correlation to nodes
  network.D3$links$corr <- network$correlation










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
    linkWidth = JS("function(d) { return d.value * 5; }"),
    #linkWidth = 1, # width of the linkgs
    #linkWidth = JS("function(d) { return d.value * 100; }"),
    fontSize = 30, # font size of words
    zoom = TRUE,
    opacityNoHover = 100,
    linkDistance = 100, # length of links
    #linkDistance = JS("function(d){return d.value * 150}"),
    charge =  -70, # the more negative the furher away nodes,
    linkColour = "red", #color of links
    bounded = F, # if T plot is limited and can not extend outside of box
    # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
    colourScale = networkD3::JS(ColourScale)
  )
}


microbenchmark::microbenchmark(arrow_read(), fread_read(), times = 20)



##########################################
###############################################
############################################
#########################################



### filter out rows that are same but with switches item1 and item2











df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter/En_NoFilter_2021-02-19.csv")

input_n_all <- 50

input_rt = 0
input_likes = 0
input_length = 0

input_search_term <- NULL
input_username <- NULL

input_n_subset <- 50

input_min_corr <- 0.15

# unneest the words
network_df <-  df %>%
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
  filter(correlation > input_min_corr)


network_df <- network_df[!duplicated(t(apply(network_df,1,sort))),]



network <- network_df %>% # optional
  igraph::graph_from_data_frame(directed = FALSE)






# Create networkD3 object.
network.D3 <- networkD3::igraph_to_networkD3(g = network)
# Define node size.
# network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)

# degree is number of adjacent edges --> here we set the size of nodes proportional to the degree
# i.e. the more adjacent words a node has the bigger it will appear

# how to calculates manually :
# e.g. for word "trump":
# count number of word pairs that contain trump eihter item1 or item2

deg <- igraph::degree(network, mode="all")
network.D3$nodes$size <- deg * 3

### add word correlation to nodes
network.D3$links$corr <- network$correlation










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
  linkWidth = JS("function(d) { return d.value * 5; }"),
  #linkWidth = 1, # width of the linkgs
  #linkWidth = JS("function(d) { return d.value * 100; }"),
  fontSize = 30, # font size of words
  zoom = TRUE,
  opacityNoHover = 100,
  linkDistance = 100, # length of links
  #linkDistance = JS("function(d){return d.value * 150}"),
  charge =  -70, # the more negative the furher away nodes,
  linkColour = "red", #color of links
  bounded = F, # if T plot is limited and can not extend outside of box
  # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
  colourScale = networkD3::JS(ColourScale)
)

















############################## mutliple days

input_date1 <- "2020-01-01"
input_date2 <- '2020-01-05'
time1 <- Sys.time()
### read all files for the dates
date_list <- seq(as.Date(input_date1), as.Date(input_date2), "days")
df_all <- NULL

all_files <- list.files("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter")
all_files <- all_files[grepl(".feather", all_files) & grepl(paste(date_list, collapse = "|"), all_files)]


#df <-  vroom::vroom(file.path("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter", all_files))

for (file in all_files){
df <- arrow::read_feather(file.path("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter", file))
#df <- fread(file.path("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter", file))
if (is.null(df)){
  df_all <- df
} else {
  df_all <- bind_rows(df_all, df)
}
}

df <- df_all
input_n_all <- 50

input_rt = 0
input_likes = 0
input_length = 0

input_search_term <- NULL
input_username <- NULL

input_n_subset <- 50

input_min_corr <- 0.15

# unneest the words
network_df <-  df %>%
  select(doc_id, text, created_at) %>%
  tidytext::unnest_tokens(word, text)


network_df <- network_df %>%
  left_join(subset(df, select = c(doc_id, text, retweets_count, likes_count,
                                  tweet_length, username, sentiment)))

network_df <- network_df %>%

  # filter out uncommon words
  group_by(word) %>%
  filter(n() >= input_n_all) %>%
  ungroup()

network_df <- network_df %>%

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
  filter(n() >= input_n_subset)

network_df <- network_df %>%

  # compute word correlations

  widyr::pairwise_cor(word, doc_id, sort = TRUE)

network_df <- network_df %>%

  # create network
  # filter out words with too low correaltion as baseline and even more if user
  # want it
  filter(correlation > 0.15) %>% # fix in order to avoid overcrowed plot
  filter(correlation > input_min_corr)


network_df <- network_df[!duplicated(t(apply(network_df,1,sort))),]



network <- network_df %>% # optional
  igraph::graph_from_data_frame(directed = FALSE)






# Create networkD3 object.
network.D3 <- networkD3::igraph_to_networkD3(g = network)
# Define node size.
# network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)

# degree is number of adjacent edges --> here we set the size of nodes proportional to the degree
# i.e. the more adjacent words a node has the bigger it will appear

# how to calculates manually :
# e.g. for word "trump":
# count number of word pairs that contain trump eihter item1 or item2

deg <- igraph::degree(network, mode="all")
network.D3$nodes$size <- deg * 3

### add word correlation to nodes
network.D3$links$corr <- network$correlation










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
  linkWidth = JS("function(d) { return d.value * 5; }"),
  #linkWidth = 1, # width of the linkgs
  #linkWidth = JS("function(d) { return d.value * 100; }"),
  fontSize = 30, # font size of words
  zoom = TRUE,
  opacityNoHover = 100,
  linkDistance = 100, # length of links
  #linkDistance = JS("function(d){return d.value * 150}"),
  charge =  -70, # the more negative the furher away nodes,
  linkColour = "red", #color of links
  bounded = F, # if T plot is limited and can not extend outside of box
  # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
  colourScale = networkD3::JS(ColourScale)
)

Sys.time() - time1
















######### data.table
############################## mutliple days

input_date1 <- "2020-01-01"
input_date2 <- '2020-01-05'
time1 <- Sys.time()
### read all files for the dates
date_list <- seq(as.Date(input_date1), as.Date(input_date2), "days")
df_all <- NULL

all_files <- list.files("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter")
all_files <- all_files[grepl(".feather", all_files) & grepl(paste(date_list, collapse = "|"), all_files)]


#df <-  vroom::vroom(file.path("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter", all_files))

for (file in all_files){
  df <- arrow::read_feather(file.path("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter", file))
  #df <- fread(file.path("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_sentiment/En_NoFilter", file))
  if (is.null(df)){
    df_all <- df
  } else {
    df_all <- bind_rows(df_all, df)
  }
}

df <- df_all
input_n_all <- 50

input_rt = 0
input_likes = 0
input_length = 0
input_sentiment = -1

input_search_term <- "trump"
input_username <- "trump"

input_n_subset <- 50

input_min_corr <- 0.15

# unneest the words
network_df <-  df %>%
  # filter out according to user
  filter(
    retweets_count >= input_rt &
      likes_count >= input_likes &
      tweet_length >= input_length &
      sentiment > input_sentiment
  ) %>%
  select(doc_id, text, created_at) %>%
  tidytext::unnest_tokens(word, text)


network_df <- network_df %>%
  left_join(subset(df, select = c(doc_id, text, username)))



time1 <- Sys.time()

network_df2 <- network_df %>%

  # filter out uncommon words
  group_by(word) %>%
  filter(n() >= input_n_all) %>%
  ungroup()

Sys.time() - time1

network_df2 <- network_df2 %>%


  # if list provided to specify tweets to look at then extract only those tweets
  { if (!is.null(input_search_term)) filter(., grepl(paste(input_search_term, collapse="|"), text)) else . } %>%
  { if (!is.null(input_username)) filter(., grepl(paste(input_username, collapse="|"), username)) else . } %>%
  # count number of words
  group_by(word) %>%
  filter(n() >= input_n_subset)
Sys.time() - time1


#### datatalbe

time1 <- Sys.time()
network_dt <- network_df
setDT(network_dt)
network_dt2 <- network_dt[, if (.N >= input_n_all) .SD, word]
Sys.time() - time1


network_dt2 <- network_dt2[like(text, input_search_term)]
network_dt2 <- network_dt2[like(username, input_username),
            if (.N >= input_n_subset) .SD, word]

Sys.time() - time1









network_df <- network_df %>%

  # compute word correlations

  widyr::pairwise_cor(word, doc_id, sort = TRUE)

network_df <- network_df %>%

  # create network
  # filter out words with too low correaltion as baseline and even more if user
  # want it
  filter(correlation > 0.15) %>% # fix in order to avoid overcrowed plot
  filter(correlation > input_min_corr)


network_df <- network_df[!duplicated(t(apply(network_df,1,sort))),]



network <- network_df %>% # optional
  igraph::graph_from_data_frame(directed = FALSE)






# Create networkD3 object.
network.D3 <- networkD3::igraph_to_networkD3(g = network)
# Define node size.
# network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)

# degree is number of adjacent edges --> here we set the size of nodes proportional to the degree
# i.e. the more adjacent words a node has the bigger it will appear

# how to calculates manually :
# e.g. for word "trump":
# count number of word pairs that contain trump eihter item1 or item2

deg <- igraph::degree(network, mode="all")
network.D3$nodes$size <- deg * 3

### add word correlation to nodes
network.D3$links$corr <- network$correlation










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
  linkWidth = JS("function(d) { return d.value * 5; }"),
  #linkWidth = 1, # width of the linkgs
  #linkWidth = JS("function(d) { return d.value * 100; }"),
  fontSize = 30, # font size of words
  zoom = TRUE,
  opacityNoHover = 100,
  linkDistance = 100, # length of links
  #linkDistance = JS("function(d){return d.value * 150}"),
  charge =  -70, # the more negative the furher away nodes,
  linkColour = "red", #color of links
  bounded = F, # if T plot is limited and can not extend outside of box
  # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
  colourScale = networkD3::JS(ColourScale)
)

Sys.time() - time1


























################################################################## barplot
######### controls
number_words <- 20000

# here only combinations were words of interest appear in item1
# so bigrams without words are ommitted --> in network plot they are kept


input_n_all <- 50

input_rt = 0
input_likes = 0
input_length = 0

input_search_term <- NULL
input_username <- NULL

input_n_subset <- 50

input_min_corr <- 0.15

# unneest the words
network_df <-  df %>%
  select(doc_id, text, created_at) %>%
  tidytext::unnest_tokens(word, text)


network_df <- network_df %>%
  left_join(subset(df, select = c(doc_id, text, retweets_count, likes_count,
                                  tweet_length, username, sentiment)))

network_df <- network_df %>%

  # filter out uncommon words
  group_by(word) %>%
  filter(n() >= input_n_all) %>%
  ungroup()

network_df <- network_df %>%

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
  filter(n() >= input_n_subset)

network_df <- network_df %>%

  # compute word correlations

  widyr::pairwise_cor(word, doc_id, sort = TRUE)





############ for specific word
tomatch <- "trump"
number_words <- 20



network_df %>%
  filter(item1 %in% tomatch) %>%
  group_by(item1) %>%
  top_n(number_words) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()



########### general
network_df %>%

  #filter(item1 %in% tomatch) %>%
  #group_by(item1) %>%
  top_n(number_words) %>%
  ungroup() %>%
  unite(words, item1, item2, sep = " ", remove= F) %>%
  mutate(words = reorder(words, correlation)) %>%
  ggplot(aes(words, correlation)) +
  geom_bar(stat = "identity") +
#  facet_wrap(~ item1, scales = "free") +
  coord_flip()
