input_date1 <- "2020-01-01"
input_date2 <- '2020-01-02'
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

df <- head(df_all, 50000)






n <- 2
threshold <- 50
























######################################## bigram



bi.gram.words <- df %>%
  unnest_tokens(
    input = text,
    output = ngram,
    token = 'ngrams',
    n = n
  ) %>%
  filter(! is.na(ngram))

# list of words to split into

split_cols <- c()
for (i in 1:n){
  word <- glue("word{i}")
  split_cols <- rlist::list.append(split_cols, word)
}

bi.gram.words %<>%
  separate(col = ngram, into = split_cols, sep = ' ') %>%
  filter(! is.na(.))


# count number of times 4 words occur together
bi.gram.count <- bi.gram.words %>%
  select(split_cols) %>%
  group_by_all() %>%
  summarise(n = n())  %>%
  arrange(desc(n)) %>%
  # We rename the weight column so that the
  # associated network gets the weights (see below).
  rename(weight = n)






# set threshold --> only select word combinations that appear more than
# threshold times






network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes <- network.D3$nodes %>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes <- network.D3$nodes %>% mutate(Group = 1)
# Define edges width.
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links,
  Nodes = network.D3$nodes,
  Source = 'source',
  Target = 'target',
  NodeID = 'name',
  Group = 'Group',
  opacity = 0.9,
  #Value = 'Width',
  #Nodesize = 'Degree',
  # We input a JavaScript function.
  #linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
  fontSize = 12,
  zoom = TRUE,
  opacityNoHover = 1
)






















######################################################### correlation


bi.gram.words_corr <- df %>%
  unnest_tokens(
    input = text,
    output = ngram,
    token = 'ngrams',
    n = n
  ) %>%
  filter(! is.na(ngram))

# list of words to split into

split_cols <- c()
for (i in 1:n){
  word <- glue("word{i}")
  split_cols <- rlist::list.append(split_cols, word)
}

bi.gram.words_corr %<>%
  separate(col = ngram, into = split_cols, sep = ' ') %>%
  filter(! is.na(.))


# count number of times 4 words occur together
bi.gram.count_corr <- bi.gram.words_corr %>%
  select(split_cols) %>%
  group_by_all() %>%
  summarise(n = n())  %>%
  arrange(desc(n)) %>%
  # We rename the weight column so that the
  # associated network gets the weights (see below).
  rename(weight = n)




network_corr <-  bi.gram.count_corr %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)





# Create networkD3 object.
network.D3 <- networkD3::igraph_to_networkD3(g = network_corr)
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
#network.D3$links$corr <- network$correlation










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
  linkWidth = JS("function(d) { return d.value / 200 ; }"),
  #linkWidth = 1, # width of the linkgs
  #linkWidth = JS("function(d) { return d.value * 100; }"),
  fontSize = 20, # font size of words
  zoom = TRUE,
  opacityNoHover = 1,
  #linkDistance = 100, # length of links
  #linkDistance = JS("function(d){return d.value * 150}"),
  #charge =  -70, # the more negative the furher away nodes,
  linkColour = "red", #color of links
  bounded = F, # if T plot is limited and can not extend outside of box
  # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")# change color scheme
  #colourScale = networkD3::JS(ColourScale)
)



