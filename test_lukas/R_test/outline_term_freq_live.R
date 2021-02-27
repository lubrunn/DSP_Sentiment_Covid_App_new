



# coomon words from emojis that can be filtered out
emoji_words <- c(
  "excus",
  "tongu",
  "stick",
  "tear",
  "joy",
  "flag",
  "skin",
  "smile",
  "heart",
  "eye",
  "index",
  "medium",
  "laugh",
  "loud",
  "roll",
  "floor",
  "mark",
  "exclam",
  "hand",
  "clap",
  "dollar",
  "hot",
  "light",
  "blow",
  "kiss",
  "amulet",
  "head",
  "tree",
  "speaker",
  "symbol",
  "money",
  "point",
  "grin",
  "biceps",
  "flex",
  "note",
  "popper",
  "fist",
  "car",
  "follow",
  "retweet",
  "year",
  "ago",
  "scoial media",
  "woman",
  "voltag",
  "star",
  "ball",
  "camera",
  "man",
  "ass",
  "video",
  "cake",
  "cool"
)

##### die parameter in querry dann durch input$date1 etc. ersetzen und filtern
con <- DBI::dbConnect(RSQLite::SQLite(), "test.db")
time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, "SELECT * FROM test_term_freq WHERE date_variable >= '2018-11-30' and date_variable < '2021-02-11'")
print(Sys.time() -  time1)

# dann mit output plot erstellen
# oder alternativ direkt aggregieren --> noch testen was schneller ist
# quasi statt colsums in R sum in sql
df_need2 <- df_need %>% 
  select(-c(date_variable, language_variable, retweets_count, likes_count, tweet_length)) %>%
  colSums(na.rm = T) %>%
  data.frame() %>%
  rename("n" = ".") %>%
  rownames_to_column("X1")
df_need2 %>%
  filter(X1 != "num_tweets" &
           !grepl(emoji_words, X1)) %>%
  top_n(20) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = X1, y = n)) +
  geom_col() +
  coord_flip()
