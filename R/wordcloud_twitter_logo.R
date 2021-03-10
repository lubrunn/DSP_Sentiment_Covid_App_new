df <- readr::read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq/En_NoFilter/uni_appended/uni_En_NoFilter_rt_0_li_0_lo_all.csv") %>%
  filter(between(date, as.Date("2018-11-30"), as.Date("2021-02-19")))

emoji_words <- c(
  "plead","scream", "social media", "steam", "nose" , "box", "circl", "whit",
  "black", "button","exo", "sad",
  "love", "good", "red", "happi","mu",
  "happi","excus","tongu","stick", "tear", "joy", "flag", "skin",  "smile",
  "heart","eye", "index", "medium", "laugh", "loud", "roll", "floor","mark", "exclam",
  "hand", "clap","dollar",
  "hot", "light","blow", "kiss","amulet", "head", "tree","speaker","symbol","money","point",
  "grin","bicep","flex","note","popper","fist","car","follow","retweet","year","ago",
  "social media","woman","voltag","star","ball","camera","man","ass","video","cake","cool",
  "fac","smil","see","evil","party","sweat","thumb","big","the","crying","fing",
  "crossed","god","watch","leaf","food","arrow", "hugg", "cri", "tone", "peopl",
  "time", "today", "day"

)


df_small <- df %>%
  filter(emo == F &
           !grepl(paste(emoji_words, collapse = "|"), word) ) %>%
  group_by( word) %>%
  summarise(n = sum(N)) %>%
  arrange(desc(n)) %>%


  top_n(800, n)

library(wordcloud2)

figPath <- system.file("examples/t.png",package = "wordcloud2")

#figPath <- "C:/Users/lukas/Documents/GitHub/DSP_Sentiment_Covid_App/data/twitter_black_white.png"

wordcloud2::wordcloud2(df_small,
                       figPath = figPath,
                       size = 0.9,color = "skyblue", backgroundColor = "#2b3e50")
