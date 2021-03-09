library(tidyverse)


df <- read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq/En_NoFilter/bi_appended/bi_En_NoFilter_rt_0_li_0_lo_all.csv") %>%
  filter(between(date, as.Date("2018-11-30"), as.Date("2021-02-19")))

 df %>%
  filter(
    emo == F | emo == T &
    retweets_count >= 0 &
           likes_count >= 0 &
           tweet_length >= 0) %>%
  select(-emo)
 # %>%
#   pivot_wider(names_from = word, values_from = N)


df <- readr::read_csv(file_path, col_types = cols(date_variable = "D")) %>%
  filter(between(date_variable, input$dates[1], input$dates[2]))

input_n <- 30
term_frequency_df <-  df %>%

  filter(
    emo == F | emo == T &
      retweets_count >= 0 &
      likes_count >= 0 &
      tweet_length >= 0) %>%
  {if (F == F) filter(., emo == F) else .} %>%
  filter(grepl(c("covid"), word)) %>%
  select(-emo) %>%
  group_by( word) %>%
  summarise(n = sum(N)) %>%
  arrange(desc(n))
  #pivot_wider(names_from = word, values_from = N) %>%


########################## barplot
input_n =30
options(scipen=999)
df %>%
  group_by( word) %>%
  summarise(n = sum(N)) %>%
  arrange(desc(n)) %>%
  top_n(input_n, n) %>%
  ggplot(aes(reorder(x = word, n), y = n)) +
  geom_col(width = 0.5) +
  coord_flip() +
  labs(x = "",
       y = "N")+
  theme_classic() +
  theme(text = element_text(size=18)) +

  scale_y_continuous(expand = c(0, 0))


library(wordcloud2)

term_frequency_df  %>%
  top_n(input_n, n) %>%  wordcloud2::wordcloud2(term_frequency_df, size = 1,shape = 'star',
           color = "random-light", backgroundColor = "grey")
a <- demoFreq


df %>%
 filter(emo == F | emo == T &
  retweets_count >= 0 &
  likes_count >= 0 &
  tweet_length >= 0) %>%
  filter(grepl("covid", word)) %>%
  group_by(date) %>%
  summarise(n = sum(N)) %>%
  ggplot() +
 geom_line(aes(date, n))
