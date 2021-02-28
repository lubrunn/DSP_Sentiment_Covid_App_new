df <- read_csv("En_NoFilter/uni/term_freq_En_NoFilter_2018-11-30_rt_0_li_0_lo_all.csv",
               col_types = cols(date_variable = "D")) %>%
  filter(between(date_variable, "2018-11-30", "2018-11-30"))

df %>%
  filter(
    emo == F | emo == T &
    retweets_count >= 0 &
           likes_count >= 0 &
           tweet_length >= 0) %>%
  select(-emo) %>%
  pivot_wider(names_from = word, values_from = N)


df <- readr::read_csv(file_path, col_types = cols(date_variable = "D")) %>%
  filter(between(date_variable, input$dates[1], input$dates[2]))


term_frequency_df <-  df %>%

  filter(
    emo == F | emo == T &
      retweets_count >= 0 &
      likes_count >= 0 &
      tweet_length >= 0) %>%
  {if (F == F) filter(., emo == F) else .} %>%
  select(-emo) %>%
  pivot_wider(names_from = word, values_from = N) %>%
  select(-c(date_variable, language_variable, retweets_count,
            likes_count, tweet_length)) %>%
  colSums(na.rm = T) %>%
  data.frame() %>%
  rename("n" = ".") %>%
  rownames_to_column("words")



library(wordcloud2)
term_frequency_df <- data.frame("freq" = term_frequency) %>% rownames_to_column(var = "word")

wordcloud2(term_frequency_df, size = 1,shape = 'star',
           color = "random-light", backgroundColor = "grey")
a <- demoFreq
