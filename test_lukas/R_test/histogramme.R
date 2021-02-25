# plot histo in app
df_bins_long %>%
  filter(date >= "2018-11-30" & date <= "2019-12-07" &
           language == "de" &
           likes_count >= likes &
           retweets_count >= retweets &
           #long_tweet == long
           tweet_length >= longs) %>% 
  select(-c(date, language, retweets_count, likes_count, tweet_length)) %>%
  
  
  
  colSums(na.rm = T) %>% data.frame() %>% rownames_to_column() %>%
  rename(bins = rowname, n = ".") %>%
  mutate(bin = cut_interval(bins, n = 200))
ggplot(aes(bins, n)) +
  geom_col()