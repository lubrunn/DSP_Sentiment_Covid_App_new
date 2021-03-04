


# df <- read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq/En_NoFilter/bi_appended/bi_En_NoFilter_rt_0_li_0_lo_all.csv") %>%
#   filter(between(date, as.Date("2018-11-30"), as.Date("2021-02-19")))


##### additional emoji filter words
emoji_words <- c(
  "plead",
  "scream",
  "social media",
  "steam",
  "nose"

)

# df <- readr::read_csv(file_path, col_types = cols(date_variable = "D")) %>%
#   filter(between(date_variable, input_date1, input_date2))

#'@export
#'@rdname term_freq_computers
word_freq_data_wrangler <- function(df, input_date1, input_date2,
                                    input_emo, emoji_words, search_term){





df <-  df %>%
  filter(between(date, as.Date(input_date1), as.Date(input_date2)))  %>%
  {if (input_emo == T) filter(., emo == F &
                                !grepl(paste(emoji_words, collapse = "|"), word) ) else .} %>%
  {if (search_term != "") filter(.,grepl(search_term, word)) else .} %>%
  select(-emo)

return(df)
}
#
# word_freq_data_wrangler(df, input_date1, input_date2,
#                                     input_emo, emoji_words, search_term)

#'@export
#'@rdname term_freq_computers
df_filterer <- function(df, input_n){

df <- df %>%
  group_by( word) %>%
  summarise(n = sum(N)) %>%
  arrange(desc(n)) %>%
  top_n(input_n, n)

return(df)


}



#'@export
#'@rdname term_freq_computers
word_cloud_plotter <- function(df, input_size){

  df    %>%
     wordcloud2::wordcloud2(term_frequency_df, size = input_size,shape = 'star',
                                              color = "random-light", backgroundColor = "grey")
}



# term freq bar plot
term_freq_bar_plot <- function(df){
  df %>%

    ggplot(aes(reorder(x = word, n), y = n)) +
    geom_col() +
    coord_flip()


}

#'@export
#'@rdname term_freq_computers
word_filter_time_series_plotter <- function(df){


df %>%
   group_by(date) %>%
    summarise(n = sum(N)) %>%

    arrange(desc(n)) %>%
  ggplot() +
  geom_line(aes(date, n))

}


