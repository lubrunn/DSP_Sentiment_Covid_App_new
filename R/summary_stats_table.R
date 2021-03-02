
# con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases/test.db")
# time1 <- Sys.time()
# df_need <- DBI::dbGetQuery(con, "SELECT * FROM sum_stats_de WHERE created_at >= '2018-11-30' and created_at <= '2021-02-19'
#                            and likes_count = 200 and retweets_count = 200 and tweet_length = 81")
# print(Sys.time() -  time1)
#
# df_need %>%
#   ggplot() +
#   geom_histogram(aes(retweets_count))
# Sys.time() - time1





#input = "sentiment_likes"
#'@export
#'@rdname sum_stats_table_ccreator

## setup dataframe for average summary statistics
sum_stats_table_creator <- function(input, df_need){
if(!grepl("senti", input)){
sum_stats <- df_need %>%
  select(-c(likes_count, retweets_count, tweet_length, language, created_at)) %>%

  summarise_all(mean) %>%
   select(contains(input) & !contains("senti"), N) %>%
  round(2)

} else if (input == "sentiment") {
  sum_stats <- df_need %>%
    select(-c(likes_count, retweets_count, tweet_length, language, created_at)) %>%

    summarise_all(mean) %>%
    select(matches(input) & !matches("rt|likes|length"), N) %>%
    round(2)
} else {
  sum_stats <- df_need %>%
    select(-c(likes_count, retweets_count, tweet_length, language, created_at)) %>%

    summarise_all(mean) %>%
    select(matches(input), N) %>%
    round(2)
}

### convert column names
names(sum_stats) <- mgsub::mgsub(names(sum_stats), c("_", input), c("", "")) %>% stringr::str_to_title()

return(knitr::kable(sum_stats, caption = glue("Summary Statistics for the selected Metric")) %>%
         kableExtra::kable_styling("striped", full_width = T,
                                   position = "center",
                                   font_size = 16))

}
