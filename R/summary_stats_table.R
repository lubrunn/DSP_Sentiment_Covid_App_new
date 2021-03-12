
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





#input = "rt"
#'@export
#'@rdname sum_stats_table_creator

## setup dataframe for average summary statistics
sum_stats_table_creator <- function(df_need, input_date1, input_date2){


df_need <-   df_need %>%
  filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
  select(!contains("sentiment_")) %>%
  select(starts_with(c("mean","median", "std"))) %>%
  summarise_all(mean) %>%
  cbind(
   df_need %>%
         filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
         select(!contains("sentiment_")) %>%
         select(starts_with(c("q"))) %>%
         summarise_all(mean) %>%
        summarise_all(as.integer)
    ) %>%
  cbind(
    df_need%>%
      filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
      select(!contains("sentiment_"))  %>% summarise_at(vars(starts_with("max")), max)
  ) %>%
  cbind(
    df_need %>%
      filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
      select(!contains("sentiment_")) %>% summarise_at(vars(starts_with("min")), min)
  ) %>%
  cbind(
    df_need %>%
      filter(between(as.Date(created_at), as.Date(input_date1), as.Date(input_date2))) %>%
      select(N) %>%

      summarise(std_N = sd(N),
                mean_N = mean(N),
                median_N = median(N),
                q25_N = quantile(N, 0.25),
                q75_N = quantile(N, 0.75),
                min_N = min(N),
                max_N = max(N))
  ) %>%
  round(2) %>%
  pivot_longer(everything(),
               names_to = c(".value", "variable"),
               #prefix = "mean",
               names_pattern = "(.+)_(.+)")



  ### convert column names
  names(df_need) <- names(df_need) %>% toupper()

  # convert variable names
  df_need[,1] <- c("Retweets", "Likes", "Tweet Length", "Sentiment", "N")

  return(knitr::kable(df_need, "html") %>%
           column_spec(1:8, color = "lightgrey") %>%
           column_spec(1, bold = T, color = "white") %>%
           row_spec(1, bold = T) %>%
           kableExtra::kable_styling(c("striped","hover"), full_width = T,
                                     position = "center",
                                     font_size = 16))


}





