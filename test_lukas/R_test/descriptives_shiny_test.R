querry_str <- "SELECT retweets_count, sum(N) as  n  FROM histo_rt_en  WHERE date >= '2018-11-30' and date <= '2021-02-13'
and retweets_count_filter = 0 and likes_count_filter = 0 and
tweet_length_filter = 0
group by retweets_count
"



old_wd <- getwd()
setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases")
con <- DBI::dbConnect(RSQLite::SQLite(), "test.db")
setwd(old_wd)
time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)



df_need %>%
  # filter(likes_count < 10000
  #         # likes_count > 0
  #        ) %>%





  mutate(log_metric = log(retweets_count + 0.0001),
    bins = cut_interval(log_metric, n = 100)) %>%
ggplot(aes(bins, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_density()




########## time series
querry_str <- "SELECT date, N as value FROM sum_stats_en  WHERE date >= '2018-11-30' and date <= '2021-02-19'
and retweets_count = 0 and likes_count = 0 and
tweet_length = 0"


old_wd <- getwd()
setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases")
con <- DBI::dbConnect(RSQLite::SQLite(), "test.db")
setwd(old_wd)
time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)


# convert date to date
df_need$date <- as.Date(df_need$date)

df_need %>%
  ggplot() +
  geom_line(aes(date, value))






###### boxplot
box_data <- rep(df_need$retweets_count, df_need$n) %>% data.frame() %>% rename(metric = ".")
ggplot(box_data) +
  geom_boxplot(aes(metric))


