querry_str <- "SELECT likes_count, N as  n  FROM histo_likes_en  WHERE date >= '2018-11-30' and date <= '2021-02-19'
and retweets_count_filter = 0 and likes_count_filter = 0 and
tweet_length_filter = 0

"



old_wd <- getwd()
setwd("C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases")
con <- DBI::dbConnect(RSQLite::SQLite(), "test.db")
setwd(old_wd)
time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
a <- df_need %>% group_by(likes_count) %>% summarise(n = sum(n))
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)
#
# time1 <- Sys.time()
# #df_need <- read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/De_NoFilter/histo_likes_De_NoFilter_rt_0_li_0_lo_all.csv")
# df_need <- df_need %>% group_by(likes_count) %>% summarise(n = sum(N))
# print(Sys.time() -  time1)

df_need %>%
  # filter(likes_count < 10000
  #         # likes_count > 0
  #        ) %>%


  group_by(likes_count) %>% summarise(n = sum(n)) %>%


  mutate(log_metric = log(likes_count + 0.0001),
    bins = cut_interval(log_metric, n = 100)) %>%
ggplot(aes(bins, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_density()




########## time series
querry_str <- "SELECT created_at, mean_rt, mean_sentiment, mean_sentiment_rt FROM sum_stats_en
WHERE created_at >= '2018-11-30' and created_at <= '2021-02-19'
and retweets_count = 0 and likes_count = 0 and
tweet_length = 0"




con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases/test.db")

time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)


# convert date to date
df_need$created_at <- as.Date(df_need$created_at)

df_need %>%
  ggplot() +
  geom_line(aes(created_at, scale(mean_rt)), color = "red") +
  geom_line(aes(created_at, scale(mean_sentiment)), color = "blue") +
  geom_line(aes(created_at, scale(mean_sentiment_rt)), color = "orange")


df_need %>%
  ggplot() +
  #geom_histogram(aes(mean_rt)) +
  geom_violin(aes(1, scale(mean_rt))) +
  geom_violin(aes(2,scale( mean_sentiment))) +
  geom_violin(aes(3, scale(mean_sentiment_rt)))


###### boxplot
box_data <- rep(df_need$retweets_count, df_need$n) %>% data.frame() %>% rename(metric = ".")
ggplot(box_data) +
  geom_boxplot(aes(metric))











######
querry_str <- "select * from cleaned_en where date < '2019-05-10' and date > '2019-05-01'"




con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases/test.db")

time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)






querry_str <- "SELECT *  FROM sum_stats_en  WHERE
          retweets_count = 0 and likes_count = 0 and
         tweet_length = 0"


con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/OneDrive - UT Cloud/Data/SQLiteStudio/databases/test.db")

time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)



## testing local database

querry_str <- "select * from cleaned_en where text like '%trump%' and date < '2019-05-05'"


con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/lukas/Documents/SQLiteStudio/databases/test.db")

time1 <- Sys.time()
df_need <- DBI::dbGetQuery(con, querry_str)
print(Sys.time() -  time1)


#disconnect
DBI::dbDisconnect(con)


