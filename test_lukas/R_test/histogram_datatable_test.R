library(tidyverse)
library(data.table)


time1 <- Sys.time()
df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/histo_likes_En_NoFilter_rt_0_li_0_lo_all.csv")
Sys.time()- time1



df %>%
  filter(between(created_at, as.Date('2018-11-30'), as.Date('2021-02-19'))) %>%
  group_by(likes_count) %>% summarise(n = sum(N)) %>%


  mutate(log_metric = log(likes_count + 0.0001),
         bins = cut_interval(log_metric, n = 100)) %>%
  ggplot(aes(bins, n)) +
  geom_col()

Sys.time()- time1




time1 <- Sys.time()
df2 <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/histo_senti_En_NoFilter_rt_0_li_0_lo_all.csv",
            select = c("created_at", "sentiment_rd", "N"))

input_metric <- "sentiment_rd"


setNames(df2, sentiment_rd, "metric")

df3 <- df2[created_at >= as.Date('2018-11-30') &
     created_at <= as.Date('2021-02-19'),
   sum_n := sum(N),
   by =  c(input_metric)]

metric <- quote("likes_count")

df[, log_metric := log(get("likes_count") + 0.0001)]
df[, bins := cut_interval(log_metric, n = 100)]

df %>%
  ggplot(aes(bins, n)) +
  geom_col()
Sys.time()- time1













df2 <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/histo_senti_En_NoFilter_rt_0_li_0_lo_all.csv",
             select = c("created_at", "sentiment_rd", "N"))







## second column is always column of interest
input_metric <- "sentiment_rd"
date_input1 <- "2018-11-30"
date_input2 <- "2021-02-19"
input_log <- F
input_bins = 100


# count number of tweets per metric bin
df <- df2[created_at >= as.Date(date_input1) &
           created_at <= as.Date(date_input1),
         list(sum_n = sum(N)),
         by = c(input_metric)]

# take log if asked
if (input_log == T){
  df[, metric := log(get(input_metric) + 0.0001)]
} else{
  setnames(df, input_metric, "metric")
}

# cut into intervalls according to bin input
df[, bins := cut_interval(metric, n = input_bins)]
