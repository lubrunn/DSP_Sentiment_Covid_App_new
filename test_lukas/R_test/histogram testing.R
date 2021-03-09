df_orig <- data.table::fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/histo_rt_En_NoFilter_rt_0_li_0_lo_all.csv")
df <- df_orig
options(scipen=10000)

df <- df_orig %>% filter(created_at == "2018-11-30") %>% select(retweets_count, N)

input_metric <- "retweets_count"
input_bins <- 50
input_log <- T
input_metric_name <- "Sentiment"


df <- df[,
   list(sum_n = sum(N)),
   by = c(input_metric)]



if (input_log == T){
  df[, metric := log10(get(input_metric) + 1.5)]
} else{
  setnames(df, input_metric, "metric")
}



# cut into intervalls according to bin input
df[, bins := cut_interval(metric, n = input_bins)]


#### count bins
df <- df[, .(sum_n = sum(sum_n)), by = bins]


#### create tick marks series
df <- df %>% separate(bins, c("bin1", "bin2"), ",")
## remove brackets
df$bin1 <- gsub("[(]", "", df$bin1)
df$bin1 <- gsub("\\[|\\]", "", df$bin1)
df$bin2 <- gsub("[)]", "", df$bin2)
df$bin2 <- gsub("\\[|\\]", "", df$bin2)


# convert to numeric
df$bin1 <- as.numeric(df$bin1)
df$bin2 <- as.numeric(df$bin2)

### take mean
df <- df %>% mutate(mean_bin = rowMeans(select(df, bin1, bin2), na.rm = T))


#### take mean column as x -axis



# plot

p <- df %>%
  select(mean_bin, sum_n) %>%
  setNames(c(input_metric_name, "N")) %>%

  ggplot(aes(.data[[input_metric_name]], N)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_classic() +
  geom_hline(yintercept = 0)


plotly::ggplotly(p)









# count number of tweets per metric bin
df <- df[,
         list(sum_n = sum(N)),
         by = c(input_metric)]

# take log if asked
if (input_log == T){
  df[, metric := log(get(input_metric) + 1)]
} else{
  setnames(df, input_metric, "metric")
}

# cut into intervalls according to bin input
df[, bins := cut_interval(metric, n = input_bins)]
# plot
df %>%
  ggplot(aes(bins, sum_n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

unique(df$bins)






####### original histogram
library(tidyverse)
df_histo_orig <- read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/cleaned_raw_sentiment/En_NoFilter/En_NoFilter_2018-11-30.csv") %>%
  select(retweets_count)

plotly::ggplotly(
  df_histo_orig %>%
    ggplot() +
    geom_histogram(aes(retweets_count), bins = 100)
)



#####################################
###### fake histogram
df <- df_orig %>% filter(created_at == "2018-11-30") %>% select(retweets_count, N)

input_metric = "retweets_count"
input_log = F
input_bins = 100
input_metric_name = "Retweets"
df <- df[,
         list(sum_n = sum(N)),
         by = c(input_metric)]



if (input_log == T){
  df[, metric := log10(get(input_metric) + 1.5)]
} else{
  setnames(df, input_metric, "metric")
}



# cut into intervalls according to bin input
df[, bins := cut_interval(metric, n = input_bins)]


#### count bins
df <- df[, .(sum_n = sum(sum_n)), by = bins]


#### create tick marks series
df <- df %>% separate(bins, c("bin1", "bin2"), ",")
## remove brackets
df$bin1 <- gsub("[(]", "", df$bin1)
df$bin1 <- gsub("\\[|\\]", "", df$bin1)
df$bin2 <- gsub("[)]", "", df$bin2)
df$bin2 <- gsub("\\[|\\]", "", df$bin2)


# convert to numeric
df$bin1 <- as.numeric(df$bin1)
df$bin2 <- as.numeric(df$bin2)

### take mean
df <- df %>% mutate(mean_bin = rowMeans(select(df, bin1, bin2), na.rm = T))


#### take mean column as x -axis



# plot

df <- df %>% arrange(bin1)
min_dist <- df$bin1[2] - df$bin1[1]

p <- df %>%
  select(bin1, sum_n) %>%
  setNames(c(input_metric_name, "N")) %>%

  ggplot(aes(.data[[input_metric_name]], N)) +
  geom_col(color = "black", fill = "grey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        aspect.ratio = 1/1) +
  theme_classic() +
  geom_hline(yintercept = 0)




plotly::ggplotly(p)



a <- hist(df_histo_orig$retweets_count, breaks = 200)
a$breaks
a$counts
