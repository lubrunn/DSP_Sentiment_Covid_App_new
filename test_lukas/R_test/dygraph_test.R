#install.packages("dygraphs")
library(dygraphs)
library(xts)

df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/sum_stats_En_NoFilter_rt_200_li_200_lo_long_only.csv")



df %>% select(created_at, mean_sentiment) %>% dygraph()


df$created_at <- as.Date(df$created_at)
df$mean_rt_sc <- scale(df$mean_rt)


### time series
df_values <- df %>% select(mean_rt, mean_sentiment,mean_sentiment_likes,
                           mean_likes, mean_length, mean_sentiment_length) %>% scale()

don <- xts(x = df_values, order.by = df$created_at)

dygraph(don)  %>%
  dyCSS(".dygraph-title {
  color: navy;
  font-weight: bold;
}
.dygraph-axis-label {
  font-size: 11px;
}")


dygraph(don, main = "This is a title") %>%
  dySeries("mean_rt", pointShape = "square", drawPoints = T, label = "Average Retweets") %>%
  dySeries("mean_sentiment", strokePattern = "dashed", label = "Average Sentiment") %>%
  dySeries("mean_sentiment_likes", strokePattern = "dashed", label = "Average Sentiment weighted by likes") %>%
  dySeries("mean_likes", strokePattern = "dashed", label = "Average Likes") %>%
  dySeries("mean_length", strokePattern = "dashed", label = "Average Tweet Length") %>%
  dySeries("mean_sentiment_length", strokePattern = "dashed", label = "Average Sentiment weighted by Length") %>%





  # dyHighlight(
  #   #highlightCircleSize = 5,
  #             #highlightSeriesBackgroundAlpha = 0.2,
  #             hideOnMouseOut = T) %>%
  #dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyOptions(axisLineWidth = 2, drawGrid = FALSE) %>%
  dyLegend(width = 800) %>%
  dyRangeSelector()




df_values2 <- df %>% select(mean_rt, mean_sentiment,mean_likes)
don2 <- xts(x = df_values2, order.by = df$created_at)

dygraph(don2, main = "Value") %>%
  dyRebase(value = 100) %>%
  dyRangeSelector()



###### smoothed

df_values3 <- df %>% select(mean_sentiment)
don3 <- xts(df_values3, order.by = df$created_at)

dygraph(don3, main = "Important Discoveries") %>%
  dyRebase(value = 100) %>%
  dyRoller(rollPeriod = 2) %>%
  dyAnnotation("2018-12-31", text = "A", tooltip = "New Year") %>%
  dyAnnotation("2020-06-01", text = "B", tooltip = "BLM")





####### addcolored ribbon

decreasing <- which(df$mean_sentiment < mean(df$mean_sentiment))
increasing <- which(df$mean_sentiment >= mean(df$mean_sentiment))


dyData <- xts::xts(df_values3, order.by = df$created_at)

ribbonData <- rep(0, nrow(dyData))
ribbonData[decreasing] <- 0.5
ribbonData[increasing] <- 1

dygraph(dyData) %>%
  dyRibbon(data = ribbonData, top = 0.05, bottom = 0)







##### testing violin plots

library(data.table)
library(tidyverse)
library(plotly)
df <- fread("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/plot_data/En_NoFilter/sum_stats_En_NoFilter_rt_200_li_200_lo_long_only.csv")



df_test <- rbind(df1, df2)
p <- df %>%
  ggplot() +
  geom_violin(aes(1,mean_rt))

plotly::ggplotly(p)



df1 <- df$mean_rt %>% data.frame()


df1 %>% ggplot() +
  geom_violin(aes_string(1, names(df1)[1]))
