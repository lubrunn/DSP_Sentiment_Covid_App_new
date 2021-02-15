#' Plot Output Functions

#' @export
#' @rdname timeseries

TS_plot <- function(test_data,tweetType,aggregation) {

  key <- list("Mean weighted by likes" = "Mean_weighted_likes",
              "Mean weighted by length" = "Mean_weighted_length",
              "Mean weighted by retweets" = "Mean_weighted_retweets",
              "Mean" = "Mean_no_weight")

  aggregation <- key[[aggregation]]

  TS_Data <- test_data

  TS_Data <- TS_Data %>% filter(folder == tweetType)


  TS_Data <- aggregate_sentiment(TS_Data)
  date = "date"
  ggplot(TS_Data, aes_string(x = date, y = aggregation , group = 1)) +
    geom_line(color = "#b3d0ec") + labs(x = "Period")
}
