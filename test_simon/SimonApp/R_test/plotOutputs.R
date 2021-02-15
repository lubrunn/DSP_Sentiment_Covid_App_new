#' Plot Output Functions

#' @export
#' @rdname timeseries

TS_plot <- function(filtered_df_min_retweet,aggregation) {

  key <- list("Mean weighted by likes" = "Mean_weighted_likes",
              "Mean weighted by length" = "Mean_weighted_length",
              "Mean weighted by retweets" = "Mean_weighted_retweets",
              "Mean" = "Mean_no_weight")

  aggregation <- key[[aggregation]]

  TS_Data <- filtered_df_min_retweet

  TS_Data <- aggregate_sentiment(TS_Data)

  date = "date"

  ggplot(TS_Data, aes_string(x = date, y = aggregation , group = 1)) +
    geom_line(color = "#b3d0ec") + labs(x = "Period")
}

#' @export
#' @rdname density

density_plot <- function(filtered_df_min_retweet,aggregation) {

  key <- list("Mean weighted by likes" = "Mean_weighted_likes",
              "Mean weighted by length" = "Mean_weighted_length",
              "Mean weighted by retweets" = "Mean_weighted_retweets",
              "Mean" = "Mean_no_weight")

  aggregation1 <- key[[aggregation]]

  TS_Data <- filtered_df_min_retweet

  TS_Data <- aggregate_sentiment(TS_Data)

  ggplot(TS_Data, aes_string(x = aggregation1)) +
    geom_density(color="black",fill="lightblue",size=1) + labs(x = aggregation)
}
