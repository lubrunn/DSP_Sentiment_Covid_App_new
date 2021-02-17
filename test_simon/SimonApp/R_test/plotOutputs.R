#' Plot Output Functions

#' @export
#' @rdname timeseries

TS_plot <- function(filtered_df,aggregation,type) {

  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")

  if (type == "NoFilter"){
    aggregation <- key[[aggregation]]

    date = "date"

    ggplot(filtered_df, aes_string(x = date, y = aggregation , group = 1)) +
      geom_line(color = "#b3d0ec") + labs(x = "Period")

  }else{


    aggregation <- key[[aggregation]]

    TS_Data <- filtered_df

    TS_Data <- aggregate_sentiment(TS_Data)

    date = "date"

  ggplot(TS_Data, aes_string(x = date, y = aggregation , group = 1)) +
    geom_line(color = "#b3d0ec") + labs(x = "Period")
 }
}

#' @export
#' @rdname density

density_plot <- function(filtered_df,aggregation,type) {

  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")

  if(type == "NoFilter"){
    aggregation1 <- key[[aggregation]]

    ggplot(filtered_df, aes_string(x = aggregation1)) +
      geom_density(color="black",fill="lightblue",size=1) + labs(x = aggregation)

  }else{

  aggregation1 <- key[[aggregation]]

  TS_Data <- filtered_df

  TS_Data <- aggregate_sentiment(TS_Data)

  ggplot(TS_Data, aes_string(x = aggregation1)) +
    geom_density(color="black",fill="lightblue",size=1) + labs(x = aggregation)

  }
}
