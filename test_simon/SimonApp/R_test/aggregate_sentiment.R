#' Data Preparation Functions
#'
#' @param test_data data object
#'
#' @export
#' @rdname dataPreps
aggregate_sentiment <- function(test_data) {

  test_data1 <- test_data %>%
    group_by(date,language) %>%
    summarise(sentiment_weight_retweet = weighted.mean(sentiment,
                                                       retweets_count/
                                                         sum(retweets_count)),
              sentiment_weight_likes = weighted.mean(sentiment,
                                                     likes_count/
                                                       sum(likes_count)),
              sentiment_weight_length = weighted.mean(sentiment,
                                                      tweet_length/
                                                        sum(tweet_length)),
              sentiment_mean = mean(sentiment),

              tweets_used_daily = n())



}
