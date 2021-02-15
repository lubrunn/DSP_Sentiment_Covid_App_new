#' Data Preparation Functions
#'
#' @param test_data data object
#'
#' @export
#' @rdname dataPreps
aggregate_sentiment <- function(test_data) {

  test_data <- test_data %>%
    group_by(date,language,folder) %>%
   summarise(mean_weight_retweet = weighted.mean(sentiment,
                                               retweets_count/
                                                  sum(retweets_count)),
             mean_weight_likes = weighted.mean(sentiment,
                                               likes_count/
                                                 sum(likes_count)),
             mean_weight_length = weighted.mean(sentiment,
                                                tweet_length/
                                                  sum(tweet_length)),
             mean_no_weight = mean(sentiment))


  test_data <- test_data[order(test_data$folder,test_data$date),]

}
