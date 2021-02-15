#' Data Preparation Functions
#'
#' @param test_data data object
#'
#' @export
#' @rdname dataPreps
aggregate_sentiment <- function(test_data) {

  test_data <- test_data %>%
    group_by(date,language,folder) %>%
   summarise(Mean_weighted_retweets = weighted.mean(sentiment,
                                               retweets_count/
                                                  sum(retweets_count)),
             Mean_weighted_likes = weighted.mean(sentiment,
                                               likes_count/
                                                 sum(likes_count)),
             Mean_weighted_length = weighted.mean(sentiment,
                                                tweet_length/
                                                  sum(tweet_length)),
             Mean_no_weight = mean(sentiment))


  test_data <- test_data[order(test_data$folder,test_data$date),]

}
