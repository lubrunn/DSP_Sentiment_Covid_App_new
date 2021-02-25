

#' @export
#' @rdname multiple_plotting
Multiple_input <- function(filtered_df,aggregation,listi,key){
  
  if(length(aggregation) == 1){
    aggregation <- key[[aggregation]]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation)
    
  }else if(length(aggregation) == 2){
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]])
    
  }else if(length(aggregation) == 3){
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]],
                                                 aggregation[[3]])
  }else{
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]],
                                                 aggregation[[3]],aggregation[[4]])}
  
  
  
  
  
}

#' @export
#' @rdname multiple_plotting
ticker_dict <- function(stock){
  
  listi <- list("adidas " = "ADS.DE","Allianz " = "ALV.DE",
                "Deutsche Bank " = "DBK.DE","Delivery Hero " = "DHER.DE")
  
  res <- listi[[stock]]
  
}

#' @export
#' @rdname multiple_plotting
key <- function(){
  
  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")
  
  
}


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
              sentiment_mean = mean(sentiment))
  
  
  
}

