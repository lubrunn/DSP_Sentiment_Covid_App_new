
#' @export
#' @rdname xgboost_prep

split_data <- function(sample,split){
  names(sample)[1] <- "date"
  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date))

  n_sample <- round(nrow(sample)*split)
  split.at = sample[n_sample,"date"]
  split = which(aa$date<split.at)
  out <- NULL
  out$df.train = sample[split,] # Predictor training data set
  out$y.train = sample[split,c(2)] # Outcome for training data set
  out$date.train = sample[split,c(1)] # Date, not a predictor but useful for plotting

  out$df.test  = sample[-split,] # Predictors for testing/evaluation data set
  out$y.test = sample[-split,c(2)] # Outcome for testing data set
  out$date.test = sample[-split,c(1)] # date for test data set

  return(out)
}


#' @export
#' @rdname xgboost_prep
AR_creator <- function(df,variable,lag){
  names(df)[1] <- "date"
  df$date <- as.Date(df$date)

  xts_object <- df %>%
    tk_xts(silent = TRUE)

  xts_object <- lag.xts(xts_object[,variable], k = 1:lag)
  
  df <- xts_object %>%
    tk_tbl() %>% dplyr::select(-index)
  names(df)[1] <- paste(variable,".")
  df <- as.data.frame(df)
  return(df)
}

#' @export
#' @rdname xgboost_prep
MA_creator <- function(df,variable,avg_len){
  avg_len <- as.numeric(avg_len)
  x <- zoo(df[,variable])

  df <- as.data.frame(zoo::rollmean(x, k = avg_len, fill = NA))
  names(df)[1] <- paste("MA_",variable)
  
  return(df)

}
