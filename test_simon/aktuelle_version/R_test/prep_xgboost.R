
#' @export
#' @rdname xgboost_prep

split_data <- function(sample,split){

  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date))

  n_sample <- round(nrow(sample)*split)
  split.at = sample[n_sample,"date"]
  split = which(sample$date<split.at)

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

  xts_object <- df %>%
    tk_xts(silent = TRUE)

  xts_object <-
    merge.xts(xts_object, lag.xts(xts_object[,variable], k = 1:lag))

  df <- xts_object %>%
    tk_tbl()

  df = df[,-c(1)]

  return(df)
}

#' @export
#' @rdname xgboost_prep
MA_creator <- function(df,variable,avg_len){

  x <- zoo(df[,"Close."])

  df <- df %>%
    dplyr::mutate(MA = as.data.frame(zoo::rollmean(x, k = avg_len, fill = NA))) %>%
    dplyr::ungroup()

  return(df)

}
