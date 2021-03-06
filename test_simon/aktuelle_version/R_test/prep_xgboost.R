
#' @export
#' @rdname xgboost_prep

split_data <- function(sample,spliti){
  names(sample)[1] <- "date"
  sample <- sample %>%
    dplyr::mutate(.,
                  months = lubridate::month(date),
                  years = lubridate::year(date),
                  weeks = lubridate::week(date),
                  days = lubridate::day(date))

  n_sample <- round(nrow(sample)*spliti)
  split.at = sample[n_sample,"date"]
  split = which(sample$date<split.at)
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
#' @export
#' @rdname xgboost_prep
ARMA_creator <- function(res,number_of_vars,var_1,var_2,var_3,num_1,num_2,num_3
                         ,num_4,num_5,num_6){
  
  if(number_of_vars == 1){
    list_var <- list(var_1)
    list_ar <- list(num_2)
    list_ma <- list(num_1)
    
  }else{
    list_var <- list(var_2,var_3)
    list_ar <- list(num_4,num_6)
    list_ma <- list(num_3,num_5)
  }
  
  bb <- mapply(c,list_ar, list_var, SIMPLIFY = T)
  cc <- mapply(c,list_ma, list_var, SIMPLIFY = T)
  
  for(i in 1:length(list_var)){
    cols_ar <- AR_creator(res,bb[2,i],bb[1,i])
    res <- cbind(res,cols_ar)
  }
  for(i in 1:length(list_var)){
    cols_ma <- MA_creator(res,cc[2,i],cc[1,i])
    res <- cbind(res,cols_ma)
  }
  
  return(res)
}