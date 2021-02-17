#' Test Functions

#' @export
#' @rdname load_filtered_files

En_NoFilter_10 <- function(){

  #filename <- "https://unitc-my.sharepoint.com/:u:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/ER8Zf-rxwnREpuxEfiqA6CABDJnPTPG-rr0UUb0jU0zgJg?download=1"
  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/Simon_test/En_NoFilter_10.feather"
  arrow::read_feather(filename)

}

#' @export
#' @rdname load_filtered_files

En_NoFilter_20 <- function(){

  #filename <- "https://unitc-my.sharepoint.com/:u:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/ER8Zf-rxwnREpuxEfiqA6CABDJnPTPG-rr0UUb0jU0zgJg?download=1"
  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/Simon_test/En_NoFilter_20.feather"
  arrow::read_feather(filename)

}








