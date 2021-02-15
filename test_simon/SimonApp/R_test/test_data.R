#' Test Functions
#' @export

#' @rdname test_data

test_data <- function(){

  filename <- "https://unitc-my.sharepoint.com/:u:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EddRSVcW2u5KmuoIE57PH8gBC27WbOQlkq1fg0owoNf6ZQ?download=1"

  #tmpFile <- tempfile()

  #download.file(filename, destfile = tmpFile)
  #filename1 <- "C:/Users/simon/Desktop/WS_20_21/DS_12/senti_data_fileType.parquet"
  filename2 <- "C:/Users/simon/Desktop/WS_20_21/DS_12/test_senti.feather"

  #test_data2 <- arrow::read_parquet(filename1)
  test_data <- arrow::read_feather(filename2)


}



