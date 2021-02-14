
#' Test Functions
#' @export
#' @rdname test_data
test_data <- function(){
  filename <- system.file("data", "beerSalesSubset.csv", package = "SentimentApp")
  #filename <- "data/beerSalesSubset.csv"
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/ESTGmTK06PJEgx5bZ99GploBA6csaAbkLRTdmpMoXDnP9A?e=okeMyu"
  test_data1 <- read.csv(url(filename))

  test <- httr::GET(filename)
  test2 <- httr::content(test)

  #test_data1 <- get("iris", "package:datasets")


}





getData <- function(test_data1){
  head(test_data1)
}

plotdata <- function(test_data1){
  ggplot(test_data1, aes(price_ounce, move_ounce)) +
    geom_point()
}






