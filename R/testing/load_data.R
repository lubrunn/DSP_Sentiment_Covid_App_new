#' @export
test_data <- function(){
filename <- system.file("data", "beerSalesSubset.csv", package = "ecomAnalytics")
filename <- "data/beerSalesSubset.csv"
test_data1 <- read.csv(filename)
#test_data1 <- get("iris", "package:datasets")


}

getData <- function(test_data1){
  head(test_data1)
}

plotdata <- function(test_data1){
  ggplot(test_data1, aes(price_ounce, move_ounce)) +
    geom_point()
}






