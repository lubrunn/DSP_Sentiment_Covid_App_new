#' UI Elements
#'
#'
#####################################################   Stocks
# selectize input for companies
#' @export
#' @rdname uiElements
selectize_Stocks <- function(components1,components2) {
  full_components <- rbind(components1,components2)

  selectInput("Stock","Chose Company",
              c(full_components[["Company.Name"]]),selected = "adidas ")

}


