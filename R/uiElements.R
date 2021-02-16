#' UI Elements
#'
#'


#####################################################   Stocks
# selectize input for companies
#' @export
#' @rdname uiElements
selectize_Stocks <- function(components) {
  selectizeInput("Stock","Chose Company",
                 c(components[["Company.Name"]]),
                 multiple=TRUE,
                 selected = "")
}
# sliderinput for dates
#' @export
#' @rdname uiElements
sliderinput_dates <- function(){
  sliderInput("dates",label="Timeseries",
              value = c(median(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
              min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
              max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
              step = 1,timeFormat = "%F")
}
#plotoutput for german companies
#' @export
#' @rdname uiElements
plot_stocks_DE <- function() {
  plotOutput("plot_DE",hover = hoverOpts("plot_hover_DE", delay = 10, delayType = "debounce"))
}
#hoverbox in german plot
#' @export
#' @rdname uiElements
hover_info_DE <- function() {
  uiOutput("hover_info_DE")
}
#plotoutput for us commpanies
#' @export
#' @rdname uiElements
plot_stocks_US <- function() {
  plotOutput("plot_US",hover = hoverOpts("plot_hover_US", delay = 10, delayType = "debounce"))
}
#hoverbox in us plot
#' @export
#' @rdname uiElements
hover_info_US <- function() {
  uiOutput("hover_info_US")
}



