#' UI Elements
#'
#'
#' @export
#' @rdname uiElements

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
  plotOutput("plot_DE",hover = hoverOpts("plot_hover_DE", delay = 10, delayType = "debounce"),
             dblclick = "plot1_dblclick",
             brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE))
}
#hoverbox in german plot
#' @export
#' @rdname uiElements
hover_info_DE <- function() {
  uiOutput("hover_info_DE",style = "pointer-events: none")
}




