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
                 selected = "Bayer ",multiple = TRUE
  )
}
# sliderinput for dates
#' @export
#' @rdname uiElements
sliderinput_dates <- function(){
  sliderInput("dates",label="Timeseries",
              value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
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

################################################################## CORONA
# selectize input for corona
#' @export
#' @rdname uiElements
selectize_corona <- function() {
  selectizeInput("corona_measurement","Chose Corona measurement",
                 c("total_cases","new_cases","total_deaths","new_deaths","total_cases_per_million",
                   "new_cases_per_million","total_deaths_per_million","new_deaths_per_million","reproduction_rate",
                   "icu_patients","icu_patients_per_million","hosp_patients","hosp_patients_per_million",
                   "weekly_icu_admissions","weekly_icu_admissions_per_million","weekly_hosp_admissions",
                   "weekly_hosp_admissions_per_million","new_tests","total_tests","total_tests_per_thousand",
                   "new_tests_per_thousand","positive_rate","tests_per_case","total_vaccinations","people_vaccinated",
                   "people_fully_vaccinated","new_vaccinations","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
                   "people_fully_vaccinated_per_hundred"),
                 multiple = FALSE,
                 selected = "total_deaths")
}
# sliderinput for dates
#' @export
#' @rdname uiElements
sliderinput_dates_corona <- function(){
  sliderInput("dates_corona",label="Time",
              value = c(as.Date("2020-01-22"),Sys.Date()),
              min = as.Date("2020-01-22"),
              max = Sys.Date(),
              step = 1,timeFormat = "%F")}

#' @export
#' @rdname uiElements
plot_corona <- function() {
  plotOutput("corona_plot",hover = hoverOpts("plot_hover_corona", delay = 10, delayType = "debounce"),
             dblclick = "plot_corona_dblclick",
             brush = brushOpts(id = "plot_corona_brush",resetOnNew = TRUE))
}

#' @export
#' @rdname uiElements
hover_info_corona <- function() {
  uiOutput("hover_info_corona",style = "pointer-events: none")
}



