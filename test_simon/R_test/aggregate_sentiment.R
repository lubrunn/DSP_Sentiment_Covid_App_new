#' Data Preparation Functions
#'
#' @param test_data data object
#'
#' @export
#' @rdname dataPreps
aggregate_sentiment <- function(test_data) {

  iris %>%
    group_by(Species) %>%
    summarise_at(vars(Sepal.Length),
                 list(name = mean))


}
