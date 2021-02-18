#' Return input range

#' @export
#' @rdname observe_input

Range_input <- function(minRetweet){

    fil <- ifelse(minRetweet < 10, "10",
                  ifelse(minRetweet >= 10 & minRetweet < 20,"20",
                  ifelse(minRetweet >= 20 & minRetweet < 30,"30",
                  ifelse(minRetweet >= 30 & minRetweet < 40,"40",
                  ifelse(minRetweet >= 40 & minRetweet < 50,"50",
                  ifelse(minRetweet >= 50 & minRetweet < 60,"60",
                  ifelse(minRetweet >= 60 & minRetweet < 70,"70",
                  ifelse(minRetweet >= 70 & minRetweet < 80,"80",
                  ifelse(minRetweet >= 80 & minRetweet < 90,"90",
                  ifelse(minRetweet >= 90 & minRetweet < 100,"100",
                  ifelse(minRetweet >= 100 & minRetweet < 110,"110",
                  ifelse(minRetweet >= 110 & minRetweet < 120,"120",
                  ifelse(minRetweet >= 120 & minRetweet < 130,"130",
                  ifelse(minRetweet >= 130 & minRetweet < 140,"140",
                  ifelse(minRetweet >= 140 & minRetweet < 150,"150",
                  ifelse(minRetweet >= 150 & minRetweet < 160,"160",
                  ifelse(minRetweet >= 160 & minRetweet < 170,"170",
                  ifelse(minRetweet >= 170 & minRetweet < 180,"180",
                  ifelse(minRetweet >= 180 & minRetweet < 190,"190",
                  ifelse(minRetweet >= 190 & minRetweet < 200,"200",
                  ifelse(minRetweet >= 200 & minRetweet < 210,"210",
                  ifelse(minRetweet >= 210 & minRetweet < 220,"220",
                  ifelse(minRetweet >= 220 & minRetweet < 230,"230",
                  ifelse(minRetweet >= 230 & minRetweet < 240,"240",
                  ifelse(minRetweet >= 240 & minRetweet < 250,"250",
                  ifelse(minRetweet >= 250 & minRetweet < 260,"260",
                  ifelse(minRetweet >= 260 & minRetweet < 270,"270",
                  ifelse(minRetweet >= 270 & minRetweet < 280,"280",
                  ifelse(minRetweet >= 280 & minRetweet < 290,"290",
                  ifelse(minRetweet >= 290,"300",NA))))))))))))))))))))))))))))))
}




#' @export
#' @rdname multiple_plotting
Multiple_input <- function(filtered_df,aggregation,listi,key){

  if(length(aggregation) == 1){
    aggregation <- key[[aggregation]]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation)

  }else if(length(aggregation) == 2){
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]])

  }else if(length(aggregation) == 3){
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]],
                                                 aggregation[[3]])
  }else{
    aggregation <- key[listi]
    filtered_df <- filtered_df %>% tidyr::gather("id", "value", aggregation[[1]],aggregation[[2]],
                                                 aggregation[[3]],aggregation[[4]])}





}
