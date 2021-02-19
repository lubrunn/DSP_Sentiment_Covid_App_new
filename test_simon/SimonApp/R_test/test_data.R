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

#' @export
#' @rdname load_components_of_Index
COMPONENTS_DE <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/EYhDUmLHadVFlHwO8KkVUuABSVgQRPS0nadLAYaRgxIYSw?download=1"
  read.csv(filename)
}

#' @export
#' @rdname load_components_of_Index
COMPONENTS_EN <- function(){
  filename <- "https://unitc-my.sharepoint.com/:x:/g/personal/zxmvp94_s-cloud_uni-tuebingen_de/Ec9gayTfc09Psw5WKjtD7qYB8DUe3PGKp1uNgAFt41JRqA?download=1"
  read.csv(filename)
}
#sentiment based on industry?


#' @export
#' @rdname load_stocks

ADS.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/Shiny_files_companies/adidas.feather"
  arrow::read_feather(filename)
}

#' @export
#' @rdname load_stocks

ALV.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/Shiny_files_companies/Allianz.feather"
  arrow::read_feather(filename)
}

#' @export
#' @rdname load_stocks

DHER.DE <- function(){

 filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/Shiny_files_companies/Delivery Hero.feather"
 arrow::read_feather(filename)
}

#' @export
#' @rdname load_stocks

DBK.DE <- function(){

  filename <- "C:/Users/simon/OneDrive - UT Cloud/Eigene Dateien/Data/Twitter/sentiment/Shiny_files_companies/Deutsche Bank.feather"
  arrow::read_feather(filename)
}
