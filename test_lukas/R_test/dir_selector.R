#runAlibrary(glue)


path_setter_setup <- function(direcory, input) {

# find home direcoty of user
volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
# allow for searching directories
shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
}



# this will be reactive
# when user clicks button gets triggered
path_setter <- function(){
  
  # when shiny is initliazed directory input is set to ineger
  # set original wd to home base which is in volumes
  if (is.integer(directory)) {
    setwd(volumes)
    cat(glue("No directory has been selected. Current directory {getwd()})"))
    
  } else { #if directory has already been set before
    
    path1 <- parseDirPath(volumes, directory)
    if (input$lang == "EN"){
      path2 <- "Twitter/cleaned/En_NoFilter"
    } else {
      path2 <- "Twitter/cleaned/De_NoFilter"
    }
    file_path <- file.path(path1, path2)
    if(dir.exists(file_path)) {
      setwd(file_path)
      cat(glue("Current path {getwd()}"))
    } else {
      cat(glue("Current path selection does not contain needed data. \n
                 Resetting directory to {getwd()}"))
    }
  }
}