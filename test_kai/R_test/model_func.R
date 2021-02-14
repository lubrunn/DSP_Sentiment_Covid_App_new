model_func <- function(data){

  model <- lm(Ozone ~ ., data = data)
  summary(model)
}


