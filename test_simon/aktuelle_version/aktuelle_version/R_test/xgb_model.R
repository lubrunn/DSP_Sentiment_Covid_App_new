#' Plot Output Functions
#' @export
#' @rdname xgboost
model_xgb <- function(res){
  

  
  slidng_eval_window <- sliding_period(res,index = date,"month",lookback = 4 , assess_stop = 1,step = 1)
  
  res$date <- NULL
  
  preprocessing_recipe <-
    recipes::recipe(Close ~ ., data = res) %>% prep()


  model_xgboost <- boost_tree(
    mode = "regression",
    mtry = 20,
    trees = 200,
    min_n = 3,
    tree_depth = 8,
    learn_rate = 0.01,
    loss_reduction = 0.01,
    sample_size = 0.7) %>%
    set_engine(engine = "xgboost", objective = "reg:squarederror")

  xgboost_wf <-
    workflows::workflow() %>%
    add_model(model_xgboost) %>%
    add_formula(Close ~ .)
  #
  #   folds <- vfold_cv(res, v = 3)
  #
  #   rf_fit_rs <-
  #     xgboost_wf %>%
  #     fit_resamples(folds)
  #
  #   a <- collect_metrics(rf_fit_rs)
  #
  xgboost_tuned <- tune::tune_grid(
    object = xgboost_wf,
    resamples = slidng_eval_window,
    metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae)
  )


  xgboost_best_params <- xgboost_tuned %>%
    tune::select_best("rmse")

  xgboost_model_final <- model_xgboost %>%
    finalize_model(xgboost_best_params)

  train_processed <- bake(preprocessing_recipe,  new_data = res)

  xgboost_model_final %>%
    fit(
      formula = Close ~ .,
      data    = train_processed
    )

  return(xgboost_model_final)
}

#' @export
#' @rdname xgboost
model_xgb_custom <- function(res,mtry,trees,min_n,tree_depth,learn_rate,loss_reduction,cv,
                             sample_size){

  res$date <- NULL

  # val_split <- rsample::initial_split(
  #   res,
  #   prop = 1,
  #   strata = Close
  # )

  preprocessing_recipe <-
    recipes::recipe(Close ~ ., data = res) %>% prep()

  cv_folds <-
    recipes::bake(
      preprocessing_recipe,
      new_data = res
    ) %>%
    rsample::vfold_cv(v = cv)


  model_xgboost <- boost_tree(
    mode = "regression",
    mtry = mtry,
    trees = trees,
    min_n = min_n,
    tree_depth = tree_depth,
    learn_rate = learn_rate,
    sample_size = sample_size,
    loss_reduction = loss_reduction) %>%
    set_engine(engine = "xgboost", objective = "reg:squarederror")

  xgboost_wf <-
    workflows::workflow() %>%
    add_model(model_xgboost) %>%
    add_formula(Close ~ .)
  #
  #   folds <- vfold_cv(res, v = 3)
  #
  #   rf_fit_rs <-
  #     xgboost_wf %>%
  #     fit_resamples(folds)
  #
  #   a <- collect_metrics(rf_fit_rs)
  #
  xgboost_tuned <- tune::tune_grid(
    object = xgboost_wf,
    resamples = cv_folds,
    metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae)
  )


  xgboost_best_params <- xgboost_tuned %>%
    tune::select_best("rmse")

  xgboost_model_final <- model_xgboost %>%
    finalize_model(xgboost_best_params)

  train_processed <- bake(preprocessing_recipe,  new_data = res)

  xgboost_model_final %>%
    fit(
      formula = Close ~ .,
      data    = train_processed
    )

  return(xgboost_model_final)
}
#' @export
#' @rdname xgboost
#'
model_xgb_hyp <- function(res,trees_hyp,cv_hyp,grid_size){

  res$date <- NULL

  # val_split <- rsample::initial_split(
  #   res,
  #   prop = 1,
  #   strata = Close
  # )

  cv_folds <-
    recipes::bake(
      preprocessing_recipe,
      new_data = res
    ) %>%
    rsample::vfold_cv(v = cv_hyp)

  xgboost_model <-
    parsnip::boost_tree(
      mode = "regression",
      trees = trees_hyp,
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
    set_engine("xgboost", objective = "reg:squarederror")

  xgboost_params <-
    dials::parameters(
      min_n(),
      tree_depth(),
      learn_rate(),
      loss_reduction()
    )

  xgboost_grid <-
    dials::grid_max_entropy(
      xgboost_params,
      size = grid_size
    )


  xgboost_wf <-
    workflows::workflow() %>%
    add_model(xgboost_model) %>%
    add_formula(Close ~ .)

  xgboost_tuned <- tune::tune_grid(
    object = xgboost_wf,
    resamples = cv_folds,
    grid = xgboost_grid,
    metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae),
    control = tune::control_grid(verbose = TRUE)
  )


  xgboost_best_params <- xgboost_tuned %>%
    tune::select_best("rmse")

  xgboost_model_final <- xgboost_model %>%
    finalize_model(xgboost_best_params)



  train_processed <- bake(preprocessing_recipe,  new_data = res)

  xgboost_model_final %>%
    fit(
      formula = Close ~ .,
      data    = train_processed
    )

  return(xgboost_model_final)
}
