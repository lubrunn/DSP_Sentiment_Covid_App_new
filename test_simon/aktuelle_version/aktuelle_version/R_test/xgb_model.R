#' Plot Output Functions
#' @export
#' @rdname xgboost
model_xgb <- function(res){

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
    rsample::vfold_cv(v = 3)


  model_xgboost <- boost_tree(
    mode = "regression",
    mtry = 20,
    trees = 200,
    min_n = 3,
    tree_depth = 8,
    learn_rate = 0.01,
    loss_reduction = 0.01) %>%
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
