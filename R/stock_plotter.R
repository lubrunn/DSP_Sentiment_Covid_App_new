

stock_plotter <- function(df, input_metric, input_comp){




####### filter out relevant variable and companies
    df <- df %>% select(Dates, input_metric, name) %>%
      filter(name != "DOW" &
          grepl(paste(input_comp, collapse = "|"), name))
    ## convert to wide format for dygraph
    df <- df %>% pivot_wider(names_from = name, values_from = input_metric)
    ##### one df for values, one for dates
    df_values <- df %>% select(-Dates)
    df_dates <- as.Date(df$Dates)
    # set up time series
    don <- xts::xts(df_values, df_dates)

    ### plot
    dygraphs::dygraph(don,
                      ylab = input_metric) %>%
      {if (!grepl("return", input_metric)) dygraphs::dyRebase(.,value = 100) else . } %>%
      dygraphs::dyOptions(axisLineWidth = 2) %>%
      dygraphs::dyLegend() %>%
      dygraphs::dyShading(from = min(df_dates), to = max(df_dates), color = "white")



}
