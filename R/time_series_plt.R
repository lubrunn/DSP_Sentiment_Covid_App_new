

time_series_plotter <- function(df, filter_type, selected_metrics){

df <-     df %>%
     select(created_at, contains("mean"), -language) %>%
     pivot_longer(
        !created_at, names_to = "metric", values_to = "value"
        #,names_prefix = "mean_"
      ) %>%
    bind_rows(
      df %>%
        select(created_at, contains("std"), -language) %>%
        pivot_longer(
          !created_at, names_to = "metric", values_to = "value"
          #,names_prefix = "mean_"
        )
    ) %>%
    bind_rows(
      df %>%
        select(created_at, contains("median"), -language) %>%
        pivot_longer(
          !created_at, names_to = "metric", values_to = "value"
          #,names_prefix = "mean_"
        )
    ) %>%
    separate(col = metric, into = c("type", "metric"), sep = "_", remove = F, extra = "merge") %>%

    filter(type == filter_type & metric %in% selected_metrics)



if (length(selected_metrics) == 1){
    df %>%
    ggplot() +
    geom_line(aes(x = as.Date(created_at), y = value, color = metric))

  } else {
    df_all <- NULL
    for (metric_value in selected_metrics){

       df_filt <- df %>% filter(metric == metric_value)
      df_filt$value <-  scale(df_filt$value)

      if (is.null(df_all)){
        df_all <- df_filt
      } else{
        df_all <- rbind(df_all, df_filt)
      }



    }
    df_all %>%
      ggplot() +
      geom_line(aes(x = as.Date(created_at), y = value, color = metric))

  }


}
