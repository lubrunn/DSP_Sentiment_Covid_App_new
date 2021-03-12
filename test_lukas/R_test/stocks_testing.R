df <- readr::read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Yahoo/Full/all_full.csv")

input_metric <- "Return"

df_ret <- df %>% select(Dates, input_metric, name) %>% filter(name != "DOW")

df_ret <- df_ret %>% pivot_wider(names_from = name, values_from = input_metric)


don <- xts::xts(df_ret$AAPL, df_ret$Dates)


dygraphs::dygraph(don)




