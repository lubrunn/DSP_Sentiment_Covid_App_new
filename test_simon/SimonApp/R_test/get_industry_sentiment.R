#' Function to gather industry specific sentiments

#' @export
#' @rdname industry_sentiment

get_industry_sentiment <- function(en,de,industry,retweets_min){
  de <- de %>%  filter(Symbol == "ADS.DE"|Symbol == "ALV.DE" |
                                      Symbol == "DBK.DE" | Symbol == "DHER.DE")

 # res_en <- de %>% filter(sector == industry)
  res_de <- de %>% filter(sector == "Financial Services")
  symbols <- c(res_de[["Symbol"]]) #,res_en[["Symbol"]]
  df_total = data.frame()
  for (s in symbols) {

    load_data <- eval(parse(text = paste(s,'()', sep='')))
    load_data <- load_data %>% filter(retweets_count > retweets_min)
    senti_stock <- aggregate_sentiment(load_data2)

    df_total <- rbind(df_total,senti_stock)
  }
  df_total <- df_total %>% group_by(date,language) %>%
    summarise_at(vars("sentiment_weight_retweet", "sentiment_weight_likes",
                      "sentiment_weight_length","sentiment_mean"), mean)
}
# maybe pull that function outside into serve reactive
