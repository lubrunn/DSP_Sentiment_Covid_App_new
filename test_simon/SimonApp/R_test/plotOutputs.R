#' Plot Output Functions
aggregation = "Mean"
#' @export
#' @rdname timeseries
TS_plot <- function(filtered_df,aggregation,aggregation1,type,facet,tweet_length,industry_sentiment,language1){

  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")

  listi = c("Mean weighted by likes","Mean weighted by length","Mean weighted by retweets","Mean")

  listi1 = listi[which(listi %in% aggregation1)]
  listi = listi[which(listi %in% aggregation)]

  if (type == "NoFilter"){


    filtered_df <- Multiple_input(filtered_df,aggregation,listi,key)

    if(facet != "Long-Short tweet"){
      filtered_df <- filtered_df %>% filter(long_tweet == tweet_length)}

    filtered_df$date <- as.Date(filtered_df$date)

    p <-  ggplot(filtered_df, aes_string(x = "date", y = "value", color = "id",group = "id")) +
        geom_line() + labs(x = "Period")+
        theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.key = element_rect(fill = "white", color = NA),
              legend.title = element_blank()) +
        ylim(-1,1) +scale_x_date(date_labels = "%m-%Y")

   if(facet != "Long-Short tweet"){
       p
     }else{
       p + facet_grid(.~long_tweet)
     }

  }else{

  if(industry_sentiment == "no"){
     filtered_df <- aggregate_sentiment(filtered_df)}

    filtered_df <- filtered_df %>% filter(language == language1)

    filtered_df <- Multiple_input(filtered_df,aggregation1,listi1,key)

    filtered_df$date <- as.Date(filtered_df$date)

    ggplot(filtered_df, aes_string(x = "date", y = "value", color = "id",group = "id")) +
      geom_line() + labs(x = "Period") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_blank()) +
      ylim(-1,1) +scale_x_date(date_labels = "%m-%Y")
    }
}


#' @export
#' @rdname density

density_plot <- function(filtered_df,aggregation,type,facet,tweet_length) {

  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")

  if(type == "NoFilter"){
    listi = c("Mean weighted by likes","Mean weighted by length","Mean weighted by retweets","Mean")

    listi = listi[which(listi %in% aggregation)]

    filtered_df <- Multiple_input(filtered_df,aggregation,listi,key)

    if(facet != "Long-Short tweet"){
      filtered_df <- filtered_df %>% filter(long_tweet == tweet_length)}

     p <- ggplot(filtered_df, aes_string("value", fill = "id",group = "id")) +
      geom_density(color="black",size=1) + labs(x = aggregation, y = "density") +
      theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank()) +
    xlim(-1,1)

     if(facet != "Long-Short tweet"){
       p
     }else{
       p + facet_grid(.~long_tweet)
     }

  }else{

  aggregation1 <- key[[aggregation]]

  TS_Data <- filtered_df

  TS_Data <- aggregate_sentiment(TS_Data)

  ggplot(TS_Data, aes_string(aggregation1)) +
    geom_density(color="black",fill="lightblue",size=1) + labs(x = aggregation,y = "density") +
    theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    xlim(-1,1)

  }
}

#' @export
#' @rdname boxplot

box_plot <- function(filtered_df,aggregation,type,facet,tweet_length) {

  key <- list("Mean weighted by likes" = "sentiment_weight_likes",
              "Mean weighted by length" = "sentiment_weight_length",
              "Mean weighted by retweets" = "sentiment_weight_retweet",
              "Mean" = "sentiment_mean")

  if(type == "NoFilter"){
    listi = c("Mean weighted by likes","Mean weighted by length","Mean weighted by retweets","Mean")

    listi = listi[which(listi %in% aggregation)]

    filtered_df <- Multiple_input(filtered_df,aggregation,listi,key)

    if(facet != "Long-Short tweet"){
      filtered_df <- filtered_df %>% filter(long_tweet == tweet_length)}

    p <- ggplot(filtered_df, aes_string("value", fill = "id",group = "id")) +
      geom_boxplot(color="black",size=1) +
      labs(x = aggregation) +
      coord_flip() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank())
    if(facet != "Long-Short tweet"){
      p
    }else{
      p + facet_grid(.~long_tweet)
    }

  }else{

    aggregation1 <- key[[aggregation]]

    TS_Data <- filtered_df

    TS_Data <- aggregate_sentiment(TS_Data)

    ggplot(TS_Data, aes_string(aggregation1)) +
      geom_boxplot(color="black",fill="lightblue",size=1) +
      labs(x = aggregation) +
      coord_flip() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))

  }

}




