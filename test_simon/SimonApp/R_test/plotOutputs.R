#' Plot Output Functions
#' @export
#' @rdname timeseries
TS_plot <- function(filtered_df,type,industry_sentiment,facet,tweet_length,reg_line){

   # if(tweet_length == "yes"){
  #    filtered_df <- filtered_df %>% filter(long_tweet == tweet_length)}
    res <- filtered_df
    res$date <- as.Date(res$date)

    p <- ggplot(res, aes_string(x = "date", y = "value", color = "id",group = "id")) +
         geom_line() + labs(x = "Period")+
         theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.key = element_rect(fill = "white", color = NA),
              legend.title = element_blank()) +
        ylim(-1,1) +scale_x_date(date_labels = "%m-%Y")


   if((facet != "Long-Short tweet") & (reg_line == "yes")){
      p + geom_smooth(method = lm, se=F,size = 0.1,color="black")}

   else if((facet == "Long-Short tweet") & (reg_line == "no")){
      p + facet_grid(.~long_tweet)}

   else if((facet == "Long-Short tweet") & (reg_line == "yes")){
     p + facet_grid(.~long_tweet) + geom_smooth(method = lm, se=F,size = 0.1,color="black") }

    else if((facet != "Long-Short tweet") & (reg_line == "no")){
      p}

}


#' @export
#' @rdname density

density_plot <- function(filtered_df,type,industry_sentiment,facet,tweet_length) {

  res <- filtered_df
  res$date <- as.Date(res$date)
  #  if(facet != "Long-Short tweet"){
   #   filtered_df <- filtered_df %>% filter(long_tweet == tweet_length)}


     p <- ggplot(res, aes_string("value", fill = "id",group = "id")) +
      geom_density(color="black",size=1) + labs(x = "sentiment score", y = "density") +
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


}

#' @export
#' @rdname boxplot

box_plot <- function(filtered_df,type,industry_sentiment,facet,tweet_length) {

  res <- filtered_df
  res$date <- as.Date(res$date)
  #  if(facet != "Long-Short tweet"){
  #   filtered_df <- filtered_df %>% filter(long_tweet == tweet_length)}


  p <- ggplot(filtered_df, aes_string("value", fill = "id",group = "id")) +
    geom_boxplot(color="black",size=1) +
    coord_flip() + # change y axis title
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

}




