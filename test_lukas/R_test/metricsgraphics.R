install.packages("metricsgraphics")

library(metricsgraphics)
library(RColorBrewer)



mjs_plot(rnorm(10000)) %>%
  mjs_histogram(bins=30, bar_margin=1)

movies <- ggplot2movies::movies[sample(nrow(ggplot2movies::movies), 1000), ]

mjs_plot(movies$rating) %>% mjs_histogram()

mjs_plot(movies, rating) %>% 
  mjs_histogram() %>% 
  mjs_labs(x_label="Histogram of movie ratings", 
           y_label="Frequency")

mjs_plot(movies$rating) %>% mjs_histogram(bins=30)

mjs_plot(runif(10000)) %>% 
  mjs_labs(x_label="runif(10000)") %>%
  mjs_histogram()


mjs_plot(rbeta(10000, 2, 5)) %>%
  mjs_labs(x_label="rbeta(10000, 2, 3)") %>%
  mjs_histogram(bins=100) %>% 
  mjs_axis_y(extended_ticks=TRUE)

bimod <- c(rnorm(1000, 0, 1), rnorm(1000, 3, 1))
mjs_plot(bimod) %>% mjs_histogram() 
mjs_plot(bimod) %>% mjs_histogram(bins=30) 

bimod %>% mjs_hist(30)

library(shiny)
library(metricsgraphics)

ui = shinyUI(fluidPage(
  h3("MetricsGraphics Example", style="text-align:center"),
  metricsgraphicsOutput('mjs1'),
  br(),
  metricsgraphicsOutput('mjs2')
))

server = function(input, output) {
  
  mtcars %>%
    mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
    mjs_point(color_accessor=carb, size_accessor=carb) %>%
    mjs_labs(x="Weight of Car", y="Miles per Gallon") -> m1
  
  set.seed(1492)
  stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4))
  
  stocks %>%
    mjs_plot(x=time, y=X) %>%
    mjs_line() %>%
    mjs_add_line(Y) %>%
    mjs_add_line(Z) %>%
    mjs_axis_x(xax_format="date") %>%
    mjs_add_legend(legend=c("X", "Y", "Z")) -> m2
  
  output$mjs1 <- renderMetricsgraphics(m1)
  
  output$mjs2 <- renderMetricsgraphics(m2)
  
}

shinyApp(ui = ui, server = server)
