library(tidyverse)
library(shiny)


df <- read_csv("C:/Users/lukas/OneDrive - UT Cloud/Data/Twitter/term_freq/En_NoFilter/bi_appended/bi_En_NoFilter_rt_0_li_0_lo_all.csv") %>%
  filter(between(date, as.Date("2018-11-30"), as.Date("2021-02-19")))



df <- df %>%
  group_by( word) %>%
  summarise(n = sum(N)) %>%
  arrange(desc(n))
df_small <- df %>%
  top_n(400, n)





server <- function(input, output, session) {


    output$cloud <- renderUI({

       wordcloud2::wordcloud2Output("d3TextCloud", width = 0.95 * input$dimension[1])

    })
   # invalidateLater(1000)
  output$d3TextCloud <- wordcloud2::renderWordcloud2({


    if (input$tabselected == 2){

    df_small    %>%
      top_n(input$n) %>%
      wordcloud2::wordcloud2(size = 1,
                             color = "random-light", backgroundColor = "grey",
                             widgetsize = 10)
  }
}
  )



}

ui <- fluidPage(
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  tabsetPanel(id = "tabselected",
  tabPanel(1

  ),

 tabPanel(2,
          conditionalPanel(
            condition = "input.tabselected == 2",
            uiOutput("cloud"),
            sliderInput("n", "n", min = 0, max = 400, value = 100, step= 1)),
          sliderInput("n2", "n", min = 0, max = 10000, value = 400, step= 1)
          )
    )

)


shinyApp(ui, server)
























d3wordcloud(df_small$word, df_small$n, tooltip = T)


server <- function(input, output, session) {
  output$d3TextCloud <- d3wordcloud::renderD3wordcloud({

    df_small <- df %>%
      top_n(400, n)
    d3wordcloud(df_small$word, df_small$n, tooltip = T)
  })
}

ui <- fluidPage(
  d3wordcloud::d3wordcloudOutput("d3TextCloud", width = "100%", height = 500)
)

shinyApp(ui, server)
