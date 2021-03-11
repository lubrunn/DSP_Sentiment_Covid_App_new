Sys.setlocale("LC_TIME", "English")


##### die ui struktur für die Regression und VAR sind jetzt im file "uiElements.R" als function




#################################################################################
################################### directory ###################################
#################################################################################

dir_setter_panel <- function() {
  tabPanel("Select Working Directory",
           fluidRow(column(4,

                           tags$p(),
                           tags$p("Please choose the directory containing the folder containig \n
               the data called 'Data'."),
                           shinyFiles::shinyDirButton("directory", "Select folder", "Please select a folder"
                           ),
                           ## in case dir path chooser not working enter manually
                           tags$br(),
                           tags$br(),
                           tags$p("In the case that choosing a path through the 'Select Folder' button \
             is not possible you can also enter your path manually"),
                           textInput("dir_path_man", ""),
                           actionButton("dir_path_man_btn", "Set path")

           ),
           column(8,
                  tags$h4("Selected folder"),
                  tags$p(HTML("Please check that you picked the correct folder containing \n
             the 'Data' folder. otherwise the App will not work.")),
                  textOutput("directorypath"),
                  tags$hr())
           ),


           fluidRow(column(12, align = "center",
                           tags$hr(),
                           tags$p(),
                           imageOutput("twitter_logo")
           )
           )

  )
}


###############################################################################
######################### twitter #############################################
###############################################################################

# the main panel for twitter containing all sub panels and main panels
twitter_main_panel <- function(){
  navbarMenu("Twitter",

             ###### tab panel for descriptive
             tabPanel("Descriptives Main",
                      # sidebar panel for descriptive
                      #twitter_desc_panel(),
                      sidebarPanel(

                        twitter_tab_desc,

                      ),







                      ########## main panels for Descritpive reiter
                      mainPanel(
                        tabsetPanel(id = "tabselected",

                                    ### panel with histograms and summary table
                                    tabPanel("Time Series Sentiment & Other Metrics", value = 1,

                                             #########################################
                                             ###########################################

                                             # first time series plot
                                             textOutput("number_tweets_info"),
                                             tags$head(tags$style("#number_tweets_info{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white;
                                 }"
                                             )
                                             ),
                                             #plotOutput('sum_stats_plot'),
                                             tags$head(
                                               # Note the wrapping of the string in HTML()
                                               tags$style(HTML("

                                        .dygraph-axis-label {
                                          font-size: 12px;
                                          color:white;
                                        }
                                        .dygraph-legend {
                                          color: black;
                                        }


                                                     "))
                                             ),
                                             tags$h4("Time Series"),
                                             dygraphs::dygraphOutput("sum_stats_plot"),

                                             # seconds time series plot
                                             tags$br(),

                                             # tags$br(),
                                             # tags$h4("Saved Time Series"),
                                             # dygraphs::dygraphOutput('sum_stats_plot2'),



                                             #########################################
                                             ###########################################




                                             #########################################
                                             ###########################################

                                             #########################################
                                             ###########################################





                                    ),

                                    ##### main panel with wod frequency and raw tweets
                                    tabPanel("Exploratory Output", value = 3,

                                             tags$br(),
                                             htmlOutput("number_words"),
                                             conditionalPanel(
                                               condition = "input.plot_type_expl == 'Frequency Plot'",
                                               plotly::plotlyOutput("freq_plot", height = "1000px")%>% shinycssloaders::withSpinner(type = 5)
                                               #uiOutput("plot.ui")
                                             ),
                                             conditionalPanel(
                                               condition = "input.plot_type_expl == 'Word Cloud'",
                                               uiOutput("cloud"),
                                               #wordcloud2::wordcloud2Output('wordcloud', height = "1000px", width = "auto")
                                             )
                                             # tags$hr(),
                                             # tags$br(),
                                             # tags$br(),
                                             # tags$br(),
                                             # tags$br(),
                                             # tags$br(),
                                             # tags$hr(),
                                             # conditionalPanel(
                                             #   condition = "input.ngram_sel == 'Bigram'",
                                             #   tags$h4("Number of Bigrams containing the choosen word (if no word selected shows all tweets in current selection)"),
                                             # plotly::plotlyOutput("word_freq_time_series") %>% shinycssloaders::withSpinner(type = 5)
                                             # )


                                    )
                        )),
                      conditionalPanel(
                        condition = "input.tabselected == 1",
                        fluidRow(column(10, style='padding:0px;',
                                        #summary statistics table
                                        tags$head(tags$style(HTML("#sum_stats_table{
                                   color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white !important;
                                 }"
                                        )
                                        )),
                                        tags$h4("Summary Statistics"),
                                        tableOutput("sum_stats_table")
                                        )),

                        fluidRow(column(10,

                                        ##### violin plot
                                        tags$h4("Distrubtion of aggregated tweets"),
                                        plotOutput("violin_sum")

                        )),

                        sidebarPanel(
                          histo_tab
                        ),
                        mainPanel(
                          textOutput("histo_plot_info"),
                          tags$head(tags$style("#histo_plot_info{
                                 font-size: 20px;
                                 font-style: bold;
                                 color: white;
                                 }"
                          )
                          ),
                          plotly::plotlyOutput("histo_plot")
                        )
                      )

             )
             ################### tab panel descirptive end



  )


}


histo_tab <- tabPanel(tags$h3("Histogram"),
                       selectInput("histo_value", "Which value would you like to show",
                                   choices = c(
                                     "Sentiment" = "sentiment",
                                     #"Retweets Weighted Sentiment" = "sentiment_rt",
                                     # "Likes Weighted Sentiment" = "sentiment_likes",
                                     #"Length Weighted Sentiment" = "sentiment_tweet_length",
                                     "Retweets" = "rt",
                                     "Likes"="likes",
                                     "Tweet Length" = "tweet_length"
                                   ),
                                   selected = "sentiment"),
                       sliderInput("bins", "Adjust the number of bins for the histogram", min = 5, max = 500, value = 50),


                       # add switch whether to use logarithmic scale
                       shinyWidgets::switchInput(inputId = "log_scale", label = "Logarithmic Scale",
                                                 value = F,
                                                 size = "small",
                                                 handleWidth = 100))

### conditional sidebar panel for the time series
twitter_desc_conditional_sum_stats <- conditionalPanel(

  #condition = "input.plot_type == 'Frequency Plot'",
  # keep for both because bigram also makes senese with wordcloud
  condition = "input.tabselected==1",
  radioButtons("metric", "Select a metric",
               choiceNames = c("Mean", "Standard deviation", "Median"),
               choiceValues = c("mean", "std", "median")),
  checkboxInput("num_tweets_box", label = "Show the average number of tweets per day", value = F),
  actionButton("plot_saver_button", "Save the plot")
)

#### sidebar layout for descriptives
twitter_tab_desc <- tabPanel( "Descriptives",


                              ####### all three
                              radioButtons("lang", "Select Language", choices = c("EN", "DE"), inline = T),

                              selectInput("comp","Choose tweets",
                                          #company_terms,
                                          c("a"),
                                          selected = "NoFilter"),
                              shinyWidgets::airDatepickerInput("dates_desc", "Date range:",
                                                               range = T,
                                                               value = c("2018-11-30", "2021-02-19"),
                                                               maxDate = "2021-02-19", minDate = "2018-11-30",
                                                               clearButton = T, update_on = "close"),
                              actionButton("reset_dates_desc", "Reset date range"),




                              radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                                           inline = T),
                              radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                                           inline = T),
                              #switchInput(inputId = "long", value = TRUE),
                              shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),



                              ##### only descr
                              conditionalPanel(
                                condition = "input.tabselected==1",




                                selectInput("value", "Which value would you like to show",
                                            choices = c(
                                              "Sentiment" = "sentiment",
                                              "Retweets Weighted Sentiment" = "sentiment_rt",
                                              "Likes Weighted Sentiment" = "sentiment_likes",
                                              "Length Weighted Sentiment" = "sentiment_tweet_length",
                                              "Retweets" = "rt",
                                              "Likes"="likes",
                                              "Tweet Length" = "tweet_length"
                                            ),
                                            selected = "sentiment", multiple = T),

                                # additional elemtns for time series analysis
                                twitter_desc_conditional_sum_stats,






                              ),
                              ######################################################################################
                              #####################################################################################
                              ############################### Word Frequencies ####################################
                              #####################################################################################
                              conditionalPanel(
                                condition = "input.tabselected==3",
                                shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F),
                                selectInput("plot_type_expl", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")
                                ),

                                conditionalPanel(
                                  condition = "input.plot_type_expl == 'Word Cloud'",
                                  sliderInput("size_wordcloud", "Change the size of the wordcloud", min = 0.1, max = 10, value = 1, step = 0.1)
                                ),
                                conditionalPanel(
                                  condition = "input.plot_type_expl == 'Frequency Plot'",
                                  sliderInput("n_freq", "Number of words to show", min = 5, max = 100, value = 20)

                                ),
                                conditionalPanel(
                                  condition = "input.plot_type_expl != 'Frequency Plot'",
                                  sliderInput("n_freq_wc", "Number of words to show", min = 5, max = 1000, value = 100)
                                ),


                                conditionalPanel(

                                  #condition = "input.plot_type == 'Frequency Plot'",
                                  # keep for both because bigram also makes senese with wordcloud
                                  condition = "true == true",
                                  radioButtons("ngram_sel", "Would like to to see single words or bigrams?", choices = c("Unigram", "Bigram"))
                                ),

                                # word search bigrams
                                conditionalPanel(
                                  condition = "input.ngram_sel == 'Bigram'",
                                  shinyWidgets::searchInput("word_freq_filter", "Show bigrams containing specific terms",
                                                            placeholder = "Placeholder",
                                                            value = "",
                                                            btnSearch = icon("search"),
                                                            btnReset = icon("remove"))
                                ),

                              )

)






#############################################################################
################### database
############################################################################
### connect to database






# #### word freq tab
# tab_panel_twitter_expl <-
# )



Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(
  #### this gets the dimension of the current window --> helps adjusting width and height of plots that
  # dont do it automatically
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
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  theme = shinythemes::shinytheme("superhero"),
  #shinythemes::themeSelector(),
  #titlePanel("Sentiment_Covid_App"),
  navbarPage("APP",

             dir_setter_panel(),
             twitter_main_panel()

)#close fluidpage
)


server <- function(input,output){
  output$sum_stats_table <- renderTable({

    mtcars
  })

  output$violin_sum <- renderPlot({

    mtcars %>% ggplot() +
      geom_line(aes(mpg, cyl))
  })
}

shinyApp(ui, server)




