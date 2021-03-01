Sys.setlocale("LC_TIME", "English")



parameter_tabsi <- tabsetPanel(
  id = "industry_tab",
  type = "hidden",
  tabPanel("no",
           selectize_Stocks_reg(COMPONENTS_DE(),COMPONENTS_US()),
           radioButtons("language1","Language of tweets ?",
                        choices = c("en","de"),inline=T),
           selectizeInput("aggregation1", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset1", "clear selected"),
           radioButtons("minRetweet_stocks1", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock1","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)


  ),
  tabPanel("yes",
           selectInput("industry", "Industry", choices = c("Consumer Cyclical","Financial Services")),

           radioButtons("language2","Language of tweets ?",
                        choices = c("en","de"),inline=T),

           selectizeInput("aggregation2", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                     "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           actionButton("reset2", "clear selected"),
           radioButtons("minRetweet_stocks2", "Select minimum number of retweets:", choices = c("0","10","50","100","200"),inline=T),
           radioButtons("tweet_length_stock2","Tweet larger than median length:",
                        choices = c("yes","no"),selected = "no",inline=T)

  )

)


parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("NoFilter",
           radioButtons("language","Choose Langugage of Tweets:",choices = c("En","De")),
           selectizeInput("aggregation", "Aggregation", choices = c("Mean","Mean weighted by retweets",
                                                                    "Mean weighted by likes", "Mean weighted by length"),
                          select = "Mean"),
           radioButtons("minRetweet", "Select minimum number of retweets", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("minLikes", "Select minimum number of likes", choices = c("0","10","50","100","200"),selected = "0",inline=T),
           radioButtons("tweet_length","Tweet larger than median length:",
                        choices = c("yes","no"),inline=T)


  ),
  tabPanel("Stocks",
           radioButtons("industry_sentiment","Sentiment by industry ?",
                        choices = c("yes","no"),selected = "no",inline=T),
           parameter_tabsi

  )

)





tabs_custom <- tabsetPanel(
  id = "regression_tabs",
  tabPanel("Model specifcation",
           radioButtons("country_regression","Which country?",c("Germany","USA"),selected = "Germany"),
           uiOutput("stock_regression"),
           radioButtons("regression_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           uiOutput("Controls"),
           actionButton("reset_regression", "clear selected"),
           #radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
           #selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
           sliderInput("date_regression",label = "Timeseries",
                       value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                       min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                       step = 1,timeFormat = "%F")



  ),
  tabPanel("Filter sentiment input",
           selectInput("Sentiment_type", "Type of Sentiment:", choices = c("NoFilter","Stocks"),
                       selected = "NoFilter"),
           parameter_tabs

  )

)



#################################################################################
################################### directory ###################################
#################################################################################

dir_setter_panel <- function() {
  tabPanel("Select Working Directory",
           sidebarPanel(

             tags$p(),
             tags$p("Please choose the directory containing the folder containig \n
               the data called 'Data'."),
             shinyFiles::shinyDirButton("directory", "Folder select", "Please select a folder"
             ),

           ),
           mainPanel(
             tags$h4("Selected folder"),
             tags$p(HTML("Please check that you picked the correct folder otherwise \n
                           the App will be not work.")),
             textOutput("directorypath"),
             tags$hr()
           ))
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
                        twitter_tab_desc
                      ),





                      ########## main panels for Descritpive reiter
                       mainPanel(
                        tabsetPanel(id = "tabselected",
                          tabPanel("Descriptives Output", value = 1,
                                   conditionalPanel(
                                     condition = "input.plot_type_desc == 'histo'",
                                     plotOutput("histo_plot") %>%
                                       shinycssloaders::withSpinner()

                                   ),
                                   conditionalPanel(
                                     condition = "input.plot_type_desc == 'sum_stats'",
                                     "text",
                                     plotOutput('sum_stats_plot') %>%
                                       shinycssloaders::withSpinner()#, height = "800px")
                                   )),
                          ##### main panel for exploratory
                          tabPanel("Exploratory Output", value = 2,
                                   mainPanel(
                                     conditionalPanel(
                                       condition = "input.plot_type_expl == 'Frequency Plot'",
                                       plotOutput("freq_plot")
                                       #uiOutput("plot.ui")
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type_expl == 'Word Cloud'",
                                       "text",
                                       wordcloud2::wordcloud2Output('wordcloud', height = "800px")
                                     )

                                   ))
                                    ))
                      ),
             ################### tab panel descirptive end




             # andere reiter
             tabPanel("Sentiment"),
             tabPanel("Daily Analysis"),
             tabPanel("Going deeper"))

}


##### main sidebar panel including both tabsetpanels
# twitter_desc_panel <- function(){
#   sidebarPanel(
#     tab_panel_twitter_desc
# )
# }

### conditional panels for the histogram plots in the descriptive panel
twitter_desc_conditional_histo <- conditionalPanel(

    #condition = "input.plot_type == 'Frequency Plot'",
    # keep for both because bigram also makes senese with wordcloud
    condition = "input.plot_type == 'histo'",

    sliderInput("bins", "Adjust the number of bins for the histogram", min = 5, max = 1000, value = 100),


    # add switch whether to use logarithmic scale
    shinyWidgets::switchInput(inputId = "log_scale", label = "Logarithmic Scale",
                              value = F,
                              size = "small",
                              handleWidth = 100
    )


  )


### conditional sidebar panel for the time series
tiwtter_desc_conditional_sum_stats <- conditionalPanel(

  #condition = "input.plot_type == 'Frequency Plot'",
  # keep for both because bigram also makes senese with wordcloud
  condition = "input.plot_type == 'sum_stats'",
  radioButtons("metric", "Select a metric",
               choiceNames = c("Mean", "Standard deviation", "Median"),
               choiceValues = c("mean", "std", "median"))
)

#### sidebar layout for descriptives
twitter_tab_desc <- tabPanel( "Descriptives",


                              ####### both
                              radioButtons("lang", "Select Language", choices = c("EN", "DE")),
                              selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),

                              dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2021-02-19",
                                             min = "2018-11-30", max = "2021-02-19", format = "yyyy-mm-dd"),
                              radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
                                           inline = T),
                              radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
                                           inline = T),
                              #switchInput(inputId = "long", value = TRUE),
                              shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),



                              ##### only descr
                              conditionalPanel(
                                condition = "input.tabselected==1",
                                selectInput("plot_type_desc", "What kind of plot would you like to see?", choices = c("Time Series"="sum_stats",
                                                                                                                 "Histogram" = "histo"),
                                            selected = "sum_stats"),



                                selectInput("value", "Which value would you like to show",
                                            choices = c(
                                              "Sentiment" = "sentiment",
                                              "Retweets Weighted Sentiment" = "sentiment_rt",
                                              "Likes Weighted Sentiment" = "sentiment_likes",
                                              "Length Weighted Sentiment" = "sentiment_tweet_length",
                                              "Retweets" = "rt",
                                              "Likes"="likes",
                                              "Tweet Length" = "tweet_length",
                                              "Number of Tweets" = "N"
                                            ),
                                            selected = "rt"),


                                twitter_desc_conditional_histo,
                                tiwtter_desc_conditional_sum_stats
                              ),

                            conditionalPanel(
                              condition = "input.tabselected==2",
                              shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F),
                              selectInput("plot_type_expl", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")),
                              sliderInput("n", "Number of words to show", min = 5, max = 5000, value = 15),

                              conditionalPanel(

                                #condition = "input.plot_type == 'Frequency Plot'",
                                # keep for both because bigram also makes senese with wordcloud
                                condition = "true == true",
                                radioButtons("ngram_sel", "Would like to to see single words or bigrams?", choices = c("Unigram", "Bigram"))
                              )
                            )

                            )

######### side bar panel for frequency analysis
# twitter_tab_expl <-  tabPanel("Exploratory",
#
#
#                               ###### both
#                               radioButtons("lang", "Select Language", choices = c("EN", "DE")),
#                               selectInput("comp", "Choose a company (optional)", choices = c("Adidas", "3M", ""), selected = ""),
#
#                               dateRangeInput("dates", "Select date range:", start = "2018-11-30", end = "2021-02-13",
#                                              min = "2018-11-30", max = "2018-12-09", format = "yyyy-mm-dd"),
#                               radioButtons("rt", "minimum rt", choices = c(0, 10, 50, 100, 200), selected = 0,
#                                            inline = T),
#                               radioButtons("likes", "minimum likes", choices = c(0, 10, 50, 100, 200), selected = 0,
#                                            inline = T),
#                               #switchInput(inputId = "long", value = TRUE),
#                               shinyWidgets::materialSwitch(inputId = "long", label = "Long Tweets only?", value = F),
#
#
#
#
#                               ##### only expl
#                               shinyWidgets::materialSwitch(inputId = "emo", label = "Remove Emoji Words?", value = F),
#                               selectInput("plot_type", "What kind of plot would you like to see?", choices = c("Frequency Plot", "Word Cloud")),
#                               sliderInput("n", "Number of words to show", min = 5, max = 5000, value = 15),
#
#                               conditionalPanel(
#
#                                 #condition = "input.plot_type == 'Frequency Plot'",
#                                 # keep for both because bigram also makes senese with wordcloud
#                                 condition = "true == true",
#                                 radioButtons("ngram_sel", "Would like to to see single words or bigrams?", choices = c("Unigram", "Bigram"))
#                               )
# )


# ################  sidebar tabs
# tab_panel_twitter_desc <- tabsetPanel(
#   id = "twitter_filter_tabs",
#
#   # descriptive
#   conditionalPanel(
#     condition = "input.tabselected==1",
#     tiwtter_tab_desc
#   ),
#
#
# # exploratory
# conditionalPanel(
#   condition = "input.tabselected==2",
#   twitter_tab_expl
# )
#
#
# )




#############################################################################
################### database
############################################################################
### connect to database






# #### word freq tab
# tab_panel_twitter_expl <-
# )



Sys.setlocale("LC_TIME", "English")
ui <- fluidPage(
  #theme = shinythemes::shinytheme("cosmo"),
  shinythemes::themeSelector(),
  #titlePanel("Sentiment_Covid_App"),
  navbarPage("APP",
             dir_setter_panel(),
              twitter_main_panel(),
             tabPanel("Sentiment"),
             tabPanel("Stocks",
                      sidebarPanel(
                        radioButtons("country_stocks","Which country?",c("Germany","USA"),selected = "Germany"),
                        #selectize_Stocks(COMPONENTS_DE()),
                        uiOutput("stock_choice"),
                        radioButtons("stock_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
                        actionButton("reset", "clear selected"),
                        checkboxInput("hovering","Enable hover",value = FALSE),
                        sliderinput_dates()
                      ),
                      mainPanel(
                        plot_stocks_DE(),
                        hover_info_DE()
                      ),#close MainPanel
             ),#close tabPanel stock
             tabPanel("Corona",
                      sidebarPanel(
                        selectize_corona(),
                        checkboxGroupInput("CoronaCountry","Country",c("Germany","United States"),selected = "Germany"),
                        sliderinput_dates_corona(),
                        checkboxInput("hovering_corona","Enable hover",value = FALSE)
                      ),
                      mainPanel(
                        plot_corona(),
                        hover_info_corona()
                      )
             ),#close tabPanel Corona
             navbarMenu("Model",
                        tabPanel("Granger",
                                 sidebarPanel(
                                   radioButtons("country_granger","Which country?",c("Germany","USA"),selected = "Germany"),
                                   uiOutput("Stock_Granger"),
                                   # selectizeInput("Stock_Granger","Choose first argument: Company or Index",
                                   #                c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                                   #                selected = "Bayer ",multiple = FALSE),
                                   radioButtons("Granger_outcome","Which variable?",c("Open","High","Low","Close","Adj.Close","Volume"),selected = "Close"),
                                   selectizeInput("Sentiment_Granger","Choose second argument: Sentiment",choices="under construction"),
                                   sliderInput("date_granger",label="Timeseries",
                                               value = c(min(as.Date(ADS()[["Date"]], "%b %d, %Y")),max(as.Date(ADS()[["Date"]], "%b %d, %Y"))),
                                               min = min(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                                               max = max(as.Date(ADS()[["Date"]], "%b %d, %Y")),
                                               step = 1,timeFormat = "%F"),
                                   checkboxInput("direction_granger","Second variable causes first?",value = TRUE)
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Information Granger",
                                              htmlOutput("info_granger")),
                                     tabPanel("Visualize",
                                              plotOutput("stocks_granger")),
                                     tabPanel("Background-steps",
                                              htmlOutput("dickey")),
                                     tabPanel("Results",
                                              verbatimTextOutput("granger_result"),
                                              htmlOutput("granger_satz"))))),
                        tabPanel("Regression Analysis",
                                 sidebarPanel(
                                   tabs_custom
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("regression",
                                              verbatimTextOutput("testi_table"),
                                              verbatimTextOutput("senti"),
                                              verbatimTextOutput("senti_agg"),
                                              htmlOutput("regression_equation"),
                                              verbatimTextOutput("regression_result")),
                                     tabPanel("Quantile Regression",
                                              plotOutput("plot_dens_Qreg"),
                                              verbatimTextOutput("regression_result_Qreg")

                                     )
                                   )
                                 )
                        ))
  )#close tabsetPanel
)#close fluidpage
