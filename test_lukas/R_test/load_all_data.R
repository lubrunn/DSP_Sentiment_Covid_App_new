setwd("C:/Users/lukas/OneDrive - UT Cloud/DSP_test_data/raw_test")

# folders <- list.files()

# for (folder in folders){
#   if (grepl("Companies", folder, fixed = T)) {
#     for (subfolder in list.files(folder)) {
#       filenames <- list.files(file.path(folder, subfolder))
#     }
#   } else {
#     filenames <- list.files(folder)
#   }
# }


filename <- "De_NoFilter_min_retweets_2/De_NoFilter_min_retweets_2_2018-11-30.json"
test_data_nof_1 <- jsonlite::stream_in(file(filename))
filename <- "De_NoFilter_min_retweets_2/De_NoFilter_min_retweets_2_2018-12-01.json"
test_data_nof_2 <- jsonlite::stream_in(file(filename))

test_data_nofilter <- rbind(test_data_nof_1, test_data_nof_2)
test_data_nofilter$search_term <- ""

filename <- "Companies_de/adidas_de/3M_2018-11-30_de.json"
test_data1 <- jsonlite::stream_in(file(filename))
filename <- "Companies_de/adidas_de/3M_2018-12-01_de.json"
test_data2 <- jsonlite::stream_in(file(filename))
filename <- "Companies_de/adidas_de/adidas_2018-11-30_de.json"
test_data3 <- jsonlite::stream_in(file(filename))
filename <- "Companies_de/adidas_de/adidas_2018-12-01_de.json"
test_data4 <- jsonlite::stream_in(file(filename))

# add search term column
test_data1$search_term <- "3M"
test_data2$search_term <- "3m"
test_data3$search_term <- "adidas"
test_data4$search_term <- "adidas"

test_data_comp <- rbind(test_data1, test_data2, test_data3, test_data4)


# add to df to one
test_data <- rbind(test_data_comp, test_data_nofilter)


# keep only column that are needed
test_data <- test_data %>% select(
  id, created_at, user_id, place, tweet, language,
  replies_count, retweets_count, likes_count, retweet
)
