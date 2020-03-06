#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#################   INSTALL PACKAGES IF REQUIRED   #################
# v_required_packages <- c("rtweet", "ggplot2", "ggthemes", 
#                          "maps", "devtools", "cli", "gh", "knitr")
# 
# for (v_package in v_required_packages) {
#     if(v_package %in% rownames(installed.packages()) == FALSE) 
#     {
#         install.packages(v_package, dependencies=TRUE)
#     }
# }
# 
# devtools::install_github("mkearney/rtweet", build_vignettes = TRUE, force = TRUE)

#################   LOAD THE REQUIRED PACKAGES   #################
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(rtweet))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(cli))
suppressPackageStartupMessages(library(gh))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(rJava))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RJDBC))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(SnowballC))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(reshape2))

httr::set_config(httr::config(http_version = 0))

#################   REQUIRED FUNCTIONS   #################
## DISPLAY A MESSAGE BOX
displayMessage <- function(p_title, p_message) {
    showModal(modalDialog(
        title = p_title,
        p_message,
        easyClose = TRUE,
        footer = NULL # modalButton("Dismiss")
    ))
}

## DISPLAY A ERROR MESSAGE BOX
displayError <- function(p_error_message) {
    displayMessage("ERROR", p_error_message)
}

## FETCH THE EXISTING SUBJECTS FOR WHICH TWEETS EXIST IN THE DATABASE
fetchExistingSubjects <- function() {
    tryCatch(
        {
            v_query <- "SELECT DISTINCT searchString, dateOfEnquiry FROM TB_TWEETS ORDER BY dateOfEnquiry DESC";
            
            conn = dbConnect(jcc, jdbc_path, user=dsn_uid, password=dsn_pwd)
            v_rs <- dbSendQuery(conn, v_query);
            
            v_existing_subjects <- fetch(v_rs, -1) %>%
                filter(!is.na(SEARCHSTRING)) %>%
                filter(!is.na(DATEOFENQUIRY))
            if( nrow(v_existing_subjects) == 0 ) {
                dbDisconnect(conn)
                displayError("Error in Application Setup. No set up data exists. Quitting")
                stop(safeError(NULL))
            }
            
            dbDisconnect(conn)
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            dbDisconnect(conn)
            displayError(paste("Error in Fetching Set Up Data. Quitting", e))
            stop(safeError(e))
        }
    )
    
    return(v_existing_subjects)
}

## FETCH THE SUBJECTS FOR WHICH TWEETS WERE SEARCHED IN THE PAST
fetchExistingSearches <- function() {
    tryCatch(
        {
            v_query <- "SELECT Search_String FROM TB_STATISTICS";
            
            conn = dbConnect(jcc, jdbc_path, user=dsn_uid, password=dsn_pwd)
            v_rs <- dbSendQuery(conn, v_query);
            
            v_existing_searches <- fetch(v_rs, -1) %>%
                filter(!is.na(SEARCH_STRING))
            if( nrow(v_existing_searches) == 0 ) {
                dbDisconnect(conn)
                displayError("Error in Application Setup. No set up data (Existing Searches) exists. Quitting")
                stop(safeError(NULL))
            }
            
            dbDisconnect(conn)
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            dbDisconnect(conn)
            displayError(paste("Error in Fetching Set Up Data. Existing Searches do not exist. Quitting", e))
            stop(safeError(e))
        }
    )
    
    return(v_existing_searches)
}

## READ TWEETS FROM THE DATABASE FOR A PARTICULAR SEARCH CRITERIA
readTweets <- function(p_search_string, p_search_date) {
    if( p_search_date == '' || is.null(p_search_date) ) {
        return(NULL)
    }
    
    if( p_search_string == '' || is.null(p_search_string) ) {
        return(NULL)
    }
    
    v_query = paste("SELECT * FROM TB_TWEETS ",
                    "WHERE searchString = '", p_search_string, "' ",
                    "AND dateOfEnquiry = '", p_search_date, "'",
                    sep = ""
    )
    
    tryCatch(
        {
            conn = dbConnect(jcc, jdbc_path, user=dsn_uid, password=dsn_pwd)
            v_rs = dbSendQuery(conn, v_query);
            
            v_temp = fetch(v_rs, -1);
            if( nrow(v_temp) == 0 ) {
                dbDisconnect(conn)
                displayError("Error in Data Fetch from Database. ZERO ROWS Returned. Quitting")
                stop(safeError(NULL))
            }
            
            v_temp <- lat_lng(v_temp) %>%
                mutate(tweet_text =  TEXT) %>%
                mutate(tweet_language = LANG) %>%
                mutate(tweet_source = SOURCE) %>%
                mutate(tweet_user = NAME) %>%
                mutate(tweet_country = COUNTRY_CODE) %>%
                mutate(tweet_place = PLACE_FULL_NAME) %>%
                mutate(tweet_hashtag = HASHTAGS) %>%
                mutate(tweet_followers_count = FOLLOWERS_COUNT) %>%
                mutate(tweet_friends_count = FRIENDS_COUNT) %>%
                mutate(tweet_urls_expanded_url = STATUS_URL) %>%
                mutate(tweet_place_url = PLACE_URL) %>%
                mutate(tweet_created_at = CREATED_AT) %>%
                mutate(latitude = LAT) %>%
                mutate(longitude = LNG)
            
            dbDisconnect(conn)
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            dbDisconnect(conn)
            displayError(paste("Error in Data Fetch from Database. Quitting", e))
            stop(safeError(e))
        }
    )
    
    return(v_temp)
}

# ## SAVE THE CURRENT SET OF TWEETS TO THE DATABASE
# saveTweetsToDatabase <- function(p_data_frame) {
#     tryCatch(
#         {
#             p_data_frame$tweet_text <- NULL
#             p_data_frame$latitude <- NULL
#             p_data_frame$longitude <- NULL
#             p_data_frame$tweet_language <- NULL
#             p_data_frame$tweet_source <- NULL
#             p_data_frame$tweet_user <- NULL
#             p_data_frame$tweet_country <- NULL
#             p_data_frame$tweet_place <- NULL
#             p_data_frame$tweet_hashtag <- NULL
#             p_data_frame$tweet_followers_count <- NULL
#             p_data_frame$tweet_friends_count <- NULL
#             p_data_frame$tweet_urls_expanded_url <- NULL
#             p_data_frame$tweet_place_url <- NULL
#             p_data_frame$tweet_created_at <- NULL
#             
#             conn = dbConnect(jcc, jdbc_path, user=dsn_uid, password=dsn_pwd)
#             dbWriteTable(conn, "TB_TWEETS", p_data_frame,
#                          overwrite = FALSE,
#                          append =TRUE
#             )
#             
#             dbDisconnect(conn)
#         },
#         error = function(e) {
#             # return a safeError if a parsing error occurs
#             dbDisconnect(conn)
#             displayError(paste("Error Writing to Database. Quitting", e))
#             stop(safeError(e))
#         }
#     )
# }

## EXTRAXT WORDS FROM THE READ TWEETS
extractWordsFromTweets <- function(p_tweet_data_frame) {
    tryCatch(
        {
            # Remove the Emoticons
            p_tweet_data_frame <- gsub("[^\x01-\x7F]", "", p_tweet_data_frame)
            
            # Form the Corpus
            v_corpus <- VCorpus(VectorSource(p_tweet_data_frame))
            
            # Remove URLs
            removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
            toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
            
            v_corpus <- tm_map(v_corpus, removeURL)
            v_corpus <- tm_map(v_corpus, toSpace, "/")
            v_corpus <- tm_map(v_corpus, toSpace, "@")
            v_corpus <- tm_map(v_corpus, toSpace, "\\|")
            v_corpus <- tm_map(v_corpus, toSpace, '"')
            v_corpus <- tm_map(v_corpus, toSpace, "'")
            
            # Create the Term Document Matrix after cleaning the Corpus
            v_tdm <- TermDocumentMatrix(v_corpus, 
                                        control = 
                                            list(removePunctuation = TRUE,
                                                 stopwords = TRUE,
                                                 tolower = TRUE,
                                                 stemming = TRUE,
                                                 removeNumbers = TRUE,
                                                 bounds = list(global = c(1, Inf))
                                            )
            )
            
            # Find the Frequent Words
            v_ft <- findFreqTerms(v_tdm, lowfreq = 1, highfreq = Inf)
            
            # Convert to a Matrix
            v_matrix <- as.matrix(v_tdm[v_ft,])
            
            # Sort the Words in DESCENDING Order and create Data Frame
            v_data <- stack(sort(apply(v_matrix, 1, sum), decreasing = TRUE))
            v_data <- subset(v_data, !(ind == "" || is.null(ind) || is.na(ind)))
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            displayError(paste("Error in Extracting Words from Tweets. This could be becuase the search does not produce any words in English Language. Please Try Again.", e, sep = " "))
            # stop(safeError(e))
        }
    )
    
    return(v_data)
}

## EVALUATE EMOTIONS
determineEmotionCount <- function(p_tidy_data) {
    v_combined <- sort(union(levels(p_tidy_data$word), levels(v_nrc$word)))
    v_text_sentiment2 <- inner_join(mutate(p_tidy_data, word=factor(word, levels=v_combined)),
                                    mutate(v_nrc, word=factor(word, levels=v_combined))
                                   )
    
    v_text_sentiment1 <- v_text_sentiment2 %>%
                            count(word, index = row_number() %/% 80, sentiment) %>%
                            mutate(v_original_n = n) %>%
                            mutate(v_positive_negative = ifelse(sentiment %in% v_positive_emotions,
                                                                'positive', 'negative')) %>%
                            mutate(v_original_sentiment = sentiment) %>%
                            spread(sentiment, n, fill = 0)

    v_text_sentiment <- v_text_sentiment1 %>%
                            mutate(positive = ifelse("positive" %in% colnames(v_text_sentiment1), positive, 0)) %>%
                            mutate(joy = ifelse("joy" %in% colnames(v_text_sentiment1), joy, 0)) %>%
                            mutate(surprise = ifelse("surprise" %in% colnames(v_text_sentiment1), surprise, 0)) %>%
                            mutate(trust = ifelse("trust" %in% colnames(v_text_sentiment1), trust, 0)) %>%
                            mutate(anticipation = ifelse("anticipation" %in% colnames(v_text_sentiment1), anticipation, 0)) %>%
                            mutate(negative = ifelse("negative" %in% colnames(v_text_sentiment1), negative, 0)) %>%
                            mutate(anger = ifelse("anger" %in% colnames(v_text_sentiment1), anger, 0)) %>%
                            mutate(disgust = ifelse("disgust" %in% colnames(v_text_sentiment1), disgust, 0)) %>%
                            mutate(sadness = ifelse("sadness" %in% colnames(v_text_sentiment1), sadness, 0)) %>%
                            mutate(fear = ifelse("fear" %in% colnames(v_text_sentiment1), fear, 0)) %>%
                            mutate(sentiment = (positive+joy+surprise+trust+anticipation) - (negative+anger+disgust+sadness+fear)) %>%
                            ungroup()

    return(v_text_sentiment)
}

## SAVE STATISTICS TO DATABASE
saveStatisics <- function() {
    tryCatch(
        {
            v_statement <- paste("INSERT INTO TB_STATISTICS (",
                                 "Date_of_Search,",
                                 "Search_String,",
                                 "No_requested,",
                                 "No_of_tweets_fetched,",
                                 "No_of_tweets_analysed,",
                                 "No_of_hashtag_found,",
                                 "No_of_countries,",
                                 "No_of_places,",
                                 "No_of_languages,",
                                 "No_of_devices,",
                                 "No_of_users,",
                                 "Total_words,",
                                 "Unique_words,",
                                 "Prevalent_emotion)",
                             "VALUES (",
                                 paste("'", v_selected_date, "',", sep = ""),
                                 paste("'", v_selected_subject, "',", sep = ""),
                                 paste(v_number_requested, ",", sep = ""),
                                 paste(v_number_fetched, ",", sep = ""),
                                 paste(v_number_analysed, ",", sep = ""),
                                 paste(v_number_hastag_found, ",", sep = ""),
                                 paste(v_number_countries, ",", sep = ""),
                                 paste(v_number_places, ",", sep = ""),
                                 paste(v_number_languages, ",", sep = ""),
                                 paste(v_number_devices, ",", sep = ""),
                                 paste(v_number_users, ",", sep = ""),
                                 paste(v_number_words, ",", sep = ""),
                                 paste(v_number_unique_words, ",", sep = ""),
                                 paste("'", v_prevalent_emotion, "'", sep = ""),
                             ")", sep = " "
            )

            conn = dbConnect(jcc, jdbc_path, user=dsn_uid, password=dsn_pwd)
            if( !dbSendUpdate(conn, v_statement) ) {
                displayError("Error Writing to Database.")
            } else {
                dbCommit(conn)
            }
            
            dbDisconnect(conn)
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            dbDisconnect(conn)
            # displayError(paste("Error Writing to Statistics.", e, sep = " "))
            # stop(safeError(e))
        }
    )
}

#################   READ THE SYSTEM PARAMETERS   #################
v_system_parameters <- read.csv(file = "SystemParameters.csv", header = TRUE)
attach(v_system_parameters)

#################   SET UP THE TWITTER ACCOUNT   #################
api_key <- as.character(v_system_parameters[(which(Parameter == "api_key")),]$Value)
api_secret_key <- as.character(v_system_parameters[(which(Parameter == "api_secret_key")),]$Value)
access_token <- as.character(v_system_parameters[(which(Parameter == "access_token")),]$Value)
access_token_secret <- as.character(v_system_parameters[(which(Parameter == "access_token_secret")),]$Value)

## Authenticate via Web Browser
token <- create_token(
    app = "ParthaEmotions",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret)

#################   SET UP THE DATABASE   #################
## Values for database connection
dsn_driver = as.character(v_system_parameters[(which(Parameter == "dsn_driver")),]$Value)
dsn_database = as.character(v_system_parameters[(which(Parameter == "dsn_database")),]$Value)
dsn_hostname = as.character(v_system_parameters[(which(Parameter == "dsn_hostname")),]$Value)
dsn_port = as.character(v_system_parameters[(which(Parameter == "dsn_port")),]$Value)
dsn_protocol = as.character(v_system_parameters[(which(Parameter == "dsn_protocol")),]$Value)
dsn_uid = as.character(v_system_parameters[(which(Parameter == "DBUserID")),]$Value)
dsn_pwd = as.character(v_system_parameters[(which(Parameter == "DBPassword")),]$Value)

## Connect to the Database
jcc = JDBC(dsn_driver, "Drivers/db2jcc4.jar", identifier.quote="`");
jdbc_path = paste("jdbc:db2://",  dsn_hostname, ":", dsn_port, "/", dsn_database, sep="");

#################   SET UP THE GOOGLE MAPS   #################
google_maps_api_key = as.character(v_system_parameters[(which(Parameter == "google_maps_api_key")),]$Value)

#################   READ THE LEXICON   #################
v_nrc <- readRDS("./nrc")
v_positive_emotions <- c("positive","joy","anticipation","surprise", "trust")
v_negative_emotions <- c("negative","anger","disgust","fear", "sadness")
v_positive_sentiment <- v_nrc %>%
                            filter(sentiment %in% v_positive_emotions)

#################   SET UP INITIAL DATA TO DISPLAY   #################
# Fetch existing searches
v_existing_searches <- fetchExistingSearches()

# Fetch the Initial Set of Tweets deom Database to Display
v_existing_subjects <- fetchExistingSubjects()

v_selected_subject <- v_existing_subjects[1,]$SEARCHSTRING
v_selected_date <- v_existing_subjects[1,]$DATEOFENQUIRY

v_tweets_to_display <- readTweets(v_selected_subject, v_selected_date)

v_number_requested <- as.integer(0)
v_number_fetched <- as.integer(0)
v_number_analysed <- as.integer(0)
v_number_hastag_found <- as.integer(0)
v_number_countries <- as.integer(0)
v_number_places <- as.integer(0)
v_number_languages <- as.integer(0)
v_number_devices <- as.integer(0)
v_number_users <- as.integer(0)
v_number_words <- as.integer(0)
v_number_unique_words <- as.integer(0)
v_prevalent_emotion <- ""

# Set up initial Emotion Analysis
tryCatch(
    {
        v_words_in_tweets <- extractWordsFromTweets(
            (v_tweets_to_display %>%
                 filter(tweet_language == 'en'))$tweet_text
        ) %>%
            rename(word = ind) %>%
            rename(n = values)
    },
    error = function(e) {
        # return a safeError if a parsing error occurs
        displayError(paste("Error Setting up Chidiya. Quitting", e, sep = " "))
        stop(safeError(e))
    }
)

v_combined <- sort(union(levels(v_words_in_tweets$word), levels(v_positive_sentiment$word)))
v_positive_sentiment_words <- semi_join(mutate(v_words_in_tweets, word=factor(word, levels=v_combined)),
                                        mutate(v_positive_sentiment, word=factor(word, levels=v_combined))) %>%
                                count(word, sort = TRUE)

#################   SET UP UI FOR THE APPLICATION   #################
ui <- fluidPage(theme = shinytheme("united"),
                tags$head(
                    tags$style(
                        HTML(".shiny-notification {
                             position:fixed;
                             top: calc(50%);
                             left: calc(50%);
                             font-family:Verdana;
                             fontSize:xx-large;
                             }
                             "
                        )
                    )
                ),
                
                # Application title
                fluidRow(
                    column(width = 3,
                           img(src="ParthaInPetra.jpg", width=200, height=112)
                    ),
                    column(width = 6,
                           h1("Chidiya", style="color:blue; text-align: center;"),
                           h3("Tweet Analyser", style="color:darkred; text-align: center;")
                    ),
                    column(width = 3,
                           h4("...", style="color:blue; text-align: right;"),
                           h4("Developed by: Partha Majumdar", style="color:blue; text-align: right;"),
                           h5("Riyadh (Saudi Arabia), 29-February-2020", style="color:black; text-align: right;")
                    )
                ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "search_option_panel", type = "tabs",
                        tabPanel("Search String",
                                 h4("Enter Search String"),
                                 h5("Enter several subjects by separating with comma"),
                                 h5("To search for hashtag, enter Search String starting with #. For example: #Climate"),
                                 textInput(inputId = "v_search_string", 
                                           label = "",
                                           value = "",
                                           placeholder = "Enter Subject(s) to Search"
                                 ),
                                 
                                 sliderInput(inputId = "v_number_of_tweets",
                                             label = h4("Number of Tweets to Fetch:"),
                                             value = 750, 
                                             min = 500, 
                                             max = 5000
                                 ),
                                 
                                 actionButton(inputId = "v_fetch", label = "Fetch")
                        ),
                        tabPanel("Location",
                                 h4("Enter Location"),
                                 h5("Enter only ONE Location name"),
                                 textInput(inputId = "v_search_location", 
                                           label = "",
                                           value = "",
                                           placeholder = "Enter Location to Search"
                                 ),
                                 
                                 sliderInput(inputId = "v_number_of_tweets_location",
                                             label = h4("Number of Tweets to Fetch:"),
                                             value = 750, 
                                             min = 500, 
                                             max = 5000
                                 ),
                                 
                                 actionButton(inputId = "v_fetch_location", label = "Fetch")
                        )
            ),
            
            br(),
            br(),
            
            wellPanel(
                h5("It can take time to fetch new Tweets. In case, Chidiya throws an error while fetching new tweets, wait for about 15 minutes before trying to fetch tweets once again. There is a restriction on how many tweets can be fetched in a given interval.",
                   style="color:red; text-align: left;")
            ),
            
            wellPanel(
                h4("Past Searches", style="color:black; text-align: center;"),
                plotOutput(outputId = "existingSearchesWordCloud")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "main_panel", type = "tabs",
                        tabPanel("Raw Data",
                                 dataTableOutput(outputId = "tweets"),
                                 wellPanel(
                                     plotOutput(outputId = "tweetTimeSeries")
                                 )
                        ),
                        tabPanel("Analysis",
                                 tabsetPanel(id = "analysis_panel", type = "tabs",
                                             tabPanel("World Map",
                                                      fluidRow(
                                                          column(width = 12,
                                                                 h3("Where are the people tweeting from",
                                                                    style="color:blue; text-align: center;"),
                                                                 # leafletOutput(outputId = "WorldMap", width = "100%", height = "500px")
                                                                 plotOutput(outputId = "WorldMap")
                                                          )
                                                      )
                                             ),
                                             # tabPanel("Country Maps",
                                             #          fluidRow(
                                             #              column(width = 6,
                                             #                     h3("India"),
                                             #                     plotOutput(outputId = "IndiaMap")
                                             #              ),
                                             #              column(width = 6,
                                             #                     h3("USA"),
                                             #                     plotOutput(outputId = "USAMap")
                                             #              )
                                             #          )
                                             # ),
                                             tabPanel("Statistics",
                                                      fluidRow(
                                                          column(width = 3,
                                                                 fluidRow(
                                                                     column(width = 12,
                                                                            wellPanel(
                                                                                h5("Number of Tweets Analysed", style="color:black; text-align: center;"),
                                                                                h3(textOutput(outputId = "numberOfTweets"), style="color:blue; text-align: center;"),
                                                                                h6("Only Tweets with complete innformation are considered for analysis.", style="color:red; text-align: center;"),
                                                                            )
                                                                     ),
                                                                     column(width = 12,
                                                                            wellPanel(
                                                                                h5("Tweets from", style="color:black; text-align: center;"),
                                                                                h3(textOutput(outputId = "countryCount"), style="color:blue; text-align: center;"),
                                                                                h4("Country(ies)", style="color:blue; text-align: center;"),
                                                                                h6("See Map for details", style="color:red; text-align: center;"),
                                                                                dataTableOutput(outputId = "placeCount")
                                                                            )
                                                                     )
                                                                 )
                                                          ),
                                                          column(width = 5,
                                                                 fluidRow(
                                                                     column(width = 12,
                                                                            wellPanel(
                                                                                plotOutput(outputId = "languageCount")
                                                                            )
                                                                     ),
                                                                     column(width = 12,
                                                                            wellPanel(
                                                                                plotOutput(outputId = "sourceCount")
                                                                            )
                                                                     ) #,
                                                                 )
                                                          ),
                                                          column(width = 4,
                                                                 fluidRow(
                                                                     column(width = 12,
                                                                            wellPanel(
                                                                                h5("Tweets from", style="color:black; text-align: center;"),
                                                                                h3(textOutput(outputId = "userCount"), style="color:blue; text-align: center;"),
                                                                                h4("User(s)", style="color:blue; text-align: center;"),
                                                                                dataTableOutput(outputId = "mostTweeter")
                                                                            )
                                                                     ),
                                                                     column(width = 12,
                                                                            # wellPanel(
                                                                                h5("Hashtags Used", style="color:black; text-align: center;"),
                                                                                plotOutput(outputId = "hashTagWordCloud")
                                                                            # )
                                                                     )
                                                                 )
                                                          )
                                                      )
                                             ),
                                             tabPanel("Emotion Analysis",
                                                      h3("Only Tweets in English Language considered for analysis", style="color:blue; text-align: center;"),
                                                      tabsetPanel(id = "emotion_analysis_panel", type = "tabs",
                                                                  tabPanel("Summary",
                                                                           fluidRow(
                                                                               column(width = 2,
                                                                                      fluidRow(
                                                                                          column(width = 12,
                                                                                                 wellPanel(
                                                                                                     h5("Total Word Analysed", style="color:black; text-align: center;"),
                                                                                                     h3(textOutput(outputId = "totalWordCount"), style="color:blue; text-align: center;")
                                                                                                 )
                                                                                          ),
                                                                                          column(width = 12,
                                                                                                 wellPanel(
                                                                                                     h5("Unique Words Found", style="color:black; text-align: center;"),
                                                                                                     h3(textOutput(outputId = "uniqueWordCount"), style="color:blue; text-align: center;")
                                                                                                 )
                                                                                          ),
                                                                                          column(width = 12,
                                                                                                 wellPanel(
                                                                                                     h5("Words with Positive Emotions Found", style="color:black; text-align: center;"),
                                                                                                     h3(textOutput(outputId = "positiveSentimentWordCount"), style="color:blue; text-align: center;")
                                                                                                 )
                                                                                          )
                                                                                      )
                                                                               ),
                                                                               column(width = 3,
                                                                                      wellPanel(
                                                                                          h5("Unique Words in Tweets", style="color:black; text-align: center;"),
                                                                                          dataTableOutput(outputId = "uniqueWords")
                                                                                      )
                                                                               ),
                                                                               column(width = 7,
                                                                                      fluidRow(
                                                                                          column(width = 12,
                                                                                                 wellPanel(
                                                                                                     h5("Most Prevalent Emotion in Tweets", style="color:black; text-align: center;"),
                                                                                                     h2(textOutput(outputId = "mostPrevalentEmotion"), style="color:red; text-align: center;")
                                                                                                 )
                                                                                          )
                                                                                      ),
                                                                                      fluidRow(
                                                                                          column(width = 4,
                                                                                                 plotOutput(outputId = "boxPlot")
                                                                                          ),
                                                                                          column(width = 8,
                                                                                                 h5("Words Used", style="color:black; text-align: center;"),
                                                                                                 plotOutput(outputId = "wordWordCloud")
                                                                                          )
                                                                                      )
                                                                               )
                                                                           )
                                                                  ),
                                                                  tabPanel(
                                                                      title = "Sentiment Analysis",
                                                                      fluidRow(
                                                                          column(width = 6,
                                                                                 h3("This graph indicates the prevalent emotions in the text."),
                                                                                 plotOutput(outputId = "sentiments")
                                                                          ),
                                                                          column(width = 6,
                                                                                 h3("This graph indicates the volume of positive and negative sentiments used through different words used in the Tweets. The sentiments are displayed from the First to the Last Tweet analysed."),
                                                                                 h4("Higher volume of GREEN colour in the graph indicates positivity in the text.", style="color:darkgreen; text-align: left;"),
                                                                                 h4("Higher volume of RED indicates negativity in the text.", style="color:red; text-align: left;"),
                                                                                 plotOutput(outputId = "positiveNegativeSentiments")
                                                                          )
                                                                      )
                                                                  ),
                                                                  tabPanel("Word Analysis",
                                                                           fluidRow(
                                                                               column(width = 6,
                                                                                      sliderInput(inputId = "v_top_n_word_contribution", 
                                                                                                  h4("Set Top N Words to Include"), 
                                                                                                  min = 4, max = 200, 
                                                                                                  value = 10, step = 2, ticks = TRUE),
                                                                                      h4("This section highlights the words which have contributed to the various emotions extracted in the text."),
                                                                                      h4("The emotions extracted is as per the lexicon used. The lexicon assigns a emotion to the words as per generic computation preset in the knowledge base."),
                                                                                      h4("While evaluating this graph, specific context needs to be kept in mind before drawing conclusions."),
                                                                                      plotOutput(outputId = "wordContributionToSentiments")
                                                                               ),
                                                                               column(width = 6,
                                                                                      sliderInput(inputId = "v_top_n_sentiment_score", 
                                                                                                  h4("Set Top N Words to Include"), 
                                                                                                  min = 4, max = 200, 
                                                                                                  value = 10, step = 2, ticks = TRUE),
                                                                                      h4("This graph display the different words that have contributed to the different emotions evaluated in the text."),
                                                                                      h3("The same word can contribute to different emotions based on the context it is used in."),
                                                                                      plotOutput(outputId = "sentimentScore")
                                                                               )
                                                                           )
                                                                  )
                                                      )
                                             )
                                 )
                        ),
                        tabPanel("Known Errors",
                                 h3("1. When the 'Fetch Tweets' Operations fetched no Tweets, then the Display in the Analysis is all incorrect. 'Chidiya' does not become inoperational. An Error Message is displayed. And 'Chidiya' allows for the next fetch operation to be conducted.", style="color:red; text-align: left;"),
                                 h3("2. If the 'Fetch Tweets' Operations is used to too frequently for large numberof Tweets, 'Chidiya' can misbehave. This is because of limitation of number of Tweets that can be pulled from Twitter. The solution is to invoke 'Chidiya' after a gap of 15-30 minutes and normal service is restored.", style="color:blue; text-align: left;"),
                                 h3("3. Progress Bar not synchronised with actual progress of task.", style="color:red; text-align: left;"),
                                 h3("4. If there is a problem with the Internet connection, 'Chidiya' cannot connect to the Twitter API. 'Chidiya' detects this. However, 'Chidiya' cannot presently recover from this error. To recover from this error, Internet Connection has to be restored and 'Chidiya' has to be revoked once again.", style="color:blue; text-align: left;")
                        )
                    )
        )
    )
)

#################   SET UP SERVER FOR THE APPLICATION   #################
server <- function(input, output, session) {
    observeEvent(input$v_fetch_location,
                 {
                     if( !(input$v_search_location == "" || is.null(input$v_search_location)) ) {
                         tryCatch(
                             {
                                 withProgress(message = 'Fetching Tweets for Location',
                                              detail = 'This may take a while...', value = 100, {
                                                  v_tweets_to_display <<- v_tweets_for_location(input$v_search_location, input$v_number_of_tweets_location)
                                                  
                                                  extractWords()
                                              })
                                 
                                 refreshDisplay()

                                 # Save Statistics
                                 saveStatisics()
                             },
                             error = function(e) {
                                 # return a safeError if a parsing error occurs
                                 displayError(paste("Error Fetching Tweets. This could be becuase the search does not fetch any Tweet OR the fetched Tweets does not contain any Words in English Language OR the search was for too few number of Tweets. The Graphs and other Outputs will not be updated due to this. Try fetching larger number of Tweets OR using Search Terms in English Language.", e, sep = " "))
                                 # stop(safeError(e))
                             }
                         )
                         
                         # Reset Display to default TAB
                         updateTabsetPanel(session, "main_panel",
                                           selected = "Raw Data"
                         )
                     }
                 }
    )
    
    observeEvent(input$v_fetch,
                 {
                     if( !(input$v_search_string == "" || is.null(input$v_search_string)) ) {
                         tryCatch(
                             {
                                 withProgress(message = 'Fetching Tweets for Search String',
                                              detail = 'This may take a while...', value = 100, {
                                                  v_tweets_to_display <<- v_tweets(input$v_search_string, input$v_number_of_tweets)
                                                  
                                                  extractWords()
                                              })
                                 
                                 refreshDisplay()

                                 # Save Statistics
                                 saveStatisics()
                             },
                             error = function(e) {
                                 # return a safeError if a parsing error occurs
                                 displayError(paste("Error Fetching Tweets. This could be becuase the search does not fetch any Tweet OR the fetched Tweets does not contain any Words in English Language OR the search was for too few number of Tweets. The Graphs and other Outputs will not be updated due to this. Try fetching larger number of Tweets OR using Search Terms in English Language.", e, sep = " "))
                                 # stop(safeError(e))
                             }
                         )
                         
                         # Reset Display to default TAB
                         updateTabsetPanel(session, "main_panel",
                                           selected = "Raw Data"
                         )
                     }
                 }
    )

    extractWords <- function() {
        tryCatch(
            {
                v_words_in_tweets <<- extractWordsFromTweets(
                    (v_tweets_to_display %>%
                         filter(tweet_language == 'en'))$tweet_text
                ) %>%
                    rename(word = ind) %>%
                    rename(n = values)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                displayError(paste("Error Extracting Words from Tweets. This could be becuase the search does not fetch any Words in English Language OR the search for too few number of Tweets. The Graphs and other Outputs will not be updated due to this. Try fetching larger number of Tweets OR using Search Terms in English Language.", e, sep = " "))
                # stop(safeError(e))
            }
        )
        
        v_combined <<- sort(union(levels(v_words_in_tweets$word), levels(v_positive_sentiment$word)))
        v_positive_sentiment_words <<- semi_join(mutate(v_words_in_tweets, word=factor(word, levels=v_combined)),
                                                mutate(v_positive_sentiment, word=factor(word, levels=v_combined))) %>%
                                            count(word, sort = TRUE)
    }
    
    refreshDisplay <- function() {
        # Dummy
        determineLanguageCount(FALSE)
        determineSourceCount(FALSE)
        determineUserCount()
        determineCountryCount()
        determinePlaceCount(FALSE)
        showUniqueWords(FALSE)
        showMostPrevalentEmotion()
        
        # Render on Fetch
        output$tweets <- renderDataTable({showTweets()})
        output$tweetTimeSeries <- renderPlot({showTweetTimeSeries()})
        output$WorldMap <- renderPlot({showWorldMap()})
        # output$WorldMap <- renderLeaflet({showWorldMap()})
        # output$IndiaMap <- renderPlot({showIndiaMap()})
        # output$USAMap <- renderPlot({showUSAMap()})
        output$numberOfTweets <- renderText({nrow(v_tweets_to_display)})
        output$languageCount <- renderPlot({determineLanguageCount()})
        output$sourceCount <- renderPlot({determineSourceCount()})
        output$userCount <- renderText({determineUserCount()})
        output$countryCount <- renderText({determineCountryCount()})
        output$placeCount <- renderDataTable({determinePlaceCount()})
        output$mostTweeter <- renderDataTable({determineMostTweeter()})
        output$hashTagWordCloud <- renderPlot({showHashTagWordCloud()})
        
        output$totalWordCount <- renderText({return(sum(v_words_in_tweets$n))})
        output$uniqueWordCount <- renderText({return(nrow(v_words_in_tweets))})
        output$positiveSentimentWordCount <- renderText({return(nrow(v_positive_sentiment_words))})
        output$uniqueWords <- renderDataTable({showUniqueWords()})
        output$boxPlot <- renderPlot({showBoxPlot()})
        output$wordWordCloud <- renderPlot({showWordWordCloud()})
        output$wordContributionToSentiments <- renderPlot({showWordContributionToSentiments()})
        output$sentiments <- renderPlot({showSentiments()})
        output$positiveNegativeSentiments <- renderPlot({showPositiveNegativeSentiments()})
        output$mostPrevalentEmotion <- renderText({showMostPrevalentEmotion()})
        output$sentimentScore <- renderPlot({determineSentimentScore()})
    }
    
    v_tweets <- function(p_search_string, p_number_of_tweets) {
        tryCatch(
            {
                v_temp <- search_tweets(
                    p_search_string, 
                    `-filter` = "replies",
                    n = p_number_of_tweets,
                    include_rts = FALSE,
                    retryonratelimit = FALSE #,
                )
                
                v_temp <- gatherTweetData(v_temp, p_search_string, p_number_of_tweets)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                displayError(paste("Error Fetching Tweets.", e))
                # stop(safeError(e))
            }
        )

        return(v_temp)
    }
    
    v_tweets_for_location <- function(p_search_location, p_number_of_tweets) {
        tryCatch(
            {
                v_temp <- search_tweets(
                    geocode = lookup_coords(p_search_location, apikey = google_maps_api_key), 
                    `-filter` = "replies",
                    n = p_number_of_tweets,
                    include_rts = FALSE,
                    retryonratelimit = FALSE #,
                )
                
                v_temp <- gatherTweetData(v_temp, p_search_location, p_number_of_tweets)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                displayError(paste("Error Fetching Tweets.", e))
                # stop(safeError(e))
            }
        )
        
        return(v_temp)
    }
    
    gatherTweetData <- function(p_tweets, p_search_string, p_number_of_tweets) {
        v_number_fetched <<- nrow(p_tweets)
        
        v_temp <- lat_lng(p_tweets) %>%
            filter(lat != "NA") %>%
            filter(lng != "NA") %>%
            mutate(tweet_text = text) %>%
            mutate(tweet_language = lang) %>%
            mutate(tweet_source = source) %>%
            mutate(tweet_user = name) %>%
            mutate(tweet_country = country_code) %>%
            mutate(tweet_place = place_full_name) %>%
            mutate(tweet_hashtag = hashtags) %>%
            mutate(tweet_followers_count = followers_count) %>%
            mutate(tweet_friends_count = friends_count) %>%
            mutate(tweet_urls_expanded_url = status_url) %>%
            mutate(tweet_place_url = place_url) %>%
            mutate(tweet_created_at = created_at) %>%
            mutate(latitude = lat) %>%
            mutate(longitude = lng)
        
        v_number_analysed <<- nrow(v_temp)
        
        v_dataframe_to_write <- v_temp
        v_dataframe_to_write$SEARCHSTRING <- p_search_string
        v_dataframe_to_write$DATEOFENQUIRY <- format(Sys.Date(), "%Y-%m-%d")
        
        # saveTweetsToDatabase(v_dataframe_to_write)
        
        v_selected_subject <<- p_search_string
        v_number_requested <<- p_number_of_tweets
        v_selected_date <<- format(Sys.Date(), "%Y-%m-%d")
        
        write_as_csv(
            v_dataframe_to_write,
            file_name = "last_fetched.csv",
            prepend_ids = TRUE,
            na = "",
            fileEncoding = "UTF-8"
        )
        
        return(v_temp)
    }
    
    output$totalWordCount <- renderText({
        return(sum(v_words_in_tweets$n))
    })
    
    output$uniqueWordCount <- renderText({
        return(nrow(v_words_in_tweets))
    })
    
    output$positiveSentimentWordCount <- renderText({
        return(nrow(v_positive_sentiment_words))
    })
    
    output$uniqueWords <- renderDataTable({
        showUniqueWords()
    })
    
    output$boxPlot <- renderPlot({
        showBoxPlot()
    })

    output$tweets <- renderDataTable({
        showTweets()
    })
    
    output$WorldMap <- renderPlot({
        showWorldMap()
    })

    # output$WorldMap <- renderLeaflet({
    #     showWorldMap()
    # })
    # 
    # output$IndiaMap <- renderPlot({
    #     showIndiaMap()
    # })
    # 
    # output$USAMap <- renderPlot({
    #     showUSAMap()
    # })
    
    output$numberOfTweets <- renderText({
        nrow(v_tweets_to_display)
    })
    
    output$languageCount <- renderPlot({
        determineLanguageCount()
    })
    
    output$sourceCount <- renderPlot({
        determineSourceCount()
    })
    
    output$userCount <- renderText({
        determineUserCount()
    })
    
    output$mostTweeter <- renderDataTable({
        determineMostTweeter()
    })
    
    output$countryCount <- renderText({
        determineCountryCount()
    })
    
    output$placeCount <- renderDataTable({
        determinePlaceCount()
    })
    
    output$hashTagWordCloud <- renderPlot({
        showHashTagWordCloud()
    })
    
    output$wordWordCloud <- renderPlot({
        showWordWordCloud()
    })
    
    showBoxPlot <- function() {
        boxplot(v_words_in_tweets$n,
                main = "Word Distribution",
                xlab = NULL,
                ylab = "Frequency",
                col = "orange",
                border = "brown",
                horizontal = FALSE,
                notch = FALSE
        )
    }

    determineLanguageCount <- function(flag = TRUE) {
        v_temp <- v_tweets_to_display %>%
            ungroup() %>%
            mutate(fac_language = as.factor(tweet_language)) %>%
            group_by(fac_language) %>%
            count(fac_language, sort = TRUE) %>%
            mutate(Language = reorder(fac_language, n))

        v_data <- v_temp$n
        v_labels <- paste(paste(v_temp$Language, "\n",
                                round(v_data/sum(v_data)*100, 2), sep=" "), 
                          "%", sep = "")
 
        v_number_languages <<- nrow(v_temp)
        
        if(flag) {
            pie(v_data, labels = v_labels, main = "Tweet Languages")
        }
    }
    
    determineSourceCount <- function(flag = TRUE) {
        v_temp <- v_tweets_to_display %>%
            ungroup() %>%
            mutate(fac_source = as.factor(tweet_source)) %>%
            group_by(fac_source) %>%
            count(fac_source, sort = TRUE) %>%
            mutate(Source = reorder(fac_source, n))
        
        v_data <- v_temp$n
        v_labels <- paste(paste(v_temp$Source, "\n",
                                round(v_data/sum(v_data)*100, 2), sep=" "), 
                          "%", sep = "")

        v_number_devices <<- nrow(v_temp)
        
        if(flag)  {
            pie(v_data, labels = v_labels, main = "Tweet Source")
        }
    }
    
    determineUserCount <- function() {
        v_temp <- v_tweets_to_display %>%
            ungroup() %>%
            mutate(fac_user = as.factor(tweet_user)) %>%
            group_by(fac_user) %>%
            count(fac_user, sort = TRUE)

        v_number_users <<- nrow(v_temp)
        
        return(nrow(v_temp))        
    }
    
    determineCountryCount <- function() {
        v_temp <- v_tweets_to_display %>%
            ungroup() %>%
            mutate(fac_country = as.factor(tweet_country)) %>%
            group_by(fac_country) %>%
            count(fac_country, sort = TRUE)

        v_number_countries <<- nrow(v_temp)
        
        return(nrow(v_temp))        
    }
    
    determinePlaceCount <- function(flag = TRUE) {
        v_temp <- v_tweets_to_display %>%
            ungroup() %>%
            mutate(fac_place = as.factor(tweet_place)) %>%
            group_by(fac_place) %>%
            count(fac_place, sort = TRUE) %>%
            mutate(Place = reorder(fac_place, n))
        
        # Determine Place URL
        v_temp$PlaceURL <- ""
        for (i in 1:nrow(v_temp)) {
            v_temp[i,]$PlaceURL <- head(subset(v_tweets_to_display, tweet_place == v_temp[i,]$fac_place)$tweet_place_url, 1)
        }

        v_temp <- v_temp %>%
                        mutate(place_with_link = paste("<a href='", 
                                                       PlaceURL, 
                                                       "', target='_blank'>", 
                                                       Place, 
                                                       "</a>", 
                                                       sep = "")
                                )

        v_number_places <<- nrow(v_temp)
        
        if(flag) {
            DT::datatable(
                { v_temp[,c("place_with_link", "n")] },
                colnames = c("Place", "#"),
                escape = FALSE,
                rownames = FALSE,
                options = list(
                    fixedColumns = TRUE,
                    autoWidth = FALSE,
                    ordering = TRUE,
                    pageLength = 6,
                    dom = 'tip'
                ))
        }
    }
    
    determineMostTweeter <- function() {
        v_temp <- v_tweets_to_display %>%
            ungroup() %>%
            mutate(fac_user = as.factor(tweet_user)) %>%
            group_by(fac_user) %>%
            count(fac_user, sort = TRUE) %>%
            mutate(Tweeter = reorder(fac_user, n))
        
        # Determine number of Followers and Friends
        v_temp$Followers <- as.integer(0)
        v_temp$Friends <- as.integer(0)
        for (i in 1:nrow(v_temp)) {
            v_temp[i,]$Followers <- as.integer(max(subset(v_tweets_to_display, tweet_user == v_temp[i,]$fac_user)$tweet_followers_count))
            v_temp[i,]$Friends <- as.integer(max(subset(v_tweets_to_display, tweet_user == v_temp[i,]$fac_user)$tweet_friends_count))
        }

        DT::datatable(
            { v_temp[,c("Tweeter", "n", "Followers", "Friends")] },
            colnames = c("Tweeter", "#", "Followers", "Friends"),
            escape = TRUE,
            rownames = FALSE,
            options = list(
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                pageLength = 3,
                dom = 'tip'
            ))
    }
    
    showHashTagWordCloud <- function() {
        tryCatch(
            {
                v_corpus <- Corpus(VectorSource(v_tweets_to_display$tweet_hashtag))
                
                # Create the Term Document Matrix after cleaning the Corpus
                v_tdm <- TermDocumentMatrix(v_corpus, 
                                            control = 
                                                list(removePunctuation = TRUE,
                                                     stopwords = TRUE,
                                                     tolower = TRUE,
                                                     stemming = TRUE,
                                                     removeNumbers = TRUE,
                                                     bounds = list(global = c(1, Inf))
                                                )
                )
                
                # Find the Frequent Words
                v_ft <- findFreqTerms(v_tdm, lowfreq = 1, highfreq = Inf)
                
                # Convert to a Matrix
                v_matrix <- as.matrix(v_tdm[v_ft,])
                
                # Sort the Words in DESCENDING Order and create Data Frame
                v_data <- stack(sort(apply(v_matrix, 1, sum), decreasing = TRUE))
                
                v_number_hastag_found <<- nrow(v_data)
                
                withProgress(message = 'Generating Word Cloud',
                             detail = 'This may take a while...', value = 0, {
                                 wordcloud(words = v_data$ind, freq = v_data$values, 
                                           min.freq = 1,
                                           max.words = 100,
                                           random.order=FALSE, 
                                           colors=brewer.pal(8, "Dark2")
                                 )
                             })
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                displayError(paste("Error creating HashTag Word Cloud", e))
                # stop(safeError(e))
            }
        )
    }
    
    output$existingSearchesWordCloud <- renderPlot({
        tryCatch(
            {
                v_corpus <- Corpus(VectorSource(v_existing_searches$SEARCH_STRING))
                
                # Create the Term Document Matrix after cleaning the Corpus
                v_tdm <- TermDocumentMatrix(v_corpus, 
                                            control = 
                                                list(removePunctuation = TRUE,
                                                     stopwords = TRUE,
                                                     tolower = TRUE,
                                                     stemming = TRUE,
                                                     removeNumbers = TRUE,
                                                     bounds = list(global = c(1, Inf))
                                                )
                )
                
                # Find the Frequent Words
                v_ft <- findFreqTerms(v_tdm, lowfreq = 1, highfreq = Inf)
                
                # Convert to a Matrix
                v_matrix <- as.matrix(v_tdm[v_ft,])
                
                # Sort the Words in DESCENDING Order and create Data Frame
                v_data <- stack(sort(apply(v_matrix, 1, sum), decreasing = TRUE))
                
                v_number_hastag_found <<- nrow(v_data)
                
                withProgress(message = 'Generating Word Cloud',
                             detail = 'This may take a while...', value = 0, {
                                 wordcloud(words = v_data$ind, freq = v_data$values, 
                                           min.freq = 1,
                                           max.words = 100,
                                           random.order=FALSE, 
                                           colors=brewer.pal(8, "Dark2")
                                 )
                             })
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                displayError(paste("Error creating Existing Searches Word Cloud", e))
                # stop(safeError(e))
            }
        )
    })

    showWordWordCloud <- function() {
        withProgress(message = 'Generating Word Cloud',
                     detail = 'This may take a while...', value = 0, {
                         wordcloud(words = v_words_in_tweets$word, freq = v_words_in_tweets$n, 
                                   min.freq = 1,
                                   max.words = 100,
                                   random.order=FALSE, 
                                   colors=brewer.pal(8, "Dark2")
                         )
                     })
    }
    
    showTweets <- function() {
        withProgress(message = 'Tabulating Tweets',
                     detail = 'This may take a while...', value = 100, {
                         v_temp <- v_tweets_to_display %>%
                                     mutate(tweet_with_link = paste("<a href='", 
                                                                    tweet_urls_expanded_url, 
                                                                    "', target='_blank'>", 
                                                                    tweet_text, 
                                                                    "</a>", 
                                                                    sep = "")
                                            )
                         DT::datatable(
                             { select(v_temp, tweet_with_link) },
                             colnames = c('Tweet Text (Click on the Tweet to see the Original Tweet)'),
                             caption = htmltools::tags$caption(
                                 style = 'caption-side: bottom; text-align: center;',
                                 'Table 1: ', 
                                 htmltools::em('Fetched Tweets')
                             ),
                             extensions = 'Buttons',
                             escape = FALSE,
                             options = list(
                                 fixedColumns = TRUE,
                                 autoWidth = FALSE,
                                 ordering = TRUE,
                                 pageLength = 10,
                                 dom = 'Bftsp',
                                 buttons = c('copy', 'csv', 'excel')
                             ))
                     })
    }
    
    output$tweetTimeSeries <- renderPlot({
        showTweetTimeSeries()
    })
    
    showTweetTimeSeries <- function() {
        # Save current locale
        loc <- Sys.getlocale("LC_TIME")
        
        # Set Locale
        Sys.setlocale("LC_TIME", "C") 
        
        # Convert to POSIXct
        v_tweets_to_display$tweet_created_at <- as.POSIXct(v_tweets_to_display$tweet_created_at, '%Y-%m-%d %H:%M:%S', tz = Sys.timezone())
        
        # Then set back to the old locale
        Sys.setlocale("LC_TIME", loc) 

        ## Plot time series of tweets frequency
        ts_plot(v_tweets_to_display[, c("tweet_created_at")], "mins") +
            ggplot2::theme_minimal() +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
            ggplot2::labs(
                x = NULL, y = NULL,
                title = paste("Frequency of", v_selected_subject, "Twitter statuses", sep = " "),
                subtitle = "Twitter status (tweet) counts aggregated using one-minute intervals",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet"
            )
    }

    showUniqueWords <- function(flag = TRUE) {
        v_number_words <<- sum(v_words_in_tweets$n)
        v_number_unique_words <<- nrow(v_words_in_tweets)
        
        if(flag) {
            withProgress(message = 'Tabulating Unique Words',
                         detail = 'This may take a while...', value = 100, {
                             DT::datatable(
                                 { v_words_in_tweets[, c("word", "n")] },
                                 colnames = c('Word', '#'),
                                 caption = htmltools::tags$caption(
                                     style = 'caption-side: bottom; text-align: center;',
                                     'Table 4: ', 
                                     htmltools::em('Unique Words')
                                 ),
                                 extensions = 'Buttons',
                                 escape = TRUE,
                                 rownames = FALSE,
                                 options = list(
                                     fixedColumns = TRUE,
                                     autoWidth = FALSE,
                                     ordering = TRUE,
                                     pageLength = 5,
                                     dom = 'Bftsp',
                                     buttons = c('copy', 'csv', 'excel')
                                 ))
                         })
        }
    }
    
    showWorldMap <- function() {
        withProgress(message = 'Generating Map',
                     detail = 'This may take a while...', value = 100, {
                         par(mar = c(0, 0, 0, 0))
                         map( 'world', fill = FALSE, col = 1:10, wrap=c(-180,180) )
                         
                         ## plot lat and lng points onto state map
                         with(v_tweets_to_display, points(lng, lat, pch = 21, cex = .75, col = rgb(1, 0, 0, 1)))
                         
                         # leaflet(v_tweets_to_display) %>%
                         #     addCircles(lng = ~longitude, lat = ~latitude) %>%
                         #     addTiles() %>%
                         #     addCircleMarkers(data = v_tweets_to_display, lat = ~latitude, lng = ~longitude,
                         #                      radius = 3,
                         #                      popup = ~as.character(tweet_text),
                         #                      # color = ~pal(Category),
                         #                      stroke = FALSE, fillOpacity = 0.8) #%>%
                         # # addLegend(pal=pal, values=bb_data$Category,opacity=1, na.label = "Not Available")%>%
                         # # addEasyButton(easyButton(
                         # #     icon="fa-crosshairs", title="ME",
                         # #     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
                     })
    }

    # showIndiaMap <- function() {
    #     withProgress(message = 'Generating Map',
    #                  detail = 'This may take a while...', value = 100, {
    #                      par(mar = c(0, 0, 0, 0))
    #                      maps::map("world", "India", lwd = .25)
    #                      
    #                      ## plot lat and lng points onto state map
    #                      with(v_tweets_to_display, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
    #                  })
    # }
    # 
    # showUSAMap <- function() {
    #     withProgress(message = 'Generating Map',
    #                  detail = 'This may take a while...', value = 100, {
    #                      ## plot state boundaries
    #                      par(mar = c(0, 0, 0, 0))
    #                      maps::map("usa", lwd = .25)
    #                      
    #                      ## plot lat and lng points onto state map
    #                      with(v_tweets_to_display, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
    #                  })
    # }
    
    # output$wordContributionToSentiments <- shinyRenderWidget(quoted = TRUE, {
    #     showWordContributionToSentiments()
    # })
    
    output$wordContributionToSentiments <- renderPlot({
        showWordContributionToSentiments()
    })
    
    showWordContributionToSentiments <- function() {
        v_data <- determineEmotionCount(v_words_in_tweets) %>%
                    filter(row_number() < input$v_top_n_word_contribution) %>%
                    group_by(v_original_sentiment) %>%
                    ungroup() %>%
                    mutate(word = reorder(word, v_original_n))
        v_data$word <- as.character(v_data$word)
        v_data <- data.frame(apply(v_data, 2, unclass))

        ggplot(aes(x = word, y = v_original_n, fill = v_original_sentiment), data = v_data, scale_x_continuous(breaks = NULL)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~v_original_sentiment, scales = "free_y") +
            labs(y = "Words Contribution to Individual Sentiments", x = NULL) +
            coord_flip()
    }
    
    output$sentiments <- renderPlot({
        showSentiments()
    })
    
    showSentiments <- function() {
        v_emotions <- determineEmotionCount(v_words_in_tweets) %>% 
            select(index, anger, anticipation, disgust, fear, joy, 
                   sadness, surprise, trust) %>% 
            melt(id = "index") %>%
            rename(linenumber = index, sentiment_name = variable, value = value)
        
        v_emotions_group <- group_by(v_emotions, sentiment_name)
        v_by_emotions <- summarise(v_emotions_group, values=sum(value))
        
        ggplot(aes(reorder(x=sentiment_name, values), y=values, fill=sentiment_name), data = v_by_emotions) +
            geom_bar(stat = 'identity') + 
            ggtitle('Sentiment in Tweets') +
            coord_flip() + 
            theme(legend.position="none")
    }
    
    output$positiveNegativeSentiments <- renderPlot({
        showPositiveNegativeSentiments()
    })
    
    showPositiveNegativeSentiments <- function() {
        v_data <- determineEmotionCount(v_words_in_tweets) %>% 
                    mutate(v_original_n = ifelse(v_positive_negative == "negative", 
                                                 -v_original_n, v_original_n))
        
        ggplot(data = v_data, aes(x = index, y = v_original_n, fill = v_positive_negative)) +
            geom_bar(stat = 'identity', position = position_dodge()) + 
            theme_minimal() +
            ylab("Sentiment") + 
            ggtitle("Positive and Negative Sentiment in Tweets") +
            scale_color_manual(values = c("red", "dark green")) +
            scale_fill_manual(values = c("red", "dark green"))
    }
    
    output$mostPrevalentEmotion <- renderText({
        showMostPrevalentEmotion()
    })
    
    showMostPrevalentEmotion <- function() {
        v_emotions <- determineEmotionCount(v_words_in_tweets) %>% 
                        select(index, anger, anticipation, disgust, fear, joy, 
                               sadness, surprise, trust) %>% 
                        melt(id = "index") %>%
                        rename(linenumber = index, sentiment_name = variable, value = value)
        
        v_emotions_group <- group_by(v_emotions, sentiment_name)
        v_by_emotions <- summarise(v_emotions_group, values = sum(value))
        v_temp <- v_by_emotions %>%
                    mutate_if(is.factor, as.character)

        v_prevalent_emotion <<- ifelse(nrow(subset(v_temp, values == max(values))) == 1,
                                      unname(unlist(subset(v_temp, values == max(values)))),
                                      "No Prevalent Emotion")
        
        return(v_prevalent_emotion)
    }
    
    output$sentimentScore <- renderPlot({
        determineSentimentScore()
    })
    
    determineSentimentScore <- function() {
        v_combined <- sort(union(levels(v_words_in_tweets$word), levels(v_nrc$word)))
        v_word_count <- inner_join(mutate(v_words_in_tweets, word=factor(word, levels=v_combined)),
                                   mutate(v_nrc, word=factor(word, levels=v_combined))
                                ) %>%
                            count(word, sentiment, sort = TRUE)
        
        v_data <- v_word_count %>%
                    filter(row_number() < input$v_top_n_sentiment_score) %>%
                    mutate(n = ifelse(sentiment %in% v_negative_emotions, -n, n)) %>%
                    mutate(word = reorder(word, n))
        
        ggplot(aes(word, n, fill = sentiment), data = v_data) +
            geom_col() +
            coord_flip() +
            labs(y = "Sentiment Score")
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
