# List of required packages
required_pkgs <- c(
    "tm", "syuzhet","tidyr","ggplot2"
)

# Empty list to hold dependencies that are not installed
not_met_dependencies <- c()

# Check if required packages are installed
for (pkg in required_pkgs) {
    if (!(pkg %in% rownames(installed.packages()))) {
        not_met_dependencies <- c(not_met_dependencies, pkg)
    }
}

# Install missing packages
if (length(not_met_dependencies) != 0) {
    install.packages(not_met_dependencies)
}

# Load packages
library("tm")
library("syuzhet")
library("tidyr")
library("udpipe")
library("ggplot2")

# Download the english lang model for udpipe
# ud_model <- udpipe_download_model(language = "english")

# Load model
# ud_model <- udpipe_load_model(ud_model$file_model)
ud_model <- udpipe_load_model("/home/algo7/Desktop/code/R_Text_Analysis/english-ewt-ud-2.5-191206.udpipe")

######## Start Here ##########

path <- Sys.getenv("DATA_SOURCE_PATH")

# Load data set
if (path != ""){
  data <- read.csv(path, header = T)
}else{
  file <- file.choose()
  filename <- basename(file)
  data <- read.csv(file, header = T)
  splitted_hotel_name <- unlist(strsplit(filename," "))
  hotel_name <- unlist(splitted_hotel_name[1], splitted_hotel_name[2])
}

# Extract the text column
docs <- iconv(data$Text)

# Load the text as a corpus
docs <- VCorpus(VectorSource(docs))

# Text transformation / pre-processing
# Function to substitute the given pattern with a white space
to_space <- content_transformer(
    function(text, pattern) {
        gsub(pattern, " ", text)
    }
)

# Function to substitute the given pattern with nothing
to_nothing <- content_transformer(
  function(text, pattern) {
    gsub(pattern, "", text)
  }
)

# Function to substitute the given pattern with another given pattern
to_custom <- content_transformer(
  function(text, pattern_a, pattern_b) {
    gsub(pattern_a, pattern_b, text)
  }
)

# Function to remove all undesired pos
remove_undesired_pos <- content_transformer(function(text) {
  
  # Annotate the text using the UDPipe model
  annotation <- udpipe_annotate(ud_model, text)
  
  # Convert the annotation to a data frame
  annotation_df <- as.data.frame(annotation)
  
  # Filter undesired POS
  filtered <- subset(annotation_df, 
                     !(upos %in% c("PROPN", "PRON", "NUM", "INTJ", "AUX", "CCONJ", "ADP", "X")))
  # print(paste("Filtered POS tags:", paste(filtered$upos, collapse = ", ")))
  
  
  # Combine the processed data into a single string
  # collapse is required other wise each non-nouns will be an individual
  # element in the character vector while a document is a string
  paste(filtered$token, collapse = " ")
})

# Function to adj and adv
remove_adjective_adverb <- content_transformer(function(text) {
  
  # Annotate the text using the UDPipe model
  annotation <- udpipe_annotate(ud_model, text)
  
  # Convert the annotation to a data frame
  annotation_df <- as.data.frame(annotation)
  
  # Filter undesired POS
  filtered <- subset(annotation_df, 
                     !(upos %in% c("ADJ","ADV")))
  # print(paste("Filtered POS tags:", paste(filtered$upos, collapse = ", ")))
  
  
  # Combine the processed data into a single string
  # collapse is required other wise each non-nouns will be an individual
  # element in the character vector while a document is a string
  paste(filtered$token, collapse = " ")
})


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove nouns, pronouns, verb, interjections, numbers, and proper nouns
docs <- tm_map(docs, remove_undesired_pos)

# Remove numbers and punctuation
docs <- tm_map(docs, to_space, "[[:punct:] ]+")
docs <- tm_map(docs, to_space, "[[:digit:] ]+")

# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stop words as a character vector
docs <- tm_map(docs, removeWords, c(
  "lake","always",
  "one","per",
  "palace",
  "just", "also", "can",
  "every","although","get",
  "even","will","radissons",
  "radisson","rivage","bolongo",
  "back","thomas","got","elysian","Emerald Beach",
  "still","away","next","emerald","Margaritaville","margaritaville","margarita","bluebeards","bolongo bay",
  "however","windward","passage","Windward","secret","harbour","point","dive","deep","tamarind","ritz","ferry"
))

# Strip single english character
docs <- tm_map(docs, to_space, "\\b[a-zA-Z]\\b")

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Eliminate extra white space at the start of a sentence
docs <- tm_map(docs, to_nothing, "^\\s+")

# Convert VCorpus to data frame
df<- data.frame(text=sapply(docs, as.character))

# Apply the get_nrc_sentiment function to the text column and create a new column
df$sentiment <- lapply(df$text, get_nrc_sentiment)

# Convert the sentiment column to separate columns
df <- df %>%
  unnest_wider(sentiment)

# Combined the raw data with analysis results
df <- cbind(data,df)

# Save the dataframe to a CSV file
write.csv(df, file = paste("Stage_1",filename, sep = ""), row.names = FALSE)

# Filter out rows that have all NRC categories with 0 value
df <- df[rowSums(df[, 7:16]) != 0, ]

# Reduce NRC category dimension
df$positive <- df$joy + df$trust + df$anticipation + df$positive
df$negative <- df$anger + df$disgust + df$fear + df$sadness + df$negative

# Remove combined categories and netural category => surprise
df <- subset(df,select = -c(trust,joy,anticipation,anger,disgust,fear,sadness,surprise))

# Remove rows where positive = negative. in other words, keep those where the abs diff is > 0
df <- subset(df, abs(positive - negative) > 0)

# Function to classify text based on highest score
classify_text <- function(row) {

  # Get index of column with highest score
  highest_index <- which.max(row[7:16])
  return (names(highest_index))
}

# MARGIN 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns
df$class <- apply(df, MARGIN=1, classify_text)

# Save the dataframe to a CSV file
write.csv(df, file = paste("Stage_2", filename,sep = ""), row.names = FALSE)


graph_top_words <- function(emotion, timespan){
  
  emotion_lower <- tolower(emotion)
  
  if (timespan == "All") {
    df_emo <- df[which(df$class == emotion_lower),]
  }else{
    df[which(df$class == emotion_lower & df$Year == timepsan),]
  }

  # Extract the text column
  docs_emo <- iconv(df_emo$text)
  
  # Load the text as a corpus
  docs_emo <- VCorpus(VectorSource(docs_emo))
  
  # Remove ADJ
  docs_emo <- tm_map(docs_emo, remove_adjective_adverb)
  
  docs_emo <- tm_map(docs_emo,to_custom,"desk", "frontdesk")
  docs_emo <- tm_map(docs_emo,to_custom,"check", "check-in/check-out")
  
  dtm_emo <- TermDocumentMatrix(docs_emo)
  
  # Convert term doc matrix into matrix
  m_emo <- as.matrix(dtm_emo)

  # Sum the frequencies of all words
  word_freq <- sort(rowSums(m_emo),decreasing = T)
  
  # Calculate the total number of unique words
  total_words <- length(word_freq)
  
  # Define the percentage of words to consider as "top words"
  percentage <- 0.01
  
  # This line calculates the number of words to be considered as the "top words". 
  # The line multiplies "total_words" by "percentage", then rounds the result up 
  # to the nearest integer using the "ceiling" function to ensure that at least one word is included in the top words.
  top_1_percent <- ceiling(total_words * percentage )
  
  # This line calculates the frequency threshold below which a word is not considered as one of the top words. 
  # The "quantile" function takes the "word_freq" vector and the percentage of words to be considered as input. 
  # The percentage value is calculated by subtracting "top_1_percent" from "total_words" and then dividing the result by "total_words".
  # The resulting value represents the fraction 
  # of words to be excluded from the top words, so the "quantile" function calculates 
  # the frequency value below which the excluded words fall.
  freq_threshold <- quantile(word_freq, 1 - top_1_percent/total_words)
  
  # Extract the top x% most frequent words
  top_words <- names(word_freq[word_freq >= freq_threshold])
  
  # Convert top_words to data frame
  top_words_df <- data.frame(word = top_words, freq = word_freq[word_freq >= freq_threshold], stringsAsFactors = FALSE)
  
  # Remove NAs from the data frame
  top_words_df <- top_words_df[!is.na(top_words_df$word), ]
  
  
  # Plot a bar chart of the top 1% most frequent words
  ggplot(top_words_df, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity", fill = rainbow(length(top_words_df$word))) +
    geom_text(aes(label = freq), vjust = -0.5, size = 3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste(hotel_name,"Top", percentage *100, "% Most Frequent Words for",
                       emotion,"Comments"), x = "Word", y = "Frequency")
  
}

graph_top_words("Positive")
graph_top_words("Negative")
