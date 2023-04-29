# List of required packages
required_pkgs <- c(
    "tm", "syuzhet","ggplot2","udpipe"
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
library("ggplot2")
library("udpipe")

# Download the english lang model for udpipe
ud_model <- udpipe_download_model(language = "english")

# Load model
ud_model <- udpipe_load_model(ud_model$file_model)

######## Start Here ##########

# Load data set
data <- read.csv(file.choose(), header = T)

# Extract the text column
docs <- iconv(data$content)

# Load the text as a corpus
docs <- VCorpus(VectorSource(docs))

# Text transformation / pre-processing

# Function to substitute the given pattern with a white space
to_space <- content_transformer(
    function(text, pattern) {
        gsub(pattern, " ", text)
    }
)

# Function to remove all english nouns
remove_undesired_pos <- content_transformer(function(text) {

    # Annotate the text using the UDPipe model
    annotation <- udpipe_annotate(ud_model, text)

    # Convert the annotation to a data frame
    annotation_df <- as.data.frame(annotation)

    # Filter undesired POS
    filtered <- subset(annotation_df, 
                       !(upos %in% c("NOUN", "PROPN", "VERB", "PRON", "NUM", "INTJ", "AUX", "CCONJ", "ADP", "X")))
    # print(paste("Filtered POS tags:", paste(filtered$upos, collapse = ", ")))

    
    # Combine the processed data into a single string
    # collapse is required other wise each non-nouns will be an individual
    # element in the character vector while a document is a string
    paste(filtered$token, collapse = " ")
})


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove numbers and punctuation
docs <- tm_map(docs, to_space, "[[:punct:] ]+")
docs <- tm_map(docs, to_space, "[[:digit:] ]+")

# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove nouns, pronouns, verb, interjections, numbers, and proper nouns
docs <- tm_map(docs, remove_undesired_pos)

# Remove your own stop word
# specify your stop words as a character vector
docs <- tm_map(docs, removeWords, c(
  "lake","always",
  "one","per","hotel","rooms",
  "palace","staff","room",
  "just", "also", "can",
  "every","although","get",
  "even","will","radissons",
  "radisson","rivage","pool","view","stay",
  "back","thomas","property","back","island","day","hill","got",
  "resort","views","time","place","two","first","front","much","stayed",
  "really","around","everything", "also","many","little","sure","never","close"
))

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(docs)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

# Sum the frequencies of all words
word_freq <- sort(rowSums(m),decreasing = T)

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
  labs(title = paste("Top", percentage *100, "% Most Frequent Words"), x = "Word", y = "Frequency")


# For the entire doc-term matrix
# Uses National Research Council Canada (NRC)  Emotion lexicon
# with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust)
# and two sentiments (negative and positive)
sentiment_scores_all <- get_nrc_sentiment(rownames(m), language = "english")

# Sum the sentiment score matrix
sentiment_sum_all <- colSums(sentiment_scores_all)
sentiment_sum_all <-sort(sentiment_sum_all,decreasing = TRUE)


# Sentiment Scores plot
sentiment_sum_df <- data.frame(sentiments = names(sentiment_sum_all), scores = sentiment_sum_all)
ggplot(sentiment_sum_df, aes(x = reorder(sentiments, -scores), y = scores)) +
  geom_bar(stat = "identity", fill = rainbow(10)) +
  geom_text(aes(label = scores), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15)) +
  labs(title = "NRC Sentiment Analysis of All Comments", x = "Terms", y = "Count")+
  ylim(0, max(sentiment_sum_df$scores) * 1.1)



# For the most frequent words defined above
sentiment_scores <- get_nrc_sentiment(top_words_df$word, language = "english")

# Sum the sentiment score matrix
sentiment_sum <- colSums(sentiment_scores)
sentiment_sum<-sort(sentiment_sum,decreasing = TRUE)

# Sentiment Scores plot
sentiment_sum_df <- data.frame(sentiments = names(sentiment_sum), scores = sentiment_sum)
ggplot(sentiment_sum_df, aes(x = reorder(sentiments, -scores), y = scores)) +
  geom_bar(stat = "identity", fill = rainbow(10)) +
  geom_text(aes(label = scores), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Sentiment Categories", x = "Terms", y = "Count")


## For word that accounts for x% of the entire courpus
# # Calculate the percentage of each word in the corpus
# word_freq_pct <- word_freq / sum(word_freq) * 100
# 
# # Extract only the words with frequency greater than the threshold
# freq_threshold <- 2 # set the threshold to 0.5%
# word_freq <- word_freq_pct[word_freq_pct >= freq_threshold]
# 
# 
# # Word Frequencies plot
# word_freq_df <- data.frame(words = names(word_freq), freq = word_freq)
# ggplot(word_freq_df, aes(x = reorder(words, -freq), y = freq)) +
#   geom_bar(stat = "identity", fill = rainbow(length(word_freq))) +
#   geom_text(aes(label = freq), vjust = -0.5, size = 3) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = paste("Frequent Terms (Occurrence >= ", freq_threshold,"% )"), x = "Terms", y = "Count")



