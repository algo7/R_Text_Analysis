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
remove_nouns_transformer <- content_transformer(function(text) {
  
    # Annotate the text using the UDPipe model
    annotation <- udpipe_annotate(ud_model, text)
 
    # Convert the annotation to a data frame
    annotation_df <- as.data.frame(annotation)

    # Filter out nouns
    non_nouns <- subset(annotation_df, upos != "NOUN" & upos != "PROPN")

    # Combine the non-nouns into a single string
    # collapse is required other wise each non-nouns will be an individual
    # element in the character vector while a document is a string
    paste(non_nouns$token, collapse = " ")
})


# Replace 'docs' with the name of your corpus variable
docs <- tm_map(docs, remove_nouns_transformer)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove numbers and punctuation
docs <- tm_map(docs, to_space, "[[:punct:] ]+")
docs <- tm_map(docs, to_space, "[[:digit:] ]+")


# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stop words as a character vector
# docs <- tm_map(docs, removeWords, c(
#   "lake","always",
#   "one","per","hotel","rooms",
#   "palace","staff","room",
#   "just", "also", "can",
#   "every","although","get",
#   "even","will","radissons",
#   "radisson","rivage","pool","view","stay",
#   "back","thomas","property","back","island","day","hill",
#   "resort","views","time","place"
# ))

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(docs)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

# Sum the frequencies of all words
word_freq <- sort(rowSums(m),decreasing = T)

# Extract only the words with frequency greater than 170
word_freq <- subset(word_freq, word_freq >= 120)


# Word Frequencies plot
word_freq_df <- data.frame(words = names(word_freq), freq = word_freq)
ggplot(word_freq_df, aes(x = reorder(words, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = rainbow(length(word_freq))) +
  geom_text(aes(label = freq), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Word Frequencies", x = "Terms", y = "Count")



# Uses National Research Council Canada (NRC)  Emotion lexicon
# with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust)
# and two sentiments (negative and positive)
sentiment_scores <- get_nrc_sentiment(names(word_freq), language = "english")

# Sum the sentiment score matrix
sentiment_sum <- colSums(sentiment_scores)
sentiment_sum<-sort(sentiment_sum,decreasing = TRUE)

# Sentiment Scores plot
sentiment_sum_df <- data.frame(sentiments = names(sentiment_sum), scores = sentiment_sum)
ggplot(sentiment_sum_df, aes(x = reorder(sentiments, -scores), y = scores)) +
  geom_bar(stat = "identity", fill = rainbow(10)) +
  geom_text(aes(label = scores), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment Scores Comment", x = "Terms", y = "Count")


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
  labs(title = "Sentiment Scores All Comment", x = "Terms", y = "Count")+
  ylim(0, max(sentiment_sum_df$scores) * 1.1)


