# List of required packages
required_pkgs <- c(
    "tm", "syuzhet"
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
docs <- tm_map(docs, removeWords, c(
  "lake","always",
  "one","per",
  "palace",
  "just", "also", "can",
  "every"
))

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(docs)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

# Sum the frequencies of all words
word_freq <- rowSums(m)

# Extract only the words with frequency greater than 60
word_freq <- subset(word_freq, word_freq >= 5)

# Plot it
sent1 <- barplot(
    height = word_freq,
    main = "World Frequencies",
    ylab = "Count",
    names.arg = names(word_freq),
    # Space between axis labels perpendicular to the bars
    las = 2,
    # Gradient
    col = rainbow(50),
    ylim = c(0, max(word_freq) * 1.1),
)

# Add actual value on top of the bars
text(sent1, word_freq, labels = word_freq, pos = 3, cex = 0.7)

# Uses National Research Council Canada (NRC)  Emotion lexicon
# with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust)
# and two sentiments (negative and positive)
sentiment_scores <- get_nrc_sentiment(names(word_freq), language = "english")

# Sum the sentiment score matrix
sentiment_sum <- colSums(sentiment_scores)
sentiment_sum<-sort(sentiment_sum,decreasing = TRUE)
# Plot it
sent2 <- barplot(sentiment_sum,
    las = 2,
    col = rainbow(10),
    ylab = "Count",
    main = "Sentiment Scores Comment",
    ylim = c(0, max(sentiment_sum) * 1.1)
)

# Add actual value on top of the bars
text(sent2,
    sentiment_sum,
    labels = sentiment_sum,
    pos = 3
)

