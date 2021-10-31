# List of required pacakges
required_pkgs <- c(
    "tm", "syuzhet", "parallel"
)

# Empty list to hold dependencies that are not isntalled
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
library("parallel")


# Creates a set of copies of R running
# in parallel and communicating over sockets.
cl <- makeCluster(detectCores() - 1)

# Parallelize the following functions
clusterExport(cl = cl, c("get_nrc_sentiment"))

# Load dataset
data <- read.csv("./sample_tweets.csv", header = T)

# Print the structure of the dataset
# str(data)

# Extract the text column
docs <- iconv(data$text)

# Load the text as a corpus
corpus <- VCorpus(VectorSource(docs))

# Text transformation / pre-processing
# Function to substitute the given pattern with a white space
to_space <- content_transformer(function(text, pattern) gsub(pattern, "", text))

# Url pattern
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# Remove orphan alphabets (one alphabet with space before and after)
remove_orphan_alphabet <- content_transformer(function(text) gsub("\\s[a-z]\\s", " ", text))

# Function to substitute url to white space
remove_url <- content_transformer(function(text) gsub(url_pattern, " ", text))

# Function to substitute url user name to white space
remove_user_name <- content_transformer(function(text) gsub("@(\\w+):", " ", text))

# Function to substitute non-alpha-num chars to white space
remove_special_chars <- content_transformer(function(text) gsub("[^a-z ]", " ", text))

# Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove urls
corpus <- tm_map(corpus, remove_url)

# Remove urls
corpus <- tm_map(corpus, remove_user_name)

# Remove all non-aphabet-numeric characters
corpus <- tm_map(corpus, remove_special_chars)

# Remove rt, which is appended to the beginning of the tweet
corpus <- tm_map(corpus, removeWords, c("rt"))

# Remove english common stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Remove appl & apple => name of the stock
corpus <- tm_map(corpus, removeWords, c("aapl", "apple"))

# Remove urls
corpus <- tm_map(corpus, remove_orphan_alphabet)

# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(corpus)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

# Sum the frequencies of all words
word_freq <- rowSums(m)

# Extract only the words with frequency greater than 25
word_freq <- subset(word_freq, word_freq >= 25)

# PDF settings
pdf("sentiment_analysis.pdf", width = 12, height = 8, compress = T)

# Plot it
sent1 <- barplot(word_freq,
    # axis lables perpendicular to the bars
    las = 2,
    # Gradient
    col = rainbow(50),
    ylab = "Count",
    main = "World Frequencies",
    ylim = c(0, max(word_freq) * 1.1),
)

# Add actual value on top of the bars
text(sent1, word_freq, labels = word_freq, pos = 3, cex = 0.7)

# Uses National Research Council Canada (NRC)  Emotion lexicon
# with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust)
# and two sentiments (negative and positive)
sentiment_scores <- get_nrc_sentiment(docs, language = "english")

# Sum the sentiment score matrix
sentiment_sum <- colSums(sentiment_scores)

# Plot it
sent2 <- barplot(sentiment_sum,
    las = 2,
    col = rainbow(10),
    ylab = "Count",
    main = "Sentiment Scores Tweets",
    ylim = c(0, max(sentiment_sum) * 1.1)
)

# Add actual value on top of the bars
text(sent2, sentiment_sum, labels = sentiment_sum, pos = 3)

# Turn of the current graphic device after creating the plot
# to finish creating the image file
dev.off()

# Stop the cluster
stopCluster(cl)