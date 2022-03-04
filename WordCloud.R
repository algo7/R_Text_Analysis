# List of required pacakges
required_pkgs <- c(
  "tm", "SnowballC", "wordcloud", "RColorBrewer",
  "wordcloud2"
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
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")

# Load Data
data <- read.csv("reviews.csv",header = T)

# Convert the raw data to a data frame
data <- as.data.frame(data)

# Extract the title
titles <- iconv(data$title)

# Extract the content
contents <- iconv(data$content)

# Load the data as a corpus
docs <- VCorpus(VectorSource(contents))

# Text transformation / pre-processing
# Function to substitute the given pattern with a white space
to_space <- content_transformer(function(text, pattern) gsub(pattern, " ", text))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove /
docs <- tm_map(docs, to_space, "/")

# Remove @
docs <- tm_map(docs, to_space, "@")

# Remove |
docs <- tm_map(docs, to_space, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("hotel",
                                    "staff","room",
                                    "lake","rooms",
                                    "one","service",
                                    "palace","breakfast",
                                    "lausanne","just","also","can",
                                    "every"
                                    ))

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(docs)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

v <- sort(rowSums(m), decreasing = TRUE)

# Convert the vector into a data frame
d <- data.frame(word = names(v), freq = v)

set.seed(2645)


# For the the graph (1 row, 2 cols)
#par(mfrow = c(1, 2))

# WC Plot
wc <- wordcloud(
  words = d$word, 
  freq = d$freq, 
  min.freq = 50,
  max.words = 200, 
  # Graphic Stuff
  random.order = FALSE, 
  rot.per = 0.35,
  colors = brewer.pal(12, "Set3")
)


# Create a histogram of the top 20 most frequent words
top30_word_histo <- barplot(d[1:30, ]$freq,
                            las = 2, names.arg = d[1:30, ]$word,
                            col = "lightblue", main = "Top 10 Most Frequent Words",
                            ylab = "Frequencies", ylim = c(0, max(d$freq) + 5), # yaxp = c(0, max(d$freq) + 5, 10)
)

# Find words that appear more than 4 times
print(findFreqTerms(dtm, lowfreq = 50))

# Find terms that are associated with "freedom" with a correlation of > 0.3
print(findAssocs(dtm, terms = "lausanne", corlimit = 0.1))

# Wordcloud2 support, needs to be printed explicitly
#wc2 <- wordcloud2(data = d, size = 1.6, color = "random-dark")


