# Set seed => all graphs will look the same given the same input
set.seed(2645)
# List of required packages
required_pkgs <- c(
  "tm", "SnowballC", "wordcloud", "RColorBrewer",
  "wordcloud2"
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
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")


######## Start Here ##########
# Load Data
data <- read.csv(file.choose(), header = T)

# Extract the title
# titles <- iconv(data$title)

# Extract the content
contents <- iconv(data$content)

# Load the data as a corpus
docs <- VCorpus(VectorSource(contents))

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
  "lausanne", "just", "also", "can",
  "every"
))

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(docs)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

# Sort the matrix and convert it into a numeric vector
v <- sort(rowSums(m), decreasing = TRUE)

# Convert the vector into a data frame
d <- data.frame(word = names(v), freq = v)


# Write to CSV
#write.csv(d, "beau_rivage_palace_dtm.csv", row.names = F)


# WC Plot
wc <- wordcloud(
  words = d$word,
  freq = d$freq,
  min.freq = 5,
  max.words = 100,
  # Graphic Stuff
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(12, "Set3")
)


# Create a bar plot of the top 30 most frequent words
top30_word_histo <- barplot(
  height = d[1:20, ]$freq,
  # Label for each bar
  names.arg = d[1:20, ]$word,
  # Titles
  main = "Top 20 Most Frequent Words",
  # Y-axis label
  ylab = "Frequencies",
  # Graphic stuff
  las = 2,
  col = "lightblue",
  ylim = c(0, max(d$freq) + 5)
)

########## Bonus
# Find words that appear more than 50 times
print(findFreqTerms(dtm, lowfreq = 50))

# Find terms that are associated with "good" with a correlation of > 0.1
print(findAssocs(dtm, terms = c("great", "view", "beautiful"), corlimit = 0.2))

# Wordcloud2 support, needs to be printed explicitly
# wc2 <- wordcloud2(data = d, size = 1.6, color = "random-dark")