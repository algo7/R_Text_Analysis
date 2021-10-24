# List of required pacakges
required_pkgs <- c("tm", "SnowballC", "wordcloud", "RColorBrewer")

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
library("RColorBrewer")

# Read the text file from internet
file_path <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"

# Load the file into a variable
text <- readLines(file_path)

# Load the data as a corpus
docs <- VCorpus(VectorSource(text))

# Text transformation
# Function to substitute the given pattern with a white space
to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

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

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)

# Build term-document matrix
# Document matrix is a table containing the frequency of the words.
# Column names are words and row names are documents
dtm <- TermDocumentMatrix(docs)

# Convert term doc matrix into matrix
m <- as.matrix(dtm)

# Sum the frequencies of each word
v <- sort(rowSums(m), decreasing = TRUE)

# Convert the vector into a data frame
d <- data.frame(word = names(v), freq = v)

# Plot
wordcloud(
    words = d$word, freq = d$freq, min.freq = 1,
    max.words = 200, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(12, "Set3")
)