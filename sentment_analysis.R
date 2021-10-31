# List of required pacakges
required_pkgs <- c(
    "tm"
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

# Load dataset
data <- read.csv("./sample_tweets.csv", header = T)

# Print the structure of the dataset
# str(data)

# Extract the text column
corpus <- iconv(data$text)

# Load the text as a corpus
corpus <- Corpus(VectorSource(corpus))

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
remove_special_chars <- function(text) gsub("[^a-z ]", " ", text)

# Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove urls
corpus <- tm_map(corpus, remove_url)

# Remove urls
corpus <- tm_map(corpus, remove_user_name)

# Remove all non-aphabet-numeric characters
corpus <- tm_map(corpus, removeSpecialChars)

# Remove rt, which is appended to the beginning of the tweet
corpus <- tm_map(corpus, removeWords, c("rt"))

# Remove english common stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Remove urls
corpus <- tm_map(corpus, remove_orphan_alphabet)

# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)

inspect(corpus[1:10])