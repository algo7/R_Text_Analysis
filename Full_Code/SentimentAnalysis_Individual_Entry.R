# List of required packages
required_pkgs <- c(
    "tm", "syuzhet","tidyr"
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
  "one","per","hotel","rooms",
  "palace","staff","room",
  "just", "also", "can",
  "every","although","get",
  "even","will","radissons",
  "radisson","rivage"
))


# Convert VCorpus to data frame
df<- data.frame(text=sapply(docs, as.character))

# Apply the get_nrc_sentiment function to the text column and create a new column
df$sentiment <- lapply(df$text, get_nrc_sentiment)

# Convert the sentiment column to separate columns
df <- df %>%
  unnest_wider(sentiment)

# Save the dataframe to a CSV file
write.csv(df, file = "results.csv", row.names = FALSE)



