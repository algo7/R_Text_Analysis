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
library("udpipe")

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

# Function to remove all english nouns
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
write.csv(df, file = paste(filename), row.names = FALSE)

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
write.csv(df, file = paste(filename,"_classified.csv",sep=""), row.names = FALSE)


