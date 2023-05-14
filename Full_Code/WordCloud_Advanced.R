# List of required packages
required_pkgs <- c(
  "tm", "syuzhet","ggplot2",
  "wordcloud","RWeka","udpipe"
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
library("wordcloud")
library("RWeka")
library("udpipe")

# Download the english lang model for udpipe
# ud_model <- udpipe_download_model(language = "english")

# Load model
# ud_model <- udpipe_load_model(ud_model$file_model)
# ud_model <- udpipe_load_model("/home/algo7/Desktop/code/R_Text_Analysis/english-ewt-ud-2.5-191206.udpipe")
######## Start Here ##########

# Load data set
file <- file.choose()
filename <- basename(file)
data <- read.csv(file, header = T)
splitted_hotel_name <- unlist(strsplit(filename, " "))
hotel_name <- paste(splitted_hotel_name[1], splitted_hotel_name[2])

data <- read.csv(file, header = T)

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


# Function to substitute the given pattern with another given pattern
to_custom <- content_transformer(
  function(text, pattern_a, pattern_b) {
    gsub(pattern_a, pattern_b, text)
  }
)

# Function to remove all undesired pos
remove_undesired_pos <- content_transformer(function(text) {
  # Annotate the text using the UDPipe model
  annotation <- udpipe_annotate(ud_model, text)
  
  # Convert the annotation to a data frame
  annotation_df <- as.data.frame(annotation)
  
  # Filter undesired POS
  filtered <- subset(
    annotation_df,
    !(upos %in% c("PROPN", "PRON", "NUM", "VERB", "INTJ", "AUX", "CCONJ", "ADP", "X"))
  )
  # print(paste("Filtered POS tags:", paste(filtered$upos, collapse = ", ")))
  
  
  # Combine the processed data into a single string
  # collapse is required other wise each non-nouns will be an individual
  # element in the character vector while a document is a string
  paste(filtered$token, collapse = " ")
})

# Bigram Tokenizer
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Trigram Tokenizer
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# # Remove nouns, pronouns, verb, interjections, numbers, and proper nouns
# docs <- tm_map(docs, remove_undesired_pos)

# Remove numbers and punctuation
docs <- tm_map(docs, to_space, "[[:punct:] ]+")
docs <- tm_map(docs, to_space, "[[:digit:] ]+")

# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stop words as a character vector
docs <- tm_map(docs, removeWords, c(
  "lake", "always",
  "one", "per",
  "palace","resort",
  "just", "also", "can",
  "every", "although", "get",
  "even", "will", "radissons",
  "radisson", "rivage", "bolongo",
  "back", "thomas", "got", "elysian", "Emerald Beach",
  "still", "away", "next", "emerald", "Margaritaville", "margaritaville", "margarita", "bluebeards", "bolongo bay",
  "however", "windward", "passage", "Windward", "secret", "harbour", "point", "dive", "deep", "tamarind", "ritz", "ferry",
  "blubeard", "castle"
))

# Strip single english character
docs <- tm_map(docs, to_space, "\\b[a-zA-Z]\\b")

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Eliminate extra white space at the start of a sentence
docs <- tm_map(docs, to_nothing, "^\\s+")


# Save the dataframe to a CSV file
# Convert VCorpus to data frame
df <- data.frame(processed_text = sapply(docs, as.character))
write.csv(df, file = paste("./Debug/Stage_1_", filename, sep = ""), row.names = FALSE)

# Create a bigram wordcloud
FormBigramWordCloud <- function(){
  # Form bigram term-doc matrix
  tdm.bigram = TermDocumentMatrix(docs,control = list(tokenize = BigramTokenizer))
  
  # Calculate bigram frequency
  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)

  # Generate wordcloud
  png(paste("./Graphs/", "word_cloud_bigram_", hotel_name, "_combined", ".png", sep = ""), width = 13.66,height = 8.68, res = 500, units='in')
  par(mai = c(1,1,1,1))
  wordcloud(
    word = freq.df$word,
    freq = freq.df$freq,
    max.words = 150,
    rot.per = 0.35,
    # min.freq = 10,
    random.order = F, 
    colors = brewer.pal(9, "Set1")
    )
  dev.off()
  
  # Horizontal barchart for frequency visualization
  plot <- ggplot(head(freq.df, 20), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Bigrams") + ylab("Frequency") +
    ggtitle("Top 20 Bigrams")
  
  ggsave(paste("./Graphs/", "freq_bigram_", hotel_name, "_combined", ".png", sep = ""), plot = plot, width = 13.66, height = 8.68, dpi = 600)
}

# Create a trigram wordcloud
FormTrigramWordCloud <- function (){
  # Form trigram term-doc matrix
  tdm.trigram = TermDocumentMatrix(docs, control = list(tokenize = TrigramTokenizer))

  # Calculate trigram frequency
  freq = sort(rowSums(as.matrix(tdm.trigram)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)

  # Generate wordcloud
  png(paste("./Graphs/", "word_cloud_trigram_", hotel_name, "_combined", ".png", sep = ""), width = 13.66,height = 8.68, res = 500, units='in')
  par(mai = c(1,1,1,1))
  wordcloud(
    word = freq.df$word,
    freq = freq.df$freq,
    max.words = 150,
    # min.freq = 10,
    random.order = F,
    rot.per = 0.35,
    colors = brewer.pal(9, "Set1")
  )
  dev.off()

  # Horizontal barchart for frequency visualization
  plot <- ggplot(head(freq.df,20), aes(reorder(word,freq), freq)) +
    geom_bar(stat="identity") + coord_flip() +
    xlab("Trigrams") + ylab("Frequency") +
    ggtitle("Top 20 Trigrams")

  ggsave(paste("./Graphs/", "freq_trigram_", hotel_name, "_combined", ".png", sep = ""), plot = plot, width = 13.66, height = 8.68, dpi = 600)
}

# Call the function
FormBigramWordCloud()
FormTrigramWordCloud()

