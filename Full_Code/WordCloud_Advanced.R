# List of required packages
required_pkgs <- c(
  "tm", "syuzhet","ggplot2","udpipe",
  "wordcloud","RWeka"
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
library("wordcloud")
library("RWeka")

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

# Function to substitute the given pattern with nothing
to_nothing <- content_transformer(
  function(text, pattern) {
    gsub(pattern, "", text)
  }
)

# Bigram Tokenizer
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Trigram Tokenizer
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers and punctuation
docs <- tm_map(docs, to_space, "[[:punct:] ]+")
docs <- tm_map(docs, to_space, "[[:digit:] ]+")

# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Strip single english character
docs <- tm_map(docs, to_space, "\\b[a-zA-Z]\\b")

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Eliminate extra white space at the start of a sentence
docs <- tm_map(docs, to_nothing, "^\\s+")


FormBigramWordCloud <- function(x){
  # Form bigram term-doc matrix
  tdm.bigram = TermDocumentMatrix(docs,control = list(tokenize = BigramTokenizer))
  
  # Calculate bigram frequency
  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  
  # Wordcloud color selection
  pal=brewer.pal(8,"Blues")
  pal=pal[-(1:3)]

  wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
}


