# List of required packages
required_pkgs <- c(
  "tm", "syuzhet","ggplot2",
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
library("wordcloud")
library("RWeka")


######## Start Here ##########

# Load data set
data <- read.csv(file.choose(), header = T)

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

# Remove your own stop word
# specify your stop words as a character vector
docs <- tm_map(docs, removeWords, c(
  "lake", "always",
  "one", "per",
  "palace",
  "just", "also", "can",
  "every", "although", "get",
  "even", "will", "radissons",
  "radisson", "rivage", "bolongo",
  "back", "thomas", "got", "elysian", "Emerald Beach",
  "still", "away", "next", "emerald", "Margaritaville", "margaritaville", "margarita", "bluebeards", "bolongo bay",
  "however", "windward", "passage", "Windward", "secret", "harbour", "point", "dive", "deep", "tamarind", "ritz", "ferry"
))

# Strip single english character
docs <- tm_map(docs, to_space, "\\b[a-zA-Z]\\b")

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Eliminate extra white space at the start of a sentence
docs <- tm_map(docs, to_nothing, "^\\s+")


# Create a bigram wordcloud
FormBigramWordCloud <- function(){
  # Form bigram term-doc matrix
  tdm.bigram = TermDocumentMatrix(docs,control = list(tokenize = BigramTokenizer))
  
  # Calculate bigram frequency
  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  
  # Wordcloud color selection
  pal=brewer.pal(8,"Blues")
  pal=pal[-(1:3)]

  # Generate wordcloud
  wordcloud(
    word = freq.df$word,
    freq = freq.df$freq,
    max.words = Inf,
    # min.freq = 10,
    random.order = F, 
    colors=pal
    )
  
  # Horizontal barchart for frequency visualization
  ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Bigrams") + ylab("Frequency") +
    ggtitle("Most frequent bigrams")
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
  wordcloud(
    word = freq.df$word,
    freq = freq.df$freq,
    max.words = Inf,
    # min.freq = 10,
    random.order = F, 
    colors=pal
  )
  
  # Horizontal barchart for frequency visualization
  ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
    geom_bar(stat="identity") + coord_flip() + 
    xlab("Trigrams") + ylab("Frequency") +
    ggtitle("Most frequent trigrams")
}

# Call the function
FormBigramWordCloud()
FormTrigramWordCloud()

