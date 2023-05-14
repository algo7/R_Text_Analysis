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

# Function to remove all english nouns
remove_undesired_pos <- content_transformer(function(text) {
  
  # Annotate the text using the UDPipe model
  annotation <- udpipe_annotate(ud_model, text)
  
  # Convert the annotation to a data frame
  annotation_df <- as.data.frame(annotation)
  
  # Filter undesired POS
  filtered <- subset(annotation_df, 
                     !(upos %in% c("NOUN", "PROPN", "VERB", "PRON", "NUM", "INTJ", "AUX", "CCONJ", "ADP", "X")))
  # print(paste("Filtered POS tags:", paste(filtered$upos, collapse = ", ")))
  
  
  # Combine the processed data into a single string
  # collapse is required other wise each non-nouns will be an individual
  # element in the character vector while a document is a string
  paste(filtered$token, collapse = " ")
})


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers and punctuation
docs <- tm_map(docs, to_space, "[[:punct:] ]+")
docs <- tm_map(docs, to_space, "[[:digit:] ]+")

# Remove nouns, pronouns, verb, interjections, numbers, and proper nouns
docs <- tm_map(docs, remove_undesired_pos)

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
  "radisson","rivage","pool","view","stay",
  "back","thomas","property","back","island","day","hill","got",
  "resort","views","time","place","two","first","front","much","stayed",
  "really","around","everything", "also","many","little","sure","never","close","elysian",
  "still","away","ocean","next","beach","emerald","Margaritaville","margaritaville","margarita",
  "however","right","windward","passage","Windward","secret","harbour","point","dive","deep","tamarind","ritz","ferry"
))

# Strip single english character
docs <- tm_map(docs, to_space, "\\b[a-zA-Z]\\b")

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Eliminate extra white space at the start of a sentence
docs <- tm_map(docs, to_nothing, "^\\s+")




