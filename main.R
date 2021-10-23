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
docs <- Corpus(VectorSource(text))

# Text transformation
# Function to substitute the given pattern with a white space
to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Remove /
docs <- tm_map(docs, to_space, "/")

# Remove @
docs <- tm_map(docs, to_space, "@")

# Remove |
docs <- tm_map(docs, to_space, "\\|")

