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

# Text transformation / pre-processing
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

# Make reproducible results by setting the seed
set.seed(2645)

# PDF
pdf("wordcloud_packages.pdf")

# The current graphic device (PDF)
current_device <- dev.cur()

# PNG
png("wordcloud_packages.png", width = 12, height = 8, units = "in", res = 300)


# Every device has a display list which records all of the graphics operations that occur in the device. dev.copy and dev.print copy graphics contents by copying the display list from one device to another device. Also, automatic redrawing of graphics contents following the resizing of a device depends on the contents of the display list.
# After the command dev.control("inhibit"), graphics operations are not recorded in the display list so that dev.copy and dev.print will not copy anything and the contents of a device will not be redrawn automatically if the device is resized.
dev.control("enable")


# Plot
wordcloud(
    words = d$word, freq = d$freq, min.freq = 1,
    max.words = 200, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(12, "Set3")
)


# Copies the graphics contents of the current device to the device specified by ‘which’
dev.copy(which = current_device)

# Turn of the current graphic device after creating the plot
# to finish creating the image file
dev.off()
dev.off()