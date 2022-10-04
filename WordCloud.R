################################
#                             ##
#    Install Packages         ##
#                             ##
################################

# Set seed => all graphs will look the same given the same input
set.seed(2645)
# List of required packages
required_pkgs <- c(
  "tm", "SnowballC", "wordcloud", "RColorBrewer",
  "wordcloud2"
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
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")

################################
#                             ##
#          START HERE         ##
#                             ##
################################


























################################
#                             ##
#        STOP HERE            ##
#                             ##
#    Graphic Settings         ##
#                             ##
################################

# World Cloud
wc <- wordcloud(
  words = d$word,
  freq = d$freq,
  min.freq = 50,
  max.words = 100,
  # Graphic Stuff
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(9, "Set1")
)


# Bar plot of the top 30 most frequent words
top30_word_histo <- barplot(
  height = d[1:30, ]$freq,
  # Label for each bar
  names.arg = d[1:30, ]$word,
  # Titles
  main = "Top 30 Most Frequent Words",
  # Y-axis label
  ylab = "Frequencies",
  # Graphic stuff
  las = 2,
  col = "lightblue",
  ylim = c(0, max(d$freq) + 5)
)