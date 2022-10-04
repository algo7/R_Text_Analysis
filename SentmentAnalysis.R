################################
#                             ##
#    Install Packages         ##
#                             ##
################################

# List of required packages
required_pkgs <- c(
  "tm", "syuzhet"
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

# Plot it
sent1 <- barplot(
  height = word_freq,
  main = "Word Frequencies",
  ylab = "Count",
  names.arg = names(word_freq),
  # Space between axis labels perpendicular to the bars
  las = 2,
  # Gradient
  col = rainbow(50),
  ylim = c(0, max(word_freq) * 1.1),
)


# Plot it
sent2 <- barplot(sentiment_sum,
                 las = 2,
                 col = rainbow(10),
                 ylab = "Count",
                 main = "Sentiment Scores Comment",
                 ylim = c(0, max(sentiment_sum) * 1.1)
)

# Add actual value on top of the bars
text(sent2,
     sentiment_sum,
     labels = sentiment_sum,
     pos = 3
)

