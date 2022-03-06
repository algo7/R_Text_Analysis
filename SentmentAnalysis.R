# Set seed => all graphs will look the same given the same input
set.seed(2645)
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

######## Start Here ##########


######## Graphs ##########
# Bar plots of word frequencies
sent1 <- barplot(
    height= word_freq,
    main = "Word Frequencies",
    ylab = "Count",
    names.arg = names(word_freq),
    # Graphic stuff
    # Space between axis labels perpendicular to the bars
    las = 2,
    # Gradient
    col = rainbow(50),
    ylim = c(0, max(word_freq) * 1.1),
)

# Add actual value on top of the bars
text(sent1, 
     word_freq, 
     labels = word_freq, 
     pos = 3, 
     cex = 0.7)


# Plot it
sent2 <- barplot(sentiment_sum,
    ylab = "Count",
    main = "Reviews Sentiment Scores",
    # Graphic stuff
    col = rainbow(10),
    las = 2,
    ylim = c(0, max(sentiment_sum) * 1.1)
)

# Add actual value on top of the bars
text(sent2, 
     sentiment_sum, 
     labels = sentiment_sum, 
     pos = 3)
