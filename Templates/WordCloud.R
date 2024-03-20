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

# Data source
data_source <- "https://storage.algo7.tools/Hotel_Schweizerhof_Bern_Spa-2807b3b1-88.csv"

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