rm(list = ls()) # clears workspace

# List of required pacakges
required_pkgs <- c(
    "rJava", "NLP", "openNLP", "RWeka",
    "rvest", "ggmap", "rworldmap"
)

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
library("rJava")
library("NLP")
library("openNLP")
library("RWeka")
library("rvest")
library("ggmap")
library("rworldmap")

# Read html file
page <- read_html("https://en.wikipedia.org/wiki/Walmart")
print(page)