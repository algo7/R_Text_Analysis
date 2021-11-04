rm(list = ls()) # clears workspace

# List of required pacakges
required_pkgs <- c(
    "rJava", "NLP", "openNLP", "RWeka",
    "rvest", "ggmap", "rworldmap", "qdap",
    "rworldxtra"
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
library("rworldxtra")
library("RWeka")
library("rvest")
library("ggmap")
library("rworldmap")

# Google API key

# Read html file
page <- read_html("https://en.wikipedia.org/wiki/Walmart")

# Select the text base on html p (paragraph) tags
text <- html_text(html_nodes(page, "p"))

# Remove white spaces
text <- text[text != ""]

# Remove references
text <- gsub("\\[[0-9]]|\\[[0-9][0-9]]|\\[[0-9][0-9][0-9]]", "", text) # removing refrences [101] type

# Make one complete document
text <- paste(text, collapse = " ")

# Convert to string
text <- as.String(text)


# Generate an annotator which computes sentence annotations using
# the Apache OpenNLP Maxent sentence detector.
sent_annot <- Maxent_Sent_Token_Annotator()

# Generate an annotator which computes word token annotations using
# the Apache OpenNLP Maxent tokenizer.
word_annot <- Maxent_Word_Token_Annotator()

# Annotate for Persons, Locations and Organizations
loc_annot <- Maxent_Entity_Annotator(kind = "location")
person_annot <- Maxent_Entity_Annotator(kind = "person")
org_annot <- Maxent_Entity_Annotator(kind = "organization")

# Compute annotations by iteratively calling the given annotators
annot_l1 <- NLP::annotate(text, list(sent_annot, word_annot, loc_annot, person_annot, org_annot))

# Extract locations
k <- sapply(annot_l1$features, `[[`, "kind")

walmart_locations <- text[annot_l1[k == "location"]]

# Remove duplicates
all_places <- unique(walmart_locations)

# Geocode all locations using google geocoding api
all_places_geocoded <- geocode(all_places)

# Print them
print(all_places_geocoded)

# Create a map using the geocoded data
# Create an empty map canvas
newmap <- getMap(resolution = "high")

# Add the geocoded data to the map
plot(newmap, asp = 1)
points(all_places_geocoded$lon,
    all_places_geocoded$lat,
    col = "red", cex = 1.2, pch = 19
)

# Extract person names
walmart_person <- text[annot_l1[k == "person"]]
walmart_organization <- text[annot_l1[k == "organization"]]
walmart_person <- unique(walmart_person)
walmart_person <- setdiff(walmart_person, walmart_organization)
walmart_person <- setdiff(walmart_person, walmart_locations)