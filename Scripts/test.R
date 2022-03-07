library(udpipe)
library(lattice)
x <- read.csv("./Data/baur_au_lac_reviews.csv")
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = x$content)
x <- as.data.frame(x)
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq,
    data = head(stats, 20), col = "cadetblue",
    main = "Most occurring adjectives", xlab = "Freq"
)
print(barchart)


x <- udpipe_annotate(ud_model, x = c("hi", "shit", "very", "good", "bad", "great", "bad", "good", "very", "shit"))
x <- as.data.frame(x)
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq,
    data = head(stats, 20), col = "cadetblue",
    main = "Most occurring adjectives", xlab = "Freq"
)
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)
library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "Words following one another", subtitle = "Nouns & Adjective")