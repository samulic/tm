library("tm")
library("dplyr")
#library("RNewsflow") # la sua funzione delete.duplicates non funziona!
library("wordcloud2")
library("htmlwidgets")
library("webshot")
### WORD CLOUD EXPLORATION ###

folder <- "Project/" # root project folder

# load function to build different representation matrixes
load(paste0(folder, "helper_functions.RData")) 

# load training and test data 
corpus_tr <- VCorpus(DirSource(paste0(folder, "Data/20news-bydate-train/"), recursive = TRUE),
                     readerControl = list(language = "en"))
corpus_te <- VCorpus(DirSource(paste0(folder, "Data/20news-bydate-test/"), recursive = TRUE),
                     readerControl = list(language = "en"))
# merge both sets. We plot wordclouds on the whole corpus!
corpus <- c(corpus_tr, corpus_te)

corpus_preprocessed <- preprocess_dataset(corpus)

# delete duplicate, NON FUNZIONA!
# meta <-data.frame(document_id = unlist(sapply(corpus_preprocessed, 'meta', "id")),
#                   date = "2000-01-01", source = "NULLO",
#                   stringsAsFactors = F)
# m <- delete.duplicates(dtm, meta, id.var = "id", date.var = "date", source.var = "source",
#                   measure = "cosine", similarity = 1, keep = "first", tf.idf = FALSE)


# Plot/save wordclouds for different combinations of and sparsity values and
# text representations (which can be one of the following:
# binary, tf, tfidf, bigram_binary, bigram_tf and bigram_tfidf
for (wanted_matrix_type in c("tfidf", "bigram_tfidf")) {
  # for problem about memory, reduce the highest sparsity threshold
  for (wanted_sparsity_value in c(0.8, 0.99, 0.995, 0.998)) {
    corpus_matrix <- create_matrix(corpus_preprocessed, 
                                   wanted_matrix_type, 
                                   wanted_sparsity_value, verbose = T)
    v <- sort(colSums(corpus_matrix), decreasing = TRUE)
    corpus_for_wc2 <- data.frame(word = names(v), freq = v)
    # ideal size depends on the amount of words present and need to be set manually
    size <- case_when(wanted_sparsity_value <= 0.95 ~ 1,
                      wanted_sparsity_value <= 0.99 ~ 0.45,
                      wanted_sparsity_value <= 0.995 ~ 0.35,
                      TRUE ~ 0.32)
    if (startsWith(wanted_matrix_type, "bigram")) {
      size <- size * 1.15
    }
    my_graph_1 <- wordcloud2(corpus_for_wc2, color = "random-dark", size = size, # cannot show 'the'
                             shape = "circle", backgroundColor = "white")
    saveWidget(my_graph_1, "tmp.html", selfcontained = F)
    webshot("tmp.html", paste0(folder, "Plots/",  # inside "Plots" folder
                               "WordCloud-", # name of the image file
                               wanted_matrix_type,
                               "-preprocessed-",
                               wanted_sparsity_value,
                               ".png"), 
            delay = 5)
  }
}
