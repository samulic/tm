library("tm")
library("dplyr")
library("wordcloud2")
library("htmlwidgets")
library("webshot")


folder <- "Project/Data/"
corpus <- VCorpus(DirSource(paste0(folder, "20news-bydate-train/"), 
                            recursive = TRUE, mode = "text"),
                     readerControl = list(language = "en"))


fix.contractions <- function(doc) {
  gsub("^(ca|could|must|need|wo|would|should|ought|do|does|did|have|has|had)nt", "\\1n't", doc$content)
  gsub("^im$", "i'm", doc$content)
  # "won't" is a special case as it does not expand to "wo not"
  doc$content <- gsub("won't", "will not", doc$content)
  doc$content <- gsub("can't", "can not", doc$content)
  doc$content <- gsub("n't", " not", doc$content)
  doc$content <- gsub("'ll", " will", doc$content)
  doc$content <- gsub("'re", " are", doc$content)
  doc$content <- gsub("'ve", " have", doc$content)
  doc$content <- gsub("'m", " am", doc$content)
  doc$content <- gsub("'d", " would", doc$content)
  # 's could be 'is' or could be possessive: it has no expansion
  doc$content <- gsub("'s", "", doc$content)
  return(doc)
}

# PREPROCESS
corpus_no_whitespace <- tm_map(corpus, content_transformer(stripWhitespace))
corpus_no_punctuation <- tm_map(corpus_no_whitespace, content_transformer(removePunctuation))
# Normalization 3 steps
corpus_normalized <- tm_map(corpus_no_punctuation, content_transformer(tolower))
corpus_normalized <- tm_map(corpus_normalized, fix.contractions)
corpus_normalized <- tm_map(corpus_normalized, removeNumbers)
corpus_normalized <- tm_map(corpus_normalized, stripWhitespace)
# Stem after normalized & stem after no_stopwords
corpus_stemmed <- tm_map(corpus_normalized, stemDocument, language = "english")
# Remove stopwords
corpus_no_stopwords <- tm_map(corpus_stemmed, removeWords, stopwords("english"))

# Feature selection
apply_feature_selection_on_dtm <- function(dtm_fs, sparsity_value = 0.99, verbose = FALSE) {
  if (verbose) {
    print("DTM before sparse term removal")
    inspect(dtm_fs)
  }
  
  dtm_fs = removeSparseTerms(dtm_fs, sparsity_value)
  
  if (verbose) {
    print("DTM after sparse term removal")
    inspect(dtm_fs)
  }
  
  return(dtm_fs)
}

# TF-IDF matrix
create_tfidf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating tf-idf matrix...")
  }
  dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  dtm_tfidf <- apply_feature_selection_on_dtm(dtm_tfidf, sparsity_value, verbose)
  matrix_tfidf <- as.matrix(dtm_tfidf)
  return(matrix_tfidf)
}
# Bigram TF-IDF matrix
create_bigram_tfidf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating bigram tf-idf matrix...")
  }
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  
  dtm_bigram_tfidf <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  dtm_bigram_tfidf <- apply_feature_selection_on_dtm(dtm_bigram_tfidf, sparsity_value, verbose)
  matrix_bigram_tfidf <- as.matrix(dtm_bigram_tfidf)
  return(matrix_bigram_tfidf)
}

create_matrix <- function(corpus, matrix_type, sparsity_value = 0.99, verbose = NULL) {
  if (matrix_type == 'binary') {
    matrix <- create_binary_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'bigram_binary') {
    matrix <- create_bigram_binary_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'tf') {
    matrix <- create_tf_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'bigram_tf') {
    matrix <- create_bigram_tf_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'tfidf') {
    matrix <- create_tfidf_matrix(corpus, sparsity_value, verbose)
  } else if (matrix_type == 'bigram_tfidf') {
    matrix <- create_bigram_tfidf_matrix(corpus, sparsity_value, verbose)
  } else {
    print('Invalid matrix type!')
  }
  return(matrix)
}


# Save wordclouds for different combination of sparsity values
for (wanted_matrix_type in c("tfidf", "bigram_tfidf")) {
  for (wanted_sparsity_value in c(0.8, 0.99, 0.995, 0.999)) {
    corpus_matrix <- create_matrix(corpus_no_stopwords, wanted_matrix_type, wanted_sparsity_value, T)
    v <- sort(colSums(corpus_matrix), decreasing = TRUE)
    corpus_for_wc2 <- data.frame(word = names(v), freq = v)
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
    webshot("tmp.html", paste0("Plots/WordCloud1-", wanted_matrix_type,"-preprocessed-no_mispellings-",
                               wanted_sparsity_value,".png"), delay = 5)
  }
}
