library("tm")
library("dplyr")
# This file contains useful functions to build different text 
# representation matrices and preprocess the corpus
folder <- "Project/" # Where to save the RData object containing all functions

# Helper function for ad-hoc preprocessing
RemoveEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}
# Helper function for normalization
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

# Preprocessing
preprocess_dataset <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  corpus <- tm_map(corpus, content_transformer(RemoveEmail))
  corpus <- tm_map(corpus, removeNumbers)
  # Normalization
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Expand contractions --> la funzione vede solo lowercase
  corpus <- tm_map(corpus, fix.contractions)
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  # Stemming
  corpus <- tm_map(corpus, stemDocument, language = "english")
  # Stopwords removal
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  return(corpus)
}
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

# Binary matrix
create_binary_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating binary matrix...")
  }
  dtm_binary <- DocumentTermMatrix(corpus, control = list(weighting = weightBin))
  dtm_binary <- apply_feature_selection_on_dtm(dtm_binary, sparsity_value, verbose)
  matrix_binary <- as.matrix(dtm_binary)
  return(matrix_binary)
}
# Bigram binary matrix
create_bigram_binary_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating bigram binary matrix...")
  }
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  dtm_bigram_binary <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer, weighting = weightBin))
  dtm_bigram_binary <- apply_feature_selection_on_dtm(dtm_bigram_binary, sparsity_value, verbose)
  matrix_bigram_binary <- as.matrix(dtm_bigram_binary)
  return(matrix_bigram_binary)
}
# TF matrix
create_tf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating tf matrix...")
  }
  dtm_tf <- DocumentTermMatrix(corpus)
  dtm_tf <- apply_feature_selection_on_dtm(dtm_tf, sparsity_value, verbose)
  matrix_tf <- as.matrix(dtm_tf)
  return(matrix_tf)
}
# Bigram TF matrix
create_bigram_tf_matrix <- function(corpus, sparsity_value, verbose) {
  if (verbose) {
    print("Creating bigram tf matrix...")
  }
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  dtm_bigram_tf <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
  dtm_bigram_tf <- apply_feature_selection_on_dtm(dtm_bigram_tf, sparsity_value, verbose)
  matrix_bigram_tf <- as.matrix(dtm_bigram_tf)
  return(matrix_bigram_tf)
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

find_intersection_and_create_dataframe <- function(matrix_1, matrix_2) {
  
  intersection_matrix <- data.frame(matrix_1[, intersect(colnames(matrix_1), 
                                                         colnames(matrix_2))])
  intersection_matrix$id <- rownames(matrix_1)
  return(intersection_matrix)
}

summarize_distribution <- function(df, macroTopic = FALSE) {
  if (macroTopic) {
    df_percentage <- prop.table(table(df$Topic_macro)) * 100
    distribution_summary <- cbind(freq = table(df$Topic_macro), df_percentage)
  } 
  if (! macroTopic) {
    df_percentage <- prop.table(table(df$Topic)) * 100
    distribution_summary <- cbind(freq = table(df$Topic), df_percentage)
  }
  
  return(distribution_summary)
}


# Save functions for reuse
objs_to_save <- c("apply_feature_selection_on_dtm", "create_bigram_binary_matrix",
                 "create_bigram_tf_matrix", "create_bigram_tfidf_matrix", 
                 "create_binary_matrix", "create_tf_matrix", "create_tfidf_matrix",
                 "create_matrix", "find_intersection_and_create_dataframe", 
                 "fix.contractions", "RemoveEmail", "preprocess_dataset",
                 "summarize_distribution")
save(list = objs_to_save, file = paste0(folder, "helper_functions.RData"))
  