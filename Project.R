library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf
library(wordcloud2)
library(htmlwidgets)
library(stringi)
library(caret)
library(dplyr)
library(data.table)

folder <- "Project/Data/"
load(paste0(folder, "dataList.RData"))
tr <- setNames(data.frame(do.call("rbind", strsplit(gsub(".txt", "", dataList_train$doc_id),
                                                    split = "/"))),
               c("Topic", "path", "id"))
tr$Topic <- as.character(tr$Topic)
te <- setNames(data.frame(do.call("rbind", strsplit(gsub(".txt", "", dataList_test$doc_id),
                                                    split = "/"))),
               c("Topic", "path", "id"))
te$Topic <- as.character(te$Topic)

create_idtopic <- function(df) {
  df$Topic_macro <- case_when(startsWith(df$Topic, "talk.politics") ~ "Politics",
                              startsWith(df$Topic, "comp") ~ "Computer",
                              startsWith(df$Topic, "sci") ~ "Science",
                              startsWith(df$Topic, "rec") ~ "Rec",
                              startsWith(df$Topic, "misc") ~ "Misc.forsale",
                              TRUE ~ "Religion")
  # merge duplicate docs under the same topic
  df_macrotopic <- group_by(df, Topic_macro, Topic, id) %>% summarise()
  return(df_macrotopic)
}
# (id, topc)
macrotopic_id_trn <- create_idtopic(tr)
macrotopic_id_tst <- create_idtopic(te)


# (id, text)
id_text <- data.frame(id = sapply(corpus, meta, "id"),
                      text = unlist(lapply(sapply(corpus, '[', "content"), paste, collapse = "\n")), 
                      stringsAsFactors = FALSE)

trn_df <- merge(macrotopic_id_trn, id_text, by = "id") %>% rename(doc_id = id) %>%
  group_by(doc_id, Topic, Topic_macro) %>% summarise(text = first(text))
tst_df <- merge(macrotopic_id_tst, id_text, by = "id") %>% rename(doc_id = id) %>%
  group_by(doc_id, Topic, Topic_macro) %>% summarise(text = first(text))

#ids <- lapply(corpus, function(x) x$meta$id)
#duplicates <- which(duplicated(unlist(lapply(ids, function(x) as.integer(x)))))

fix.contractions <- function(doc) {
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

fix.diacritics <- function(doc) {
  diacritics = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A',
                    'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E',
                    'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O',
                    'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y',
                    'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a',
                    'æ'='a', 'ç'='c', 'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i',
                    'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                    'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b',
                    'ÿ'='y')
  doc$content <- chartr(paste(names(diacritics), collapse=''), paste(diacritics, collapse=''),
                        doc$content)
  return(doc)
}

## PREPROCESSING
# Data Preprocessing
preprocess_dataset <- function(corpus) {
  #ifelse(isFALSE(source), 
  #       corpus <- VCorpus(DataframeSource(set)),
  #       corpus <- VCorpus(DataframeSource(source, readerControl = list(language = "en"))))
  # Strip white spaces at the beginning and at the end to overcome some problems
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  # User replace_contraction function from textclean package
  # parameter 'ucp': characters with Unicode general category Nd (Decimal_Number)
  corpus <- tm_map(corpus, removeNumbers, ucp = TRUE)
  corpus <- tm_map(corpus, fix.contractions)
  #corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, fix.diacritics)
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  corpus <- tm_map(corpus, stemDocument, language = "english")
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

train_dt_classifier <- function(train_df, metric, control) {
  library("C50")
  # Start timer...
  tic("Decision Tree")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="C5.0", metric=metric, trControl=control)
  # Stop timer...
  toc()
  return(model)
  detach("package:C50", unload = TRUE)
}
# Support Vector Machine
train_svm_classifier <- function(train_df, metric, control) {
  tic("SVM")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="svmRadial", metric=metric, trControl=control)
  toc()
  return(model)
}
# K-Nearest Neighbors
train_knn_classifier <- function(train_df, metric, control) {
  tic("KNN")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="knn", metric=metric, trControl=control)
  toc()
  return(model)
}
# Random Forest
train_rf_classifier <- function(train_df, metric, control) {
  tic("Random Forest")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="rf", metric=metric, trControl=control)
  toc()
  return(model)
}
# Neural Networks
train_nn_classifier <- function(train_df, metric, control) {
  tic("Neural Networks")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="nnet", metric=metric, trControl=control)
  toc()
  return(model)
}

### START
train_set <- VCorpus(DirSource(paste0(folder, "20news-bydate-train/"), recursive = TRUE),
                     readerControl = list(language = "en"))
test_set <- VCorpus(DirSource(paste0(folder, "20news-bydate-test/"), recursive = TRUE),
                    readerControl = list(language = "en"))
# Training set preprocessing
print("Training Set preprocessing...")
train_set <- preprocess_dataset(train_set)
# Test set preprocessing
print("Test Set preprocessing...")
test_set <- preprocess_dataset(test_set)

# Possible values:  binary, bigram_binary, tf, bigram_tf, tfidf, bigram_tfidf
wanted_matrix_type <- "bigram_tfidf"
wanted_sparsity_value <- 0.99
wanted_verbose <- FALSE

train_matrix <- create_matrix(train_set, 
                              wanted_matrix_type, 
                              wanted_sparsity_value, 
                              wanted_verbose)
test_matrix <- create_matrix(test_set, 
                             wanted_matrix_type, 
                             wanted_sparsity_value, 
                             wanted_verbose)

# Create intersection dataframes and label them
train_df <- find_intersection_and_create_dataframe(train_matrix, test_matrix)
test_df <- find_intersection_and_create_dataframe(test_matrix, train_matrix)

# Add target class column (adds both topic and macro_topic)
#macrotopic_id_trn$id <- paste0("X", macrotopic_id_trn$id)
#macrotopic_id_tst$id <- paste0("X", macrotopic_id_tst$id)

train_df <- left_join(train_df, macrotopic_id_trn, by = "id")
test_df <- left_join(test_df, macrotopic_id_tst, by = "id")
train_df$id <- NULL
test_df$id <- NULL

# Summarize distributions
print(summarize_distribution(train_df, macroTopic = FALSE))
print(summarize_distribution(test_df, macroTopic = TRUE))

control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"

# If we want to use macro_topic with six classes take the step below
train_df$Topic <- train_df$Topic_macro

train_df$Topic_macro <- NULL

# Classifiers
dt_model <- train_dt_classifier(train_df, metric, control)
svm_model <- train_svm_classifier(train_df, metric, control)
knn_model <- train_knn_classifier(train_df, metric, control)
rf_model <- train_rf_classifier(train_df, metric, control)
nn_model <- train_nn_classifier(train_df, metric, control)  