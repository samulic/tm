library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf
library(stringi)
library(caret)
library(dplyr)
library(tictoc)
library(data.table)

folder <- "Data/"
load(paste0(folder, "dataList.RData"))
# Read corpus from filesystem
corpus_tr <- VCorpus(DirSource(paste0(folder, "20news-bydate-train/"), recursive = TRUE),
                     readerControl = list(language = "en"))
corpus_te <- VCorpus(DirSource(paste0(folder, "20news-bydate-test/"), recursive = TRUE),
                    readerControl = list(language = "en"))

## Create TARGET class function
create_idtopic <- function(df) {
  df$Topic_macro <- case_when(startsWith(df$Topic, "talk.politics") ~ "Politics",
                              startsWith(df$Topic, "comp") ~ "Computer",
                              startsWith(df$Topic, "sci") ~ "Science",
                              startsWith(df$Topic, "rec") ~ "Rec",
                              startsWith(df$Topic, "misc") ~ "Misc.forsale",
                              TRUE ~ "Religion") # altrimenti in tutti gli altri casi ~ 'religione'
  # Important !
  # merge duplicate docs under the same topic
  df_macrotopic <- group_by(df, Topic_macro, Topic, id) %>% summarise()
  return(df_macrotopic)
}
create_idtopic <- function(df) {
  df$Topic_macro <- case_when(startsWith(df$Topic, "rec") ~ "Rec",
                              TRUE ~ "NoRec") # altrimenti in tutti gli altri casi 
  # Important !
  # merge duplicate docs under the same topic
  df_macrotopic <- group_by(df, Topic_macro, Topic, id) %>% summarise()
  return(df_macrotopic)
}

# PRE_PREPROCESS dataframes
tr <- setNames(data.frame(do.call("rbind", strsplit(gsub(".txt", "", dataList_train$doc_id),
                                                    split = "/"))),
               c("Topic", "path", "id"))
te <- setNames(data.frame(do.call("rbind", strsplit(gsub(".txt", "", dataList_test$doc_id),
                                                    split = "/"))),
               c("Topic", "path", "id"))
tr$Topic <- as.character(tr$Topic)
te$Topic <- as.character(te$Topic)

# (id, topc)
macrotopic_id_trn <- create_idtopic(tr)
macrotopic_id_tst <- create_idtopic(te)


# (id, text) # from filesystem's corpora
id_text_tr <- data.frame(id = sapply(corpus_tr, meta, "id"),
                      text = unlist(lapply(
                        sapply(corpus_tr, '[', "content"), paste, collapse = "\n")), 
                      stringsAsFactors = FALSE)
id_text_te <- data.frame(id = sapply(corpus_te, meta, "id"),
                         text = unlist(lapply(
                           sapply(corpus_te, '[', "content"), paste, collapse = "\n")), 
                         stringsAsFactors = FALSE)

## JOIN (id,topic) with docs
trn_df <- merge(macrotopic_id_trn, id_text_tr, by = "id") %>% rename(doc_id = id) %>%
  group_by(doc_id, Topic, Topic_macro) %>% summarise(text = first(text))
tst_df <- merge(macrotopic_id_tst, id_text_te, by = "id") %>% rename(doc_id = id) %>%
  group_by(doc_id, Topic, Topic_macro) %>% summarise(text = first(text))

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

RemoveEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}

## PREPROCESSING
# Data Preprocessing
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
train_nn_classifier <- function(train_df, metric, control) {results_bigramTfIdf_0.99sparsity
  tic("Neural Networks")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="nnet", metric=metric, trControl=control)
  toc()
  return(model)
}

### START HERE ! ###
# Training set preprocessing
print("Training Set preprocessing...")
train_set <- preprocess_dataset(corpus_tr)
# Test set preprocessing
print("Test Set preprocessing...")
test_set <- preprocess_dataset(corpus_te)

# Possible values:  binary, bigram_binary, tf, bigram_tf, tfidf, bigram_tfidf
wanted_matrix_type <- "tfidf"
wanted_sparsity_value <- 0.95
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
# Change id to be equal to rownames of the df, which are the ids
#macrotopic_id_trn$id <- paste0("X", macrotopic_id_trn$id)
#macrotopic_id_tst$id <- paste0("X", macrotopic_id_tst$id)
# Remove duplicates in wider macro group (that now have the same macro_topic)
macrotopic_id_trn <- group_by(macrotopic_id_trn, id, Topic_macro) %>% summarise()
macrotopic_id_tst <- group_by(macrotopic_id_tst, id, Topic_macro) %>% summarise()
# Duplicati che stanno in piu' macroclassi contemporaneamente sono da rimuovere
# Individua duplicati
dup_tr <- group_by(macrotopic_id_trn, id) %>% filter(n() > 1)
dup_te <- group_by(macrotopic_id_tst, id) %>% filter(n() > 1)
# Rimuovi duplicati
macrotopic_trn <- macrotopic_id_trn[-which(macrotopic_id_trn$id %in% dup_tr$id),]
macrotopic_tst <- macrotopic_id_tst[-which(macrotopic_id_tst$id %in% dup_te$id),]

# Add target class column (adds both topic and macro_topic)
train_df <- merge(train_df, macrotopic_trn, by = "id")
test_df <- merge(test_df, macrotopic_tst, by = "id")
# Individua duplicati
dup_tr_df <- group_by(train_df, id, Topic_macro) %>% filter(n() > 1)
dup_te_df <- group_by(test_df, id, Topic_macro) %>% filter(n() > 1)
# Rimuovili
train_df <- train_df[-which(train_df$id %in% dup_tr_df$id),]
test_df  <- test_df[-which(test_df$id %in% dup_te_df$id),]
# Any null?
any(is.na(train_df$Topic_macro))
any(is.na(test_df$Topic_macro))
# Remove id, not needed anymore
train_df$id <- NULL
test_df$id <- NULL

# Summarize target class distributions
print(summarize_distribution(train_df, macroTopic = F))
print(summarize_distribution(test_df, macroTopic = F))
# If we want to use macro_topic with six classes take the step below
train_df$Topic <- train_df$Topic_macro
test_df$Topic  <- test_df$Topic_macro

# At this point we have collinear Topic column at different level of granularity
# as a consequence we also have duplicates
# so we need to group by id and desired column of Topic which will
#n_occur_tr <- data.frame(table(train_df$id))
#n_occur_te <- data.frame(table(test_df$id))
#duplicated_tr <- train_df[train_df$id %in% n_occur_tr$Var1[n_occur_tr$Freq > 1],
#                          c("Topic_macro", "id")]
#duplicated_te <- test_df[test_df$id %in% n_occur_te$Var1[n_occur_te$Freq > 1],
#                          c("Topic_macro", "id")]

train_df$Topic_macro <- NULL
test_df$Topic_macro  <- NULL

# CARET: train control parameters and desired performance metric
control <- trainControl(method = "cv", number = 5, classProbs = T)
metric <- "ROC"

# Classifiers
dt_model <- train_dt_classifier(train_df, metric, control)
svm_model <- train_svm_classifier(train_df, metric, control)
rf_model <- train_rf_classifier(train_df, metric, control)
nn_model <- train_nn_classifier(train_df, metric, control)  
knn_model <- train_knn_classifier(train_df, metric, control)

results <- resamples(list(DecisionTree = dt_model, 
                          RandomForest = rf_model, 
                          SVM = svm_model, 
                          KNN = knn_model,
                          NeuralNetwork = nn_model))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

# Test predictions
dt_predictions <- predict(dt_model, newdata = test_df)
dt_confusion_matrix <- confusionMatrix(table(dt_predictions, test_df$Topic))
cat('Decision Tree test accuracy: ', unname(dt_confusion_matrix$overall[1]), '\n')

svm_predictions <- predict(svm_model, newdata = test_df)
svm_confusion_matrix <- confusionMatrix(table(svm_predictions, test_df$Topic))
cat('SVM test accuracy: ', unname(svm_confusion_matrix$overall[1]), '\n')

knn_predictions <- predict(knn_model, newdata = test_df)
knn_confusion_matrix <- confusionMatrix(table(knn_predictions, test_df$Topic))
cat('KNN test accuracy: ', unname(knn_confusion_matrix$overall[1]), '\n')

rf_predictions <- predict(rf_model, newdata = test_df)
rf_confusion_matrix <- confusionMatrix(table(rf_predictions, test_df$Topic))
cat('Random Forest test accuracy: ', unname(rf_confusion_matrix$overall[1]), '\n')

nn_predictions <- predict(nn_model, newdata = test_df)
nn_confusion_matrix <- confusionMatrix(table(nn_predictions, test_df$Topic))
cat('Neural Networks test accuracy: ', unname(nn_confusion_matrix$overall[1]), '\n')


#results_bigramTfIdf_0.99sparsity <- results
save(list = c("results"),
     file = paste0(folder, "results_1.RData"))
                                                                
