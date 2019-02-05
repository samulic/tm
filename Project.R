library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(caret)

load("Project/Data/dataList.RData")
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
  df_macrotopic <- group_by(df, Topic_macro, id) %>% summarise()
  return(df_macrotopic)
}
# (id, topc)
macrotopic_id_trn <- create_idtopic(dataList_train)
macrotopic_id_tst <- create_idtopic(dataList_test)
# (id, text)
id_text <- data.frame(id = sapply(corpus, meta, "id"),
             text = unlist(lapply(sapply(corpus, '[', "content"), paste, collapse = "\n")),
             stringsAsFactors = FALSE)

trn_df <- semi_join(macrotopic_id_trn, id_text, by = "id")
left_joi
tst_df <- dataList_test

source <- DirSource("Project/Data/20news-bydate-train/", 
                    recursive = TRUE, mode = "text")

corpus <- VCorpus(source, readerControl = list(language = "en"))

labels <- read.csv("Project/Data/corpus_train_dir.csv", header = FALSE)
labels <- labels[1:dim(labels)[1] - 1, ]

meta(corpus, "label") <- labels$V1

ids <- lapply(corpus, function(x) x$meta$id)
duplicates <- which(duplicated(unlist(lapply(ids, function(x) as.integer(x)))))

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
preprocess_dataset <- function(set) {
  corpus <- VCorpus(DataframeSource(set))
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
# (1) Remove numbers (i.e. "'70s" -> "'s")
# (2) Remove English stopwords
corpus_stopwords_removed <- tm_map(corpus_no_numbers, removeWords, stopwords("english"))
# (3) Mutate English contractions
corpus_contraction_fixed <- tm_map(corpus_stopwords_removed, fix.contractions)
# (4) Mutate diacritics 
# - how is performance if we don't take this step? English shouldn't suffer from this
corpus_diacritics_fixed <- corpus_contraction_fixed
#corpus_diacritics_fixed <- tm_map(corpus_contraction_fixed, fix.diacritics)
corpus_no_punctuation <- tm_map(corpus_diacritics_fixed, removePunctuation)
# (5) To lowercase
corpus_normalized <- tm_map(corpus_no_punctuation, content_transformer(tolower))
# (6) Strip whitespaces
corpus_normalized <- tm_map(corpus_normalized, stripWhitespace)

## STEMMING
corpus_stemmed <- tm_map(corpus_normalized, stemDocument, language = "english")
corpus_preprocessed <- tm_map(corpus_stemmed, stripWhitespace)


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
  return(intersection_matrix)
}

summarize_distribution <- function(df) {
  df_percentage <- prop.table(table(df$Topic)) * 100
  distribution_summary <- cbind(freq = table(df$Topic), df_percentage)
  return(distribution_summary)
}

## TEXT REPRESENTATION
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

corpus_bigram_tfidf_sparse_removed = removeSparseTerms(corpus_bigram_tfidf, 0.99)
inspect(corpus_bigram_tfidf_sparse_removed)


wanted_matrix_type <- "bigram_tfidf"
wanted_sparsity_value <- 0.99
wanted_verbose <- FALSE

trn_set <- corpus_bigram_tfidf_sparse_removed
#trn_set <- preprocess_dataset(trn_set)
tst_set <- preprocess_dataset(tst_set) # doc.id | doc.text

trn_matrix <- create_matrix(trn_set, wanted_matrix_type, wanted_sparsity_value, 
                            wanted_verbose)
tst_matrix <- create_matrix(tst_set, wanted_matrix_type, wanted_sparsity_value, 
                            wanted_verbose)

trn_df <- find_intersection_and_create_dataframe(trn_matrix, tst_matrix)
tst_df <- find_intersection_and_create_dataframe(tst_matrix, trn_matrix)
# Label documents
trn_df <- cbind(trn_df, trn_labels) #class named Topic
tst_df <- cbind(tst_df, tst_labels)

print(summarize_distribution(trn_df))
print(summarize_distribution(tst_df))


## CLASSIFICATION
# Decision trees
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

control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"

dt_model <- train_dt_classifier(trn_df, metric, control)


## Word cloud
corpus_matrix <- as.matrix(corpus_bigram_tfidf_sparse_removed)
v <- sort(colSums(corpus_matrix), decreasing = TRUE)
corpus_for_wc2 <- data.frame(word=names(v), freq = v)
bigram_tf_matrix_wc <- wordcloud2(corpus_for_wc2, fontFamily="Helvetica", fontWeight="100", color="random-dark", size=0.4, shape="circle", backgroundColor="white")
saveWidget(bigram_tf_matrix_wc, "tmp.html", selfcontained = F)
webshot("tmp.html", "corpus_bigram_tfidf_sparse_removed.png", delay = 5)



##TODO - from here on
corpus_bigram_tf <- DocumentTermMatrix(corpus_preprocessed, 
                                       control = list(tokenize = BigramTokenizer))



# Tokenize
boost_tokenized_corpus <- tm_map(corpus, Boost_tokenizer)
mc_tokenized_corpus <- lapply(corpus, MC_tokenizer)
scan_tokenized_corpus <- lapply(corpus, scan_tokenizer)
split_space_tokenizer <- function(x) unlist(strsplit(as.character(x), "[[:space:]]+"))
split_space_tokenized_corpus <- lapply(corpus, split_space_tokenizer)
# Choose tokenizer
tokenized_list <- boost_tokenized_corpus



# Term frequency document-term matrix
corpus_dtm <- DocumentTermMatrix(corpus)
corpus_matrix <- as.matrix(corpus_dtm)
v <- sort(colSums(corpus_matrix), decreasing = TRUE)
corpus_for_wc2 <- data.frame(word = names(v), freq = v)
