library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf
library(stringi)
library(dplyr)
library(data.table)


folder <- "Project/"
source(paste0(folder, "0_helperFunctions.R")) # load functions
load(paste0(folder, "Data/dataList.RData"))
# Read corpus from filesystem
corpus_tr <- VCorpus(DirSource(paste0(folder, "Data/20news-bydate-train/"), recursive = TRUE),
                     readerControl = list(language = "en"))
corpus_te <- VCorpus(DirSource(paste0(folder, "Data/20news-bydate-test/"), recursive = TRUE),
                    readerControl = list(language = "en"))

## Create TARGET class function
# create_idtopic <- function(df) {
#   df$Topic_macro <- case_when(startsWith(df$Topic, "talk.politics") ~ "Politics",
#                               startsWith(df$Topic, "comp") ~ "Computer",
#                               startsWith(df$Topic, "sci") ~ "Science",
#                               startsWith(df$Topic, "rec") ~ "Rec",
#                               startsWith(df$Topic, "misc") ~ "Misc.forsale",
#                               TRUE ~ "Religion") # altrimenti in tutti gli altri casi ~ 'religione'
#   df_macrotopic <- group_by(df, Topic_macro, Topic, id) %>% summarise()
#   return(df_macrotopic)
# }
create_idtopic <- function(df) {
  df$Topic_macro <- case_when(startsWith(df$Topic, "sci") ~ "Sci",
                              TRUE ~ "NoSci") # altrimenti in tutti gli altri casi -> "NoScience"
  # Important !
  # merge duplicate docs under the same topic
  df_macrotopic <- group_by(df, Topic_macro, Topic, id) %>% summarise()
  return(df)
}

# PRE_PREPROCESS dataframes
tr <- setNames(data.frame(do.call("rbind", strsplit(dataList_train$doc_id, split = "/"))),
               c("Topic", "localdir", "id"))
te <- setNames(data.frame(do.call("rbind", strsplit(dataList_test$doc_id, split = "/"))),
               c("Topic", "localdir", "id"))
tr$Topic <- as.character(tr$Topic)
te$Topic <- as.character(te$Topic)

# (id, topic)
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

## START ##
# Training set preprocessing
print("Training Set preprocessing...")
train_set <- preprocess_dataset(corpus_tr)
# Test set preprocessing
print("Test Set preprocessing...")
test_set <- preprocess_dataset(corpus_te)

# Choose text representation matrix and sparsity threshold
# Possible values:  binary, bigram_binary, tf, bigram_tf, tfidf, bigram_tfidf
wanted_matrix_type <- "tfidf"
wanted_sparsity_value <- 0.99

train_matrix <- create_matrix(train_set, 
                              wanted_matrix_type, 
                              wanted_sparsity_value, 
                              wanted_verbose = F)
test_matrix <- create_matrix(test_set, 
                             wanted_matrix_type, 
                             wanted_sparsity_value, 
                             wanted_verbose = T)

# Create intersection dataframes
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
print(c("Missing topics? :", any(is.na(train_df$Topic_macro)) ||
        any(is.na(test_df$Topic_macro))))
# Remove id, not needed anymore
train_df$id <- NULL
test_df$id <- NULL

# Summarize target class distributions
print(summarize_distribution(train_df, macroTopic = F))
print(summarize_distribution(test_df, macroTopic = F))
# If we want to use macro_topic, overwrite in Topic which is target class name
train_df$Topic <- train_df$Topic_macro
test_df$Topic  <- test_df$Topic_macro

train_df$Topic_macro <- NULL
test_df$Topic_macro  <- NULL

text_representation <- wanted_matrix_type
sparsity <- wanted_sparsity_value 

save(list = c("train_df", "test_df", "text_representation", "sparsity"), 
     file = paste0(folder, "Data/preprocessed.RData"))
