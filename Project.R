library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf
library(wordcloud2)
library(htmlwidgets)
library(webshot)

source <- DirSource("Project/Data/20news-bydate-train/", 
                    recursive = TRUE, mode = "text")

corpus <- Corpus(source, readerControl = list(language = "en"))
corpus <- VCorpus(source)

corpus[[4]]$content


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
# (1) Remove numbers (i.e. "'70s" -> "'s")
# parameter 'ucp': characters with Unicode general category Nd (Decimal_Number)
corpus_no_numbers <- tm_map(corpus, removeNumbers, ucp = TRUE) 
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


## TEXT REPRESENTATION
# Bi-grams
BigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

corpus_bigram_tfidf <- DocumentTermMatrix(corpus_preprocessed, 
                                       control = list(tokenize = BigramTokenizer,
                                                      weighting = function(x)
                                                        weightTfIdf(x, normalize = FALSE)))

inspect(corpus_bigram_tfidf)

corpus_bigram_tfidf_sparse_removed = removeSparseTerms(corpus_bigram_tfidf, 0.99)
inspect(corpus_bigram_tfidf_sparse_removed)



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
