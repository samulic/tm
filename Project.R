library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf

source <- DirSource("Project/Data/20news-bydate-train/", 
                    recursive = TRUE, mode = "text")

source1 <- DirSource("Project/Data/20news-bydate-test/", 
                     recursive = TRUE, mode = "text")

corpus <- Corpus(source, readerControl = list(language = "en"))

corpus[[2]]

plainTextDocument <- corpus[[1]]
# Tokenizers
boost_tokenized_document <- Boost_tokenizer(plainTextDocument)
boost_tokenized_document
mc_tokenized_document <- MC_tokenizer(plainTextDocument)
mc_tokenized_document
scan_tokenized_document <- scan_tokenizer(plainTextDocument)
scan_tokenized_document
# custom tokenizer
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
split_space_tokenized_document <- strsplit_space_tokenizer(plainTextDocument)

# Term frequency document-term matrix
corpus_dtm <- DocumentTermMatrix(corpus)
corpus_matrix <- as.matrix(corpus_dtm)
v <- sort(colSums(corpus_matrix), decreasing = TRUE)
corpus_for_wc2 <- data.frame(word = names(v), freq = v)
