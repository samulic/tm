library(tm) # https://cran.r-project.org/web/packages/tm/tm.pdf

source <- DirSource("Project/Data/20news-bydate-train/", 
                    recursive = TRUE, mode = "text")

corpus <- Corpus(source, readerControl = list(language = "en"))

corpus[[2]]
