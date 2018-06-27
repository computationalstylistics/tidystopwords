list_supported_pos <- function() {
  multilingual_stoplist <- read.csv("DATA/multilingual_stoplist.csv")
  supported_languages <- multilingual_stoplist$POS %>% unique() %>% sort() %>% as.character()
  return(supported_languages)
}
