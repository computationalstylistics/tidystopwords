list_supported_pos <- function() {
  multilingual_stoplist <- read.csv("DATA/multilingual_stoplist.csv")
  supported_languages <- unique(multilingual_stoplist$POS) %>% levels()
  return(supported_languages)
}
