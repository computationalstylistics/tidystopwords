list_supported_languages <- function() {
  multilingual_stoplist <- read.csv("DATA/multilingual_stoplist.csv")
  supported_languages <- unique(multilingual_stoplist$language) %>% levels()
  return(supported_languages)
}
