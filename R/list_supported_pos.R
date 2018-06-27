list_supported_pos <- function() {
  multilingual_stoplist <- data(multilingual_stoplist)
  supported_languages <- multilingual_stoplist$POS %>% unique() %>% sort() %>% as.character()
  return(supported_languages)
}
