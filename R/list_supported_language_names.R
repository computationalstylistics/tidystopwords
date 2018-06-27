list_supported_language_names <- function() {
  multilingual_stoplist <- data(multilingual_stoplist)
  supported_languages <- multilingual_stoplist$language_name %>% unique() %>% sort() %>% as.character()
  return(supported_languages)
}
