list_supported_language_ids <- function() {
  multilingual_stoplist <- data(multilingual_stoplist)
  supported_languages <- multilingual_stoplist$language_id %>% unique() %>% sort() %>% as.character()
  return(supported_languages)
}
