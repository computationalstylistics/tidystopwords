list_supported_languages <- function() {
  multilingual_stoplist <- data(multilingual_stoplist)
  supported_languages <- unique(multilingual_stoplist$language_id)
  return(supported_languages)
}
