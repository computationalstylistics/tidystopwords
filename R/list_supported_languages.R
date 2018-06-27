list_supported_languages <- function() {
  data(multilingual_stoplist)
  supported_languages <- unique(multilingual_stoplist$language_id)
  return(supported_languages)
}
