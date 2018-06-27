list_supported_pos <- function() {
  data(multilingual_stoplist)
  supported_languages <- unique(multilingual_stoplist$POS)
  return(supported_languages)
}
