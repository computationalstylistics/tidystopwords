list_supported_pos <- function() {
  data(sysdata)
  supported_languages <- unique(multilingual_stoplist$POS)
  return(supported_languages)
}
