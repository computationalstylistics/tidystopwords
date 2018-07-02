list_supported_pos <- function() {
  multilingual_stoplist <- data(sysdata)
  supported_languages <- unique(multilingual_stoplist$POS)
  return(supported_languages)
}
