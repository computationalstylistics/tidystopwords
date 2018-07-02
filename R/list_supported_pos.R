list_supported_pos <- function() {
  data(sysdata)
  supported_languages <- unique(sysdata$POS)
  return(supported_languages)
}
