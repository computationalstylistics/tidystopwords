list_supported_language_names <- function() {
  multilingual_stoplist <- readr::read_csv("DATA/multilingual_stoplist.csv", col_names = TRUE)
  supported_languages <- multilingual_stoplist$language_name %>% unique() %>% sort() %>% as.character()
  return(supported_languages)
}
