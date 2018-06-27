list_supported_language_ids <- function() {
  multilingual_stoplist <- readr::read_csv("DATA/multilingual_stoplist.csv", col_names = TRUE)
  supported_languages <- multilingual_stoplist$language_id %>% unique() %>% sort() %>% as.character()
  return(supported_languages)
}
