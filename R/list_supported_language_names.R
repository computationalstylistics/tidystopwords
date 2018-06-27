list_supported_language_names <- function() {
    data(multilingual_stoplist)
    supported_languages <- unique(multilingual_stoplist$language_name)
    return(supported_languages)
}
