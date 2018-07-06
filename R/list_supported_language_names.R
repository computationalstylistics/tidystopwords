list_supported_language_names <- function() {
    load("sysdata.rda")
    supported_languages <- unique(multilingual_stoplist$language_name)
    return(supported_languages)
}
