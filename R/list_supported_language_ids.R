list_supported_language_ids <- function() {
    load("sysdata.rda")
    supported_languages <- unique(multilingual_stoplist$language_id)
    return(supported_languages)
}
