list_supported_language_ids <- function() {
    # load("sysdata.rda")
    data(multilingual_stoplist)
    supported_languages <- unique(multilingual_stoplist$language_id)
    return(supported_languages)
}
