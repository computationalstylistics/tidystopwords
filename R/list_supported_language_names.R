list_supported_language_names <- function() {
    # load("sysdata.rda")
    data(multilingual_stoplist)
    supported_languages <- unique(multilingual_stoplist$language_name)
    return(supported_languages)
}
