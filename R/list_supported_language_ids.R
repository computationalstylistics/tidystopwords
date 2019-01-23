list_supported_language_ids <- function() {
    # a hack to silence package warnings  
    data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
    multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    #
    supported_languages <- unique(multilingual_stoplist$language_id)
    return(supported_languages)
}
