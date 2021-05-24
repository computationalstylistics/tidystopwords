list_supported_language_names <- function() {
    # a hack to silence package warnings  
   # data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
#    multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    #
    supported_languages <- unique(multilingual_stoplist$language_name) 
    supported_languages <- sort(supported_languages)
    return(supported_languages)
}
