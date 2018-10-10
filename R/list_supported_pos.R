list_supported_pos <- function() {
    # a hack to silence package warnings  
    data("multilingual_stoplist", package = "stopwoRds", envir = environment()) 
    multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    #
    supported_languages <- unique(multilingual_stoplist$POS)
    return(supported_languages)
  }
