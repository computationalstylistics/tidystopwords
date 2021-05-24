list_supported_pos <- function() {
    # a hack to silence package warnings  
  #  data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
   # multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    #
    supported_pos <- unique(multilingual_stoplist$POS)
    supported_pos <- sort(supported_pos)
    return(supported_pos)
  }
