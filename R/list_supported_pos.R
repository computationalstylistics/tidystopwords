list_supported_pos <- function() {
    # load("sysdata.rda")
    data(multilingual_stoplist)
    supported_languages <- unique(multilingual_stoplist$POS)
    return(supported_languages)

  }
