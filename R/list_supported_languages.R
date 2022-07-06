list_supported_languages <- function() {

    # a hack to silence package warnings  
    data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
    multilingual_stoplist <- get("multilingual_stoplist", envir = environment())
    form <- lang_id <- lang_name <- stopclass <- NULL 
    #
    
    supported_languages <- multilingual_stoplist %>% 
        group_by(lang_name, lang_id) %>% count(name = "stopwords") %>%
        arrange(lang_name)
    print(supported_languages, n = Inf)
    return(supported_languages)
}

