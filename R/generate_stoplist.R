


generate_stoplist <- function(lang, pos = c("ADP", "AUX", "DET", "INTJ", "PART", "PRON", "SCONJ")) {
  if (require("dplyr") == FALSE) {
    library(dplyr)
  }
  stopifnot(require("dplyr"))
  multilingual_stoplist <- read.csv("DATA/multilingual_stoplist.csv")
  languages <- unique(multilingual_stoplist$language) %>% sort()
  all_pos <- c("ADP", "AUX", "DET", "INTJ", "PART", "PRON", "SCONJ")
 if (tolower(lang) %in% c("en", "english" )) {
    language <- "en"
  } else if (tolower(lang) %in% c("cs", "cz", "czech", "check")) {
    language <- "cs"
  } else if (tolower(lang) %in% c("la", "lat", "latin", "latine", "latino")){ 
    language <- "la"
  } else if (tolower(lang) %in% c("pl", "polish", "polisch")) {
    language <- "pl"
  } else if (tolower(lang) %in% c("es", "spanish", "espanish", "espaniol", "espagnol")) {
    language <- "es"
  } else if (!(tolower(lang) %in% multilingual_stoplist$language)){
    cat("This language is not available. Currently supported languages: ",
        "\n", languages, "\n")
    stop()
  }
  difpos <- setdiff(pos, all_pos)
  if (length(difpos) > 0) {
    
   cat(difpos, " not among available parts of speech. Currently supported parts of speech: ",
   "\n", all_pos, "\n")
   stop()
 }
 stopwords_df <- dplyr::filter(multilingual_stoplist, POS %in% pos & language == lang) 
 stopwords <- stopwords_df$enc_wforms %>% unique() %>% as.character()  
 return(stopwords)   

}







