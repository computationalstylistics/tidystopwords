generate_stoplist <- function(lang_name = c("Afrikaans", "Ancient_Greek", "Arabic", "Basque","Bulgarian", "Buryat", "Catalan", 
                                                "Chinese", "Coptic", "Croatian", "Czech", "Danish", "Dutch", "English", "Estonian", 
                                                "Finnish", "French", "Galician", "German", "Gothic", "Greek", "Hebrew", "Hindi", 
                                                "Hungarian", "Indonesian", "Irish", "Italian", "Japanese", "Kazakh", "Korean", 
                                                "Kurmanji", "Latin", "Latvian", "North_Sami", "Norwegian", "Old_Church_Slavonic",
                                                "Persian", "Polish" ,"Portuguese", "Romanian", "Russian", "Serbian", "Slovak",
                                                "Slovenian", "Spanish", "Swedish", "Tamil", "Turkish", "Ukrainian", "Upper_Sorbian",
                                                "Urdu", "Uyghur", "Vietnamese"), 
                              lang_id = NULL,
                              output_form = "vector",
                              stop_lemmas = NULL,
                              stop_forms = NULL,
                              stop_foreign_words = TRUE, 
                              stop_abbreviations = TRUE,
                              stop_pronominals = TRUE,
                              stop_determiners_quantifiers = TRUE,
                              stop_conjuctions = TRUE, 
                              stop_adpositions = TRUE,
                              stop_subordinating_conjunctions = TRUE,
                              stop_auxiliary_verbs = TRUE,
                              stop_interjections = TRUE,
                              stop_particles = TRUE,
                              stop_numerals = TRUE,
                              stop_symbols_crosslingual = TRUE,
                              stop_punctuation_crosslingual = TRUE,
                              custom_filter = NULL
                              
                              )  {

                                  

    # a hack to silence package warnings  
    data("multilingual_stoplist", package = "stopwoRds", envir = environment()) 
    multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    #
    language_name = multilingual_stoplist$language_name
    language_id = multilingual_stoplist$language_id
    lemma = multilingual_stoplist$lemma
    word_form = multilingual_stoplist$word_form
    UFeat = multilingual_stoplist$UFeat
    POS = multilingual_stoplist$POS
    
    
    


##############
# Control of user language selection by language name.

# the vector of language names is to be updated manually with every new version of the multilingual_stoplist.csv file
 if (!isTRUE(all.equal(lang_name, c("Afrikaans", "Ancient_Greek", "Arabic", "Basque","Bulgarian", "Buryat", "Catalan", 
                                   "Chinese", "Coptic", "Croatian", "Czech", "Danish", "Dutch", "English", "Estonian", 
                                   "Finnish", "French", "Galician", "German", "Gothic", "Greek", "Hebrew", "Hindi", 
                                   "Hungarian", "Indonesian", "Irish", "Italian", "Japanese", "Kazakh", "Korean", 
                                   "Kurmanji", "Latin", "Latvian", "North_Sami", "Norwegian", "Old_Church_Slavonic",
                                   "Persian", "Polish" ,"Portuguese", "Romanian", "Russian", "Serbian", "Slovak",
                                   "Slovenian", "Spanish", "Swedish", "Tamil", "Turkish", "Ukrainian", "Upper_Sorbian",
                                   "Urdu", "Uyghur", "Vietnamese"
 )))){unsupported_language_names <- character()
   for (i in 1:length(lang_name)){
   if (!lang_name[i] %in% list_supported_language_names()){ 
     unsupported_language_names <- c(unsupported_language_names, lang_name[i])
     # Even if we do not manually update the vector of supported languages of the lang_name parameter, 
     # missing languages will come through provided they occur in the current multilingual_stoplist.csv file. 
   }}
  if (length(unsupported_language_names) > 0){
    print(unsupported_language_names)      
    stop("Remove the item(s) listed above from lang_name. \n To check out the supported languages, call `list_supported_language_names()`.\n", call. = FALSE )
  }   
   
 } 

  
# Selection in both lang_name and lang_id triggers a warning.  
    if (!is.null(lang_id)) {
      # if (!lang_id %in% list_supported_language_ids()){
      #   stop("Your lang_id *", lang_id, "* is not among supported language ids.
      #   Call `list_supported_language_ids()`.\n", call. = FALSE )
      # }
      unsupported_language_ids <- character()
      for (i in 1:length(lang_id)){
        if (!lang_id[i] %in% list_supported_language_ids()){ 
          unsupported_language_ids <- c(unsupported_language_ids, lang_id[i])
          # Even if we do not manually update the vector of supported languages of the lang_name parameter, 
          # missing languages will come through provided they occur in the current multilingual_stoplist.csv file. 
        }}
      if (length(unsupported_language_ids) > 0){
        print(unsupported_language_ids)      
        stop("Remove the item(s) listed above from lang_id. \n To check out the supported language_ids, call `list_supported_language_ids()`.\n", call. = FALSE )
      }   
      
      
      if (!isTRUE(all.equal(lang_name, c("Afrikaans", "Ancient_Greek", "Arabic", "Basque","Bulgarian",
       "Buryat", "Catalan",
                       "Chinese", "Coptic", "Croatian", "Czech", "Danish", "Dutch", "English", "Estonian",
                       "Finnish", "French", "Galician", "German", "Gothic", "Greek", "Hebrew", "Hindi",
                       "Hungarian", "Indonesian", "Irish", "Italian", "Japanese", "Kazakh", "Korean",
                       "Kurmanji", "Latin", "Latvian", "North_Sami", "Norwegian", "Old_Church_Slavonic",
                       "Persian", "Polish" ,"Portuguese", "Romanian", "Russian", "Serbian", "Slovak",
                       "Slovenian", "Spanish", "Swedish", "Tamil", "Turkish", "Ukrainian", "Upper_Sorbian",
                       "Urdu", "Uyghur", "Vietnamese")))) {
      warning("HEADS UP! Language selection by_name as well as by lang_id. \n You may want to check your selection.", call. = FALSE)
      }}
  if (is.null(lang_id) &
              isTRUE(all.equal(lang_name, c("Afrikaans", "Ancient_Greek", "Arabic",
              "Basque","Bulgarian", "Buryat", "Catalan",
                                              "Chinese", "Coptic", "Croatian",
                                               "Czech", "Danish", "Dutch", "English", "Estonian",
                                              "Finnish", "French", "Galician", "German",
                                              "Gothic", "Greek", "Hebrew", "Hindi",
                                              "Hungarian", "Indonesian", "Irish", "Italian",
                                              "Japanese", "Kazakh", "Korean",
                                              "Kurmanji", "Latin", "Latvian", "North_Sami",
                                              "Norwegian", "Old_Church_Slavonic",
                                              "Persian", "Polish" ,"Portuguese", "Romanian",
                                              "Russian", "Serbian", "Slovak",
                                              "Slovenian", "Spanish", "Swedish", "Tamil",
                                              "Turkish", "Ukrainian", "Upper_Sorbian",
                                              "Urdu", "Uyghur", "Vietnamese")))){
                warning("HEADS UP! Selection includes all supported languages. \n  You may want to check your selection.", call. = FALSE)
              }
##################################################################################################################################  

  # LINGUISTIC FILTERS  
  
  # Each linguistic filter will filter rows from multilingual_stoplist into a subdataframe. 
  # A new empty dataframe (called stoplist) will rowbind subdataframes step by step. 
  # The final stoplist will be generated from wordforms of the stoplist data frame after all. 
  stoplist_db <- matrix(nrow = 0, ncol = ncol(multilingual_stoplist)) %>% as.data.frame(stringsAsFactors = FALSE) 
  colnames(stoplist_db) = colnames(multilingual_stoplist)
 
 
   ##stop_lemmas: a character vector or a single string from user
  if (!is.null(stop_lemmas)) {
    ling_filter_db  <- dplyr::filter(multilingual_stoplist,
                                     (language_name %in%  lang_name  | language_id %in% lang_id)
                                     & tolower(lemma) %in% tolower(stop_lemmas)
                                       )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
  }

  ##stop_forms: a character vector or a single string from user
  if (!is.null(stop_forms)) {
    ling_filter_db  <- dplyr::filter(multilingual_stoplist,
                                     (language_name %in% lang_name | language_id %in% lang_id)
                                     & tolower(word_form) %in% tolower(stop_forms)
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
  }

  ## stop_foreign_words
  if ((stop_foreign_words)) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) &
                                    stringr::str_detect(string = UFeat, pattern = "Foreign=Yes")
                                    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }

 ## stop_abbreviations
  if ((stop_abbreviations)) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                    stringr::str_detect(string = UFeat, pattern = "Abbr=Yes")
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  ## stop_numerals
  if (stop_numerals) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      stringr::str_detect(string = UFeat, 
                                                          pattern = "NumType") &
                                      (POS %in% c("ADV", "DET") == FALSE)
                                     
   
     )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
 
  ## stop_determiners_quantifiers
  if(stop_determiners_quantifiers){
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      (POS == "DET" | (
                                       stringr::str_detect(string = UFeat, 
                                                            pattern = "NumType") 
                                        & stringr::str_detect(string = UFeat, pattern = "PronType")
                                        & (stringr::str_detect(string = UFeat, pattern = "Ind")
                                            | stringr::str_detect(string = UFeat, pattern = "Dem")
                                            | stringr::str_detect(string = UFeat, pattern = "Int")
                                            | stringr::str_detect(string = UFeat, pattern = "Rel")
                                           | stringr::str_detect(string = UFeat, pattern = "Tot")
                                           | stringr::str_detect(string = UFeat, pattern = "Neg"))
                                      )))
                                        
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  ## stop_conjuctions 
  if (stop_conjuctions) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                    POS == "CCONJ"
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  # stop_adpositions
  if (stop_adpositions) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "ADP"
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  
  
  # stop_subordinating_conjunctions
  if (stop_subordinating_conjunctions) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "SCONJ"
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
 
  #stop_auxiliary_verbs
  if (stop_auxiliary_verbs) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "AUX"
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  # stop_interjections 
  if (stop_interjections) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "INTJ"
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
 
  #stop_particles
  if (stop_particles) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "PART"
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  
  
   # stop_pronominals
  if (stop_pronominals) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist,
                                    (language_name %in% lang_name | language_id %in% lang_id) &
                                      stringr::str_detect(string = UFeat,
                                                          pattern = "PronType")
                                      )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
  }

  #stop_symbols_crosslingual
  if (stop_symbols_crosslingual) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    #(language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "SYM" 
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
  }
  
  #stop_punctuation_crosslingual
  if (stop_punctuation_crosslingual) {
    ling_filter_db <- dplyr::filter(multilingual_stoplist, 
                                    #(language_name %in% lang_name | language_id %in% lang_id) & 
                                    POS == "PUNCT" 
                                    
                                    
    )
    stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
    
  }
  
  #custom_filter
  if (!is.null(custom_filter)){
  ling_filter_db <- dplyr::filter_(multilingual_stoplist, custom_filter) 
}
  stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
####
  
  #print(stoplist_db)
  stoplist_vec <- tolower(stoplist_db$word_form) %>% unique() %>% sort()
  #print(stoplist_vec)
  if (output_form == "data.frame"){
    return(stoplist_db)
  } else  {return(stoplist_vec)}
  
}






