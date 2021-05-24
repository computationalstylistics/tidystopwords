

generate_stoplist <- function(lang_name = NULL, 
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

                                  
## 2021-05-24 uncomment this for the package
    # a hack to silence package warnings  
    #data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
    #multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    ######
  ## 2021-05-24 this here is for the function tweaks, delete for the package
  load(file = "stopwoRds/DATA/multilingual_stoplist.RData")
  ##
    
  langage_name = multilingual_stoplist$language_name
    language_id = multilingual_stoplist$language_id
    lemma = multilingual_stoplist$lemma
    word_form = multilingual_stoplist$word_form
    UFeat = multilingual_stoplist$UFeat
    POS = multilingual_stoplist$POS
    

# the vector of language names is to be updated manually with every new version of the multilingual_stoplist.csv file
 # if (!isTRUE(all.equal(lang_name, c("Afrikaans", "Ancient_Greek", "Arabic", "Basque","Bulgarian", "Buryat", "Catalan", 
 #                                   "Chinese", "Coptic", "Croatian", "Czech", "Danish", "Dutch", "English", "Estonian", 
 #                                   "Finnish", "French", "Galician", "German", "Gothic", "Greek", "Hebrew", "Hindi", 
 #                                   "Hungarian", "Indonesian", "Irish", "Italian", "Japanese", "Kazakh", "Korean", 
 #                                   "Kurmanji", "Latin", "Latvian", "North_Sami", "Norwegian", "Old_Church_Slavonic",
 #                                   "Persian", "Polish" ,"Portuguese", "Romanian", "Russian", "Serbian", "Slovak",
 #                                   "Slovenian", "Spanish", "Swedish", "Tamil", "Turkish", "Ukrainian", "Upper_Sorbian",
 #                                   "Urdu", "Uyghur", "Vietnamese"
 # ))))

### added to allow selecting language with ID only    
    if (!is.null(lang_name)) {

        unsupported_language_names <- character()
        for (i in 1:length(lang_name)){
            if (!lang_name[i] %in% list_supported_language_names()){ 
                unsupported_language_names <- c(unsupported_language_names, lang_name[i])
                # Even if we do not manually update the vector of supported languages of the lang_name parameter, 
                # missing languages will come through provided they occur in the current multilingual_stoplist.csv file. 
            }
        }
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
      
      
      if (!is.null(lang_name) & !is.null(lang_id)) {
      warning("HEADS UP! Language selection by_name as well as by lang_id. \n You may want to check your selection.\n", call. = FALSE)
      }}

    #####added to allow selection with lang_id only
    
if (is.null(lang_name) & is.null(lang_id) )
    {
     lang_name <- list_supported_language_names()
     warning("HEADS UP! Selection includes all supported languages. \n  You may want to check your selection.\n", call. = FALSE)
    }
        
###### 2021-05-24 NEW LINGUISTIC FILTERS
    
   #eval(parse(text = paste("2 == 3", "|", "2==2") )) #this could work to combine filters
    
  
  fltr_stoplemmas <- '(
  (language_name %in%  lang_name  | language_id %in% lang_id)
                                     & tolower(lemma) %in% tolower(stop_lemmas)
  
  )'
  
  fltr_stopforms <- '(
  (language_name %in% lang_name | language_id %in% lang_id)
                                     & tolower(word_form) %in% tolower(stop_forms)
  )'
  
  fltr_foreign_words <- '(
  (language_name %in% lang_name | language_id %in% lang_id) &
                                    stringr::str_detect(string = UFeat, pattern = "Foreign=Yes")
  )'
  fltr_abbreviations <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      stringr::str_detect(string = UFeat, 
                                                          pattern = "NumType") &
                                     (POS %in% c("ADV", "DET") == FALSE)
  )'
  fltr_numerals <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      stringr::str_detect(string = UFeat, 
                                                          pattern = "NumType") &
                                      (POS %in% c("ADV", "DET") == FALSE)
  )'
  fltr_determiners_quantifiers <- '(
   (language_name %in% lang_name | language_id %in% lang_id) & 
  (POS == "DET" | (stringr::str_detect(string = UFeat, 
                                                            pattern = "NumType") 
                                        & stringr::str_detect(string = UFeat, pattern = "PronType")
                                        & (stringr::str_detect(string = UFeat, pattern = "Ind")
                                            | stringr::str_detect(string = UFeat, pattern = "Dem")
                                            | stringr::str_detect(string = UFeat, pattern = "Int")
                                            | stringr::str_detect(string = UFeat, pattern = "Rel")
                                           | stringr::str_detect(string = UFeat, pattern = "Tot")
                                           | stringr::str_detect(string = UFeat, pattern = "Neg"))
                                      ))
  
  )'
  fltr_conjunctions <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                    POS == "CCONJ"
  )'
  fltr_adpositions <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "ADP"
  )'
  fltr_subordinating_conjunctions <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "SCONJ"
  )'
  fltr_auxiliary_verbs <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "AUX"
  
  )'
  fltr_interjections <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "INTJ"
  )'
  
  fltr_particles <- '(
  (language_name %in% lang_name | language_id %in% lang_id) & 
                                      POS == "PART"
  )'
  
  fltr_pronominals <- '(
  (language_name %in% lang_name | language_id %in% lang_id) &
                                      stringr::str_detect(string = UFeat,
                                                          pattern = "PronType")
                                      
  )'
  fltr_symbols_multilingual <- '(
  POS == "SYM"
  )'
  fltr_punctuation_multilingual <- '(
  POS == "PUNCT"
  )'
  fltr_customfilter <- custom_filter
   
#####    
    
    
# ## 2021-05-24 ORIGINAL LINGUISTIC FILTERS    
# ##################################################################################################################################  
# 
#   # LINGUISTIC FILTERS  
#   
#   # Each linguistic filter will filter rows from multilingual_stoplist into a subdataframe. 
#   # A new empty dataframe (called stoplist) will rowbind subdataframes step by step. 
#   # The final stoplist will be generated from wordforms of the stoplist data frame after all. 
#   stoplist_db <- matrix(nrow = 0, ncol = ncol(multilingual_stoplist)) %>% as.data.frame(stringsAsFactors = FALSE) 
#   colnames(stoplist_db) = colnames(multilingual_stoplist)
#  
#  
#    ##stop_lemmas: a character vector or a single string from user
#   if (!is.null(stop_lemmas)) {
#     ling_filter_db  <- dplyr::filter(multilingual_stoplist,
#                                      (language_name %in%  lang_name  | language_id %in% lang_id)
#                                      & tolower(lemma) %in% tolower(stop_lemmas)
#                                        )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
#   }
# 
#   ##stop_forms: a character vector or a single string from user
#   if (!is.null(stop_forms)) {
#     ling_filter_db  <- dplyr::filter(multilingual_stoplist,
#                                      (language_name %in% lang_name | language_id %in% lang_id)
#                                      & tolower(word_form) %in% tolower(stop_forms)
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
#   }
# 
#   ## stop_foreign_words
#   if ((stop_foreign_words)) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) &
#                                     stringr::str_detect(string = UFeat, pattern = "Foreign=Yes")
#                                     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
# 
#  ## stop_abbreviations
#   if ((stop_abbreviations)) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                     stringr::str_detect(string = UFeat, pattern = "Abbr=Yes")
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   ## stop_numerals
#   if (stop_numerals) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       stringr::str_detect(string = UFeat, 
#                                                           pattern = "NumType") &
#                                       (POS %in% c("ADV", "DET") == FALSE)
#                                      
#    
#      )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#  
#   ## stop_determiners_quantifiers
#   if(stop_determiners_quantifiers){
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       (POS == "DET" | (
#                                        stringr::str_detect(string = UFeat, 
#                                                             pattern = "NumType") 
#                                         & stringr::str_detect(string = UFeat, pattern = "PronType")
#                                         & (stringr::str_detect(string = UFeat, pattern = "Ind")
#                                             | stringr::str_detect(string = UFeat, pattern = "Dem")
#                                             | stringr::str_detect(string = UFeat, pattern = "Int")
#                                             | stringr::str_detect(string = UFeat, pattern = "Rel")
#                                            | stringr::str_detect(string = UFeat, pattern = "Tot")
#                                            | stringr::str_detect(string = UFeat, pattern = "Neg"))
#                                       )))
#                                         
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   ## stop_conjuctions 
#   if (stop_conjuctions) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                     POS == "CCONJ"
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   # stop_adpositions
#   if (stop_adpositions) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       POS == "ADP"
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   
#   
#   # stop_subordinating_conjunctions
#   if (stop_subordinating_conjunctions) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       POS == "SCONJ"
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#  
#   #stop_auxiliary_verbs
#   if (stop_auxiliary_verbs) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       POS == "AUX"
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   # stop_interjections 
#   if (stop_interjections) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       POS == "INTJ"
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#  
#   #stop_particles
#   if (stop_particles) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     (language_name %in% lang_name | language_id %in% lang_id) & 
#                                       POS == "PART"
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   
#   
#    # stop_pronominals
#   if (stop_pronominals) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist,
#                                     (language_name %in% lang_name | language_id %in% lang_id) &
#                                       stringr::str_detect(string = UFeat,
#                                                           pattern = "PronType")
#                                       )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
#   }
# 
#   #stop_symbols_crosslingual
#   if (stop_symbols_crosslingual) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     #(language_name %in% lang_name | language_id %in% lang_id) & 
#                                       POS == "SYM" 
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#   }
#   
#   #stop_punctuation_crosslingual
#   if (stop_punctuation_crosslingual) {
#     ling_filter_db <- dplyr::filter(multilingual_stoplist, 
#                                     #(language_name %in% lang_name | language_id %in% lang_id) & 
#                                     POS == "PUNCT" 
#                                     
#                                     
#     )
#     stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db) 
#     
#   }
#   
#   #custom_filter
#   if (!is.null(custom_filter)){
#   ling_filter_db <- dplyr::filter_(multilingual_stoplist, custom_filter) 
# }
#   stoplist_db <- dplyr::bind_rows(stoplist_db, ling_filter_db)
# ####

#####2021-05-24 
  
ling_filter_db <- character()
     ##stop_lemmas: a character vector or a single string from user
   if  (!is.null(stop_lemmas)) {
      ling_filter_db <- c(ling_filter_db, fltr_stoplemmas)

  }

  #   ##stop_forms: a character vector or a single string from user
   if (!is.null(stop_forms)) {
     ling_filter_db <- c(ling_filter_db, fltr_stopforms)
   }

 
  #   ## stop_foreign_words
   if (stop_foreign_words) {
     ling_filter_db <- c(ling_filter_db, fltr_foreign_words)
   }

 
  #  ## stop_abbreviations
    if (stop_abbreviations) {
      ling_filter_db <- c(ling_filter_db, fltr_abbreviations)
    }

  #   
  #   ## stop_numerals
    if (stop_numerals) { 
      ling_filter_db <- c(ling_filter_db, fltr_numerals)
    }

  #   ## stop_determiners_quantifiers
   if(stop_determiners_quantifiers){
     ling_filter_db <- c(ling_filter_db, fltr_determiners_quantifiers)
   }
  #   
  #   ## stop_conjuctions 
   if (stop_conjuctions) {
     ling_filter_db <- c(ling_filter_db, fltr_conjunctions)
   }
  #   # stop_adpositions
  if (stop_adpositions) {
    ling_filter_db <- c(ling_filter_db, fltr_adpositions)
  }
  
  #   # stop_subordinating_conjunctions
  if (stop_subordinating_conjunctions) {
     ling_filter_db <- c(ling_filter_db, fltr_subordinating_conjunctions )
   }
   
  #   #stop_auxiliary_verbs
  if (stop_auxiliary_verbs) {
    ling_filter_db <- c(ling_filter_db, fltr_auxiliary_verbs )
  }
  #   # stop_interjections 
  if (stop_interjections) {
     ling_filter_db <- c(ling_filter_db, fltr_interjections)
   }
    #   #stop_particles
  if (stop_particles) {
    ling_filter_db <- c(ling_filter_db, fltr_particles)
  }
  #    # stop_pronominals
  if (stop_pronominals) {
    ling_filter_db <- c(ling_filter_db, fltr_pronominals)
  }
  #   #stop_symbols_crosslingual
  if (stop_symbols_crosslingual) {
    ling_filter_db <- c(ling_filter_db, fltr_symbols_multilingual)
  }
  if (stop_punctuation_crosslingual) {
    ling_filter_db <- c(ling_filter_db, fltr_punctuation_multilingual)
   }
 custom_filter
 if (!is.null(custom_filter)) {
   ling_filter_db <- c(ling_filter_db, fltr_customfilter)
 }

ling_filter_db <- str_c(ling_filter_db, collapse = "|") %>%
  parse(text = .)

stoplist_db <- multilingual_stoplist %>%
  filter(eval(ling_filter_db))

####original, keep!!!!

  #print(stoplist_db)
  stoplist_vec <- tolower(stoplist_db$word_form) %>% unique() %>% sort()
  #print(stoplist_vec)
  if (output_form == "data.frame"){
    return(stoplist_db)
  } else  {return(stoplist_vec)}

}

#EXAMPLE - this has worked
# generate_stoplist(lang_name = "German",  stop_punctuation_crosslingual = FALSE, 
#                   stop_foreign_words = FALSE, stop_abbreviations = FALSE, 
#                   stop_pronominals = FALSE, stop_determiners_quantifiers = FALSE, 
#                   stop_adpositions = FALSE, stop_interjections = FALSE, 
#                   stop_numerals = FALSE, stop_conjuctions = FALSE, 
#                   stop_symbols_crosslingual = FALSE, 
#                   stop_auxiliary_verbs = FALSE,stop_particles = FALSE, 
#                   stop_subordinating_conjunctions = FALSE,
#                   custom_filter = "lemma == 'und' & language_name == 'German'", 
#                   output_form = "data.frame")



