

generate_stoplist <- function(lang_name = NULL, 
                              lang_id = NULL,
                              output_form = c("unique_forms", 
                                              "stopclass_namedvec", "data_frame")
                              
                              {

                                  
## 2021-05-24 uncomment this for the package
    # a hack to silence package warnings  
    #data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
    #multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
    ######
  ## 2021-05-24 this here is for the function tweaks, delete for the package
  load(file = "stopwoRds/DATA/multilingual_stoplist.RData")
  ##
    
  lang_name = multilingual_stoplist$lang_name
    lang_id = multilingual_stoplist$lang_id
    lemma = multilingual_stoplist$lemma
    form = multilingual_stoplist$form
    ufeat = multilingual_stoplist$ufeat
    upos = multilingual_stoplist$POS
    

  #print(stoplist_vec)
  if (output_form == "unique_forms"){
    
    return(stoplist_db)
  } else if (output_form == "stopclass_namedvec")  {
   
    
    
    return(stopclass_namedvec)
    } else {
      
      return(data_frame)
    }

}

list_supported_language_names()
