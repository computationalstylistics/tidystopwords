#library(dplyr)
#library(readr)
#library(tidyr)
#library(magrittr)
#library(purrr)

generate_stoplist <- function(language = NULL, 
                              output_form = 1)
                              
                              {

                                  

# a hack to silence package warnings  
data("multilingual_stoplist", package = "tidystopwords", envir = environment()) 
multilingual_stoplist <- get("multilingual_stoplist", envir = environment()) 
form <- lang_id <- lang_name <- stopclass <- NULL


#load(file = "../DATA/multilingual_stoplist.RData")  
###    
#multilingual_stoplist <- stpwrds3_df
language_check <- filter(multilingual_stoplist, 
                         lang_name %in% language |lang_id %in% language)   
if (nrow(language_check) == 0) {
  stop("The language name or language id you have selected is not supported. 
       (Or you didn't specify a language at all). Check out the supported languages 
       by calling `list_supported_languages`.\n", call. = FALSE )
}
 
  #print(stoplist_vec)
  if (output_form == 3){
        return(language_check)
  } else if (output_form == 2)  {
    stopclass_namedvec <- language_check %>% 
      arrange(lang_name, stopclass, form) %>% 
      distinct(lang_name, form, stopclass) %>% 
      pull(form, name = "stopclass")
    return(stopclass_namedvec)
    } else {
    unique_forms <- language_check %>% 
      arrange(lang_name, stopclass, form) %>% 
      distinct(lang_name, form, stopclass) %>% 
      pull(form)
   
     return(unique_forms)
    }

}




