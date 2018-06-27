<<<<<<< HEAD
=======
\encoding{utf8}
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
\name{generate_stoplist}
\alias{generate_stoplist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
<<<<<<< HEAD
Function to list stopwords in a supported language, controlled part-of-speech selection
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function gives you a character vector of word forms in a language from a selection. 
All words belong to closed parts of speech; i.e., function words such as prepositions, pronouns, and auxiliary verbs automatically collected from large corpora. You can narrow down the selection of parts of speech by listing only the desired parts of speech in the pos parameter.


}
\usage{
generate_stoplist(lang, pos = c("ADP", "AUX", "CCONJ", "DET", "INTJ", "PART", "PRON", "SCONJ"))

list_supported_languages()
list_supported_pos()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
The function filters a data frame with stopwords according to parts of speech and language. 


The data frame has the following columns:
\enumerate{
\item \code{word_form}: word forms as they are. 
\item  \code{POS}: Universal part of speech of the given word form. Cf. documentation of Universal Dependencies.
\item \code{UFeat}: Universal Features - complementary information to the universal part of speech tags. 
\item \code{refcorpus} column contains the corpus ID. We have tried to gather as large a data set for each language as possible, so we combined corpora across several releases of Universal dependencies. For instance the Latin stopwords come from three different corpora: ud_la_, ud_la_itt_, and ud_la_proiel_.   
\item \code{language}: language ids.
\item \code{freq_formPOS}: frequencies of each word form in a given language with a given POS tag across all corpora.
E.g. "her" as a personal pronoun (PRON) and "her" as a possessive pronoun (encoded under DET) would each have its own frequency.
\item \code{freq_formPOS}: frequencies of each word form in a given language across all corpora, no matter its POS tag. Here, the frequency of "her" would be the sum of its frequencies as a personal pronoun and as a possessive pronoun. 
}

The data comes from diverse Universal Dependencies corpora available in the LINDAT-CLARIN repository at http://hdl.handle.net/11234/1-2515, http://hdl.handle.net/11234/1-1699, and others (different versions of Universal Dependencies). We used the Kontext tool to extract words with the following Universal parts of speech:

\itemize{
\item \code{ADP. }{Adpositions (prepositions and postpositions); e.g. "during", "for", "in". Multiword prepositions such as "in spite of" are not included.}
\item \code{AUX. }{Auxiliary verb forms, such as "have" in "has read". In some languages, e.g. English, also modal verbs count as auxiliary verbs. In others, e.g. Czech, they are not, because their behavior does not differ from other lexical verbs.}
\item \code{DET. }{Determiners; e.g. "the", "this", "all". They modify nouns or noun phrases and express the reference of the noun phrase in context. In some languages, indefinite quantifiers, such as "all" and "few", belong to determiners.}
\item \code{CCONJ. }{Coordinating conjunctions; e.g. "but", "and", "or".}
\item \code{INTJ. }{Interjections; exclamatory words, such as "ecce", "bravo", "yes".}
\item \code{PART. }{Particles; function words that are associated with another word or phrase and encode grammatical categories such as negation, mood, or tense. NB: Germanic verb particles are captured as adpositions (ADP). }
\item \code{PRON. }{Pronouns that act as nouns; e.g. "you". Pronouns that behave like adjectives (e.g. "your") are mostly coded as determiners (DET).}
\item \code{SCONJ. }{Subordinating conjunctions; e.g "because".}
}
=======
Listing of stop words with control over language and part of speech
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generate a vector of stop words in one or several languages. Parameters allow you 
to toggle different parts of speech as well as to add new words.

}
\usage{
\code{function(lang_name = c("Afrikaans", "Ancient_Greek", "Arabic", #
                             "Basque","Bulgarian", "Buryat", "Catalan", #
                             "Chinese", "Coptic", "Croatian", "Czech",#
                             "Danish", "Dutch", "English", "Estonian",#
                             "Finnish", "French", "Galician", "German",#
                             "Gothic", "Greek", "Hebrew", "Hindi",#
                             "Hungarian", "Indonesian", "Irish", "Italian",#
                             "Japanese", "Kazakh", "Korean", "Kurmanji", #
                             "Latin", "Latvian", "North_Sami", "Norwegian",# 
                             "Old_Church_Slavonic", "Persian", "Polish",#
                             "Portuguese", "Romanian", "Russian", "Serbian",#
                             "Slovak", "Slovenian", "Spanish", "Swedish",#
                             "Tamil", "Turkish", "Ukrainian", "Upper_Sorbian",#
                             "Urdu", "Uyghur", "Vietnamese"), 
                             lang_id = NULL,
                             output_form = "vector", #option: "data.frame"
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
                             custom_filter = NULL )}}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{lang_name} -- single string or a character vector. A vector of some languages by default. 

\code{lang_id} -- a single string or a character vector. 
Call \code{list_supported_language_ids()} to see the supported ISO-639 language codes, if
you prefer these codes to language names. Mind to set the \code{lang_name} 
argument to \code{NULL}, if you do so. 

\code{output_form} -- default \code{"vector"}, alternatively \code{"data.frame"}

\code{stop_lemmas} -- default \code{NULL}. Supply a string or a character vector. 
You get all word forms of each listed lemma found in the data set for your 
selected language(s). POS disambiguation is not possible with this argument: 
mind homonyms.

\code{stop_forms} -- default \code{NULL}. Supply a string or a character vector. 
You get all word forms present in the data set for your selected language(s). 
POS disambiguation is not possible with this argument: mind homonyms.

The following parameters extract word forms from the data set with the relevant 
linguistic markup for your selected languages(s). It is often a combination of 
the coarse-grained universal POS tag and a set of so-called Universal Features. 
The same set of filters are used across all languages. 

\code{stop_foreign_words} -- default \code{TRUE}. 

\code{stop_abbreviations} -- default \code{TRUE}.

\code{stop_pronominals} -- default \code{TRUE}. All pronouns (e.g. "me", "this",
"what") along with pronominal adjectives and adverbs (e.g. "whose", "where").

\code{stop_determiners_quantifiers} -- default \code{TRUE}. Articles, 
possessive pronouns, quantifiers (e.g. "all", "few", "both").

\code{stop_conjuctions} -- default \code{TRUE}. Coordinating conjunctions; e.g. 
"and", "but". 

\code{stop_adpositions} -- default \code{TRUE}. Prepositions and postpositions; 
e.g. "of", "instead", "ago".

\code{stop_subordinating_conjunctions} -- default \code{TRUE}. E.g. "because".

\code{stop_auxiliary_verbs} -- default \code{TRUE}. E.g. "have", "be". 
Many languages mark also modal verbs ("must", "can") as auxiliary verbs, 
but some (e.g. Czech) do not. Check the UD documentation for your language
of interest. Also, mind that when adding e.g. "have" to your stop-word vector, 
you are also going to get rid of all its lexical uses; e.g. in "I have two cars." 
                             
\code{stop_interjections} -- default \code{TRUE}. Exclamatory words; e.g. "wow",
"ouch", but also "yes". Here the individual languages differ very much. 

\code{stop_particles} -- default \code{TRUE}. Also a very heterogeneous class. 
Check what is in there for your language of interest. NB: the English particles 
in phrasal verbs are regarded as adpositions, not as particles. 

\code{stop_numerals} -- default \code{TRUE}. Words denoting numbers: cardinal and ordinal numerals, as well as words that many languages normally regard as adjectives or adverbs; e.g. "double", "fourfold".

\code{stop_symbols_crosslingual} -- default \code{TRUE}. All symbols (e.g. emoji or currencies) harvested across all languages. 

\code{stop_punctuation_crosslingual} -- default \code{TRUE}. All punctuation marks harvested across all languages.

\code{custom_filter} -- default \code{NULL}. If you are not happy with the 
predefined stopword specifications, you can write your own filter for the 
underlying data set. Insert an evaluating expression in quotes. It will be 
parsed in \code{dplyr::filter(multilingual_stoplist,} your expression\code{)}. 
If you want to use the full power of \code{dplyr}, e.g. grouping, use 
the \code{multilingual_stoplist} data set on its own. 
This argument works independently of the pre-defined filters. 
Mind to turn them off if you do not want them. This filter also helps you 
override the pre-defined language choice, should you wish to.


%The data frame has the following columns:
%\enumerate{
%\item \code{word_form}: word forms as they are. 
%\item  \code{POS}: Universal part of speech of the given word form. Cf. documentation of Universal Dependencies.
%\item \code{UFeat}: Universal Features - complementary information to the universal part of speech tags. 
%\item \code{refcorpus} column contains the corpus ID. We have tried to gather as large a data set for each language as possible, so we combined corpora across several releases of Universal dependencies. For instance the Latin stopwords come from three different corpora: ud_la_, ud_la_itt_, and ud_la_proiel_.   
%\item \code{language}: language ids.
%\item \code{freq_formPOS}: frequencies of each word form in a given language with a given POS tag across all corpora.
%E.g. "her" as a personal pronoun (PRON) and "her" as a possessive pronoun (encoded under DET) would each have its own frequency.
%\item \code{freq_formPOS}: frequencies of each word form in a given language across all corpora, no matter its POS tag. Here, the frequency of "her" would be the sum of its frequencies as a personal pronoun and as a possessive pronoun. 
%}
%}
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
<<<<<<< HEAD
a character vector encoded in escape-Unicode
}
\references{
\url{http://universaldependencies.org}

Nivre, Joakim; Agi\\u0107, \\u017deljko; Ahrenberg, Lars; et al., 2017, Universal Dependencies 2.1, LINDAT/CLARIN digital library at the Institute of Formal and Applied Linguistics (\\u00daFAL), Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
=======
a character vector, UTF-8 encoded.
}
\references{
The data set is based on the official release of Version 2.1 of Universal Dependencies.

\url{http://universaldependencies.org}

Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), 
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}.  
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
<<<<<<< HEAD
Silvie Cinkova, Maciej Eder
=======
Silvie Cinková, Maciej Eder
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
<<<<<<< HEAD

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## sample(generate_stoplist(lang = "en", pos = c("INTJ", "ADP")), 10)
## [1] "no"       "ohh"      "across"   "farewell" "rather"   "x"       ....     

## sample(generate_stoplist(lang = "en"), 10)
##  [1] "before" "thy"  "whatever" "through"  "jeez"  "were" ..... 
=======
\section{Warning}{
\itemize{
\item{The function stops when both \code{lang_name} and \code{lang_id} equal \code{NULL}. Setting a language in the custom filter does not do.}
\item{The function stops when \code{lang_name} or \code{lang_id} contains an unsupported item. Mind that the selection is case-sensitive. The error message prints the culprit(s). If you have set both \code{lang_name} or \code{lang_id} and the check finds an error in the former, it will not go on checking the latter. }
\item{The languages supported by the first version of this package are listed as argument default, but the actual 
language inventory relies on the underlying data frame. The initial checks call \code{list_supported_language_names()} and \code{list_supported_language_ids()}.}
\item{The pre-defined linguistic filters are not mutually exclusive. Their overlap varies among languages.}
\item{You will see a warning message when you set both \code{lang_name} and \code{lang_id}, no matter whether they represent the same language or different languages.}
\item{When you run the function with default argument values, you will get stopwords from all supported languages mixed up and see a warning about that.}
\item{The stoplists are fully data-driven. We have set a threshold of 10 occurrences of a combination of language, word form, lemma, POS and the Universal Features to remove obvious noise, but some noise is bound to have come through anyway. It is mainly foreign words that were given a regular POS tag (cf. the example code snippets below: the English "and" has sneaked in among the German coordinating conjunctions). 
Many languages are represented by balanced and large corpora of standard written texts, but some are not; e.g. based mainly on a Bible translation. Hence also their stopwords can be biased. }
}
}
%}
%}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{list_supported_language_names}}, \code{\link{list_supported_language_ids}}, \code{\link{list_supported_pos}}, \code{\link{multilingual_stoplist}}}

\examples{
\code{## sample(generate_stoplist(lang_name = "English"), 20)}
\code{[1] "a"     "24"    "an"    "c:"    "《"    "into"  "b"    
[8] "**"    "x"     "'1"    "us$"   "whom"  "!"     "；"   
[15] "you"   "£"     "whose" "・"    "(4)"   "three"}

\code{## generate_stoplist(lang_name = "English", 
                          #default NULL in lang_id, stop_lemmas, stop_forms 
                          stop_foreign_words = FALSE, 
                          stop_abbreviations = FALSE,
                          stop_pronominals = FALSE,
                          stop_determiners_quantifiers = FALSE,
                          stop_conjuctions = TRUE, 
                          stop_adpositions = FALSE,
                          stop_subordinating_conjunctions = FALSE,
                          stop_auxiliary_verbs = FALSE,
                          stop_interjections = FALSE,
                          stop_particles = FALSE,
                          stop_numerals = FALSE,
                          stop_symbols_crosslingual = FALSE,
                          stop_punctuation_crosslingual = FALSE,
                          custom_filter = NULL)}
\code{ 
 [1] "&"       "and"     "both"    "but"    
 [5] "either"  "neither" "nor"     "not"    
 [9] "or"      "plus"    "yet" }

\code{##
generate_stoplist(lang_name = "English",          ## LOOK HERE
                  stop_foreign_words = FALSE, 
                   stop_abbreviations = FALSE,
                   stop_pronominals = FALSE,
                   stop_determiners_quantifiers = FALSE,
                   stop_conjuctions = FALSE, 
                   stop_adpositions = FALSE,
                   stop_subordinating_conjunctions = FALSE,
                   stop_auxiliary_verbs = FALSE,
                   stop_interjections = FALSE,
                   stop_particles = FALSE,
                   stop_numerals = FALSE,
                   stop_symbols_crosslingual = FALSE,
                   stop_punctuation_crosslingual = FALSE,
                   custom_filter = "language_name == 'German' & POS == 'CCONJ'"
                   # filter overrides the lang_name argument
                   # NB real column names of the data set, not argument names
                   # in the filter!!
                   )}
\code{##
[1] "aber"     "als"      "and"      "denn"    
 [5] "doch"     "entweder" "jedoch"   "noch"    
 [9] "oder"     "sondern"  "sowie"    "sowohl"  
[13] "und"      "weder"    "wie" }

\code{##
generate_stoplist(lang_name = "Japanese", lang_id = "cs", stop_foreign_words = FALSE, 
                   stop_abbreviations = FALSE,
                   stop_pronominals = FALSE,
                   stop_determiners_quantifiers = FALSE,
                   stop_conjuctions = FALSE, 
                   stop_adpositions = FALSE,
                   stop_subordinating_conjunctions = FALSE,
                   stop_auxiliary_verbs = FALSE,
                   stop_interjections = FALSE,
                   stop_particles = FALSE,
                   stop_numerals = FALSE,
                   stop_symbols_crosslingual = FALSE,
                   stop_punctuation_crosslingual = FALSE,
                   custom_filter = "language_name == 'German' & POS == 'CCONJ'"
                   )}
\code{##
[1] "aber"     "als"      "and"      "denn"    
 [5] "doch"     "entweder" "jedoch"   "noch"    
 [9] "oder"     "sondern"  "sowie"    "sowohl"  
[13] "und"      "weder"    "wie" 
Warning message:
HEADS UP! Language selection by_name as well as by lang_id. 
 You may want to check your selection.}

\code{##
generate_stoplist(lang_name = c("english", "Russian", "Klingon", "Esperanto"))}
\code{##
[1] "english"   "Klingon"   "Esperanto"
Error: Remove the item(s) listed above from lang_name. 
 To check out the supported languages, call `list_supported_language_names()`.
Called from: generate_stoplist(lang_name = c("english", "Russian", "Klingon", 
    "Esperanto"))} }
 

 
 
 
 
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
