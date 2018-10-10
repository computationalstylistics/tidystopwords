\encoding{utf8}
\name{generate_stoplist}
\alias{generate_stoplist}

\title{
Listing of stop words with control over language and part of speech.
}
\description{
Generate a vector of stop words in one or several languages. Parameters allow you 
to toggle different parts of speech as well as to add new words.

}
\usage{
generate_stoplist(lang_name = c("Afrikaans", "Ancient_Greek", "Arabic",
                         "Basque","Bulgarian", "Buryat", "Catalan",
                         "Chinese", "Coptic", "Croatian", "Czech",
                         "Danish", "Dutch", "English", "Estonian",
                         "Finnish", "French", "Galician", "German",
                         "Gothic", "Greek", "Hebrew", "Hindi",
                         "Hungarian", "Indonesian", "Irish", "Italian",
                         "Japanese", "Kazakh", "Korean", "Kurmanji",
                         "Latin", "Latvian", "North_Sami", "Norwegian",
                         "Old_Church_Slavonic", "Persian", "Polish",
                         "Portuguese", "Romanian", "Russian", "Serbian",
                         "Slovak", "Slovenian", "Spanish", "Swedish",
                         "Tamil", "Turkish", "Ukrainian", "Upper_Sorbian",
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
                  custom_filter = NULL )
}

\arguments{
\item{lang_name}{single string or a character vector. A vector of some languages by default.}

\item{lang_id}{a single string or a character vector. Call \code{list_supported_language_ids()} to see the supported ISO-639 language codes, if you prefer these codes to language names. Mind to set the \code{lang_name} argument to \code{NULL}, if you do so.}

\item{output_form}{default \code{"vector"}, alternatively \code{"data.frame"}.}

\item{stop_lemmas}{default \code{NULL}. Supply a string or a character vector. You get all word forms of each listed lemma found in the data set for your selected language(s). POS disambiguation is not possible with this argument: mind homonyms.}

\item{stop_forms}{default \code{NULL}. Supply a string or a character vector. You get all word forms present in the data set for your selected language(s). POS disambiguation is not possible with this argument: mind homonyms. 
    
The following parameters extract word forms from the data set with the relevant  linguistic markup for your selected languages(s). It is often a combination of the coarse-grained universal POS tag and a set of so-called Universal Features. The same set of filters are used across all languages.}

\item{stop_foreign_words}{default \code{TRUE}.}

\item{stop_abbreviations}{default \code{TRUE}.}

\item{stop_pronominals}{default \code{TRUE}. All pronouns (e.g. "me", "this",
"what") along with pronominal adjectives and adverbs (e.g. "whose", "where").}

\item{stop_determiners_quantifiers}{default \code{TRUE}. Articles, 
possessive pronouns, quantifiers (e.g. "all", "few", "both").}

\item{stop_conjuctions}{default \code{TRUE}. Coordinating conjunctions; e.g. 
"and", "but".}

\item{stop_adpositions}{default \code{TRUE}. Prepositions and postpositions; 
e.g. "of", "instead", "ago".}

\item{stop_subordinating_conjunctions}{default \code{TRUE}. E.g. "because".}

\item{stop_auxiliary_verbs}{default \code{TRUE}. E.g. "have", "be". 
Many languages mark also modal verbs ("must", "can") as auxiliary verbs, 
but some (e.g. Czech) do not. Check the UD documentation for your language
of interest. Also, mind that when adding e.g. "have" to your stop-word vector, 
you are also going to get rid of all its lexical uses; e.g. in "I have two cars."}
                             
\item{stop_interjections}{default \code{TRUE}. Exclamatory words; e.g. "wow",
"ouch", but also "yes". Here the individual languages differ very much.}

\item{stop_particles}{default \code{TRUE}. Also a very heterogeneous class. 
Check what is in there for your language of interest. NB: the English particles 
in phrasal verbs are regarded as adpositions, not as particles.} 

\item{stop_numerals}{default \code{TRUE}. Words denoting numbers: cardinal and ordinal numerals, as well as words that many languages normally regard as adjectives or adverbs; e.g. "double", "fourfold".}

\item{stop_symbols_crosslingual}{default \code{TRUE}. All symbols (e.g. emoji or currencies) harvested across all languages.}

\item{stop_punctuation_crosslingual}{default \code{TRUE}. All punctuation marks harvested across all languages.}

\item{custom_filter}{default \code{NULL}. If you are not happy with the 
predefined stopword specifications, you can write your own filter for the 
underlying data set. Insert an evaluating expression in quotes. It will be 
parsed in \code{dplyr::filter(multilingual_stoplist,} your expression\code{)}. 
If you want to use the full power of \code{dplyr}, e.g. grouping, use 
the \code{multilingual_stoplist} data set on its own. 
This argument works independently of the pre-defined filters. 
Mind to turn them off if you do not want them. This filter also helps you 
override the pre-defined language choice, should you wish to.}
}


%\details{
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
%}

\value{
A character vector, UTF-8 encoded.
}

\references{
The data set is based on the official release of Version 2.1 of Universal Dependencies.

\url{http://universaldependencies.org}

Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), 
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}.  
}

\author{
Silvie Cinková, Maciej Eder
}


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


\seealso{
\code{\link{list_supported_language_names}}, \code{\link{list_supported_language_ids}}, \code{\link{list_supported_pos}}, \code{\link{multilingual_stoplist}}
}

\examples{

    # standard usage:
    generate_stoplist(lang_name = "English")
    
    
    # to get only conjunctions (11 items):
    generate_stoplist(lang_name = "English", 
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
                          stop_punctuation_crosslingual = FALSE)

}
