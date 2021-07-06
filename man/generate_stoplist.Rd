\encoding{utf8}
\name{generate_stoplist}
\alias{generate_stoplist}

\title{
Listing of stop words in different languages.
}
\description{
Generate a vector of stop words in one or several languages. 
}
\usage{
generate_stoplist(language = NULL, output_form = 1)
}

\arguments{
\item{language}{single string or a character vector. \code{NULL} by default. 
The strings can be language names or ISO-639 language codes as listed by the \code{list_supported_languages()}, freely combined. When no recognized language is recognized, this error message appears: \code{"The language name or language id you have selected is not supported. Check out the supported languages by calling 
`list_supported_languages`.\n"}. 

\item{output_form}{default \code{1}, alternatively \code{2} or \code{3}.}
	Option \code{1} returns a character vector of alphabetically sorted unique stopwords word forms. 
	Option \code{2} returns a named vector whose elements are the stopwords word forms and names are the associated stop classes. One word form can occur with different stop classes; hence the word forms in this vector are not unique, unlike Option \code{1}. 
	Option \code{3} returns a data frame filtered according to the language selection. 



%\details{
%The underlying data frame has the following columns:
%\enumerate{
%\item \code{word_form}: word forms in lower case. 
%\item \code{lemma}: dictionary form of the word form in lower case.
%\item \code{stopclass}: one of the following: 
	- \code{abbreviation}: common abbreviations acting as adverbs or adjectives, for instance *e.g., etc., cf.*;  
	- \code{adposition}: prepositions or postpositions (e.g. *in*, *ago*);
	- \code{auxiliary_verb}: auxiliary or modal verb (e.g. *would*);
	- \code{conjunction_subordinator}: coordinating or subordinating conjunctions(e.g. *and*, *because*);
	- \code{contractions}: contracted forms (e.g. *'n'* or *she'd*);
	- \code{determiner_quantifier}: pronouns, articles, pronominal adverbs, and some numerals not written as digits - all acting as adjectives or adverbs, not nouns (e.g. *yours*, *the*, *both* ,*where*, *twofold*. Cf. \code{pronominals}; 
	- \code{interjection}: words denoting sounds and performative words like *yes*, *no*, *please*, *thanks*; 
	\code{particle}: either preposition-like words in phrasal verbs (e.g. in English) or diverse words indicating the speaker's attitude to the statement (e.g. *fortunately*);
	\code{pronominal}: pronouns acting as nouns (e.g. *we* - cf. \code{determiner_quantifier})
%\item \code{upos}: Universal part of speech of the given word form. Cf. documentation of Universal Dependencies.
%\item \code{ufeat}: Universal Features - complementary information to the universal part of speech tags. The \code{multilingual_stoplist} data frame contains only a selection from the original ufeat string to abstract from morphological categories that do not affect whether or not a word is eligible for the stopword list (for instance case, gender, number, person, and tense). 
%\item \code{n_stopclasses}: 
%\item \code{lang_name}: language name
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
    # standard usage (might return some non-ASCII characters):
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
