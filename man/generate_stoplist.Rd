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
The strings can be language names or ISO-639 language codes as listed by the \code{list_supported_languages()}, freely combined, case-sensitive. When no recognized language is recognized, this error message appears: \code{"The language name or language id you have selected is not supported. Check out the supported languages by calling 
`list_supported_languages`.\n"}. 

\item{output_form}{default \code{1}, alternatively \code{2} or \code{3}.}
	Option \code{1} returns a character vector of alphabetically sorted unique stopwords word forms. 
	Option \code{2} returns a named vector whose elements are the stopwords word forms and names are the associated stop classes. One word form can occur with different stop classes; hence the word forms in this vector are not unique, unlike Option \code{1}. 
	Option \code{3} returns a data frame filtered according to the language selection. 



%\details{
%The underlying data frame has the following columns:
%\enumerate{
%\item \code{form}: word forms in lower case. 
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
%\item \code{ufeat}: Universal Features - here a selection of the original UD ufeat string containing complementary information to the universal part of speech tags, removing morphological categories that do not affect a word's eligibility for the stopword list (for instance case, gender, number, person, and tense). 
%\item \code{n_stopclasses}: the number of stop classes to which the given combination of form and lemma belongs. If greater than 1, the given combination of form and lemma is grammatically homonymous. For instance, *that* can act as a noun (corresponding to \code{pronominal}), determiner/adjective (corresponding to \code{determiner_quantifier}), and subordinating conjunction (corresponding to \code{conjunction_subordinator}), and hence it appears in three stopclasses;  
%item code{n_formlemma}: the frequency of the given combination of form and lemma across all UD treebanks currently available for the given language;
%\item \code{n_uposformlemma}: the frequency of the given combination of upos, form, and lemma across all UD treebanks currently available for the given language;
%\item \code{lang_id}: ISO-639 language code;
%\item \code{lang_name}: language name (case-sensitive).


%}
%}
%}

\value{
The function comes with three output options. 

-   Option `1` outputs a character vector of unique word forms.
-   Option `2` outputs a named character vector of word forms. The names denote 
`stop classes` roughly corresponding to parts of speech. Note that, in this 
output, the word forms are not unique. For instance, in English stopwords, *that*
would occur as a subordinating conjunction as well as as a pronoun.  
- Option `3` (the default) outputs a data frame, where each row represents a combination 
of language (columns `lang_name` and `lang_id`), word form and word lemma 
(columns `form` and `lemma`), and several other columns explained below.    

All outputs are encoded in UT-8.
}

\references{
The underlying data frame `multilingual_stoplist` is based on the official release of Version 2.8 of Universal Dependencies.

\url{http://universaldependencies.org}

Zeman, Daniel; et al., 2021, 
Universal Dependencies 2.8.1, LINDAT/CLARIAH-CZ digital library at the Institute of Formal and Applied Linguistics (ÚFAL), Faculty of Mathematics and Physics, Charles University, http://hdl.handle.net/11234/1-3687.

}



\author{
Silvie Cinková, Maciej Eder
}


\section{Warning}{
\itemize{
\item{The function stops when no language is selected.}
\item{The stop classes (pre-defined linguistic filters) are not mutually exclusive. Their overlap varies among languages.}
\item{The stoplists are fully data-driven. We have set a threshold of 3 occurrences of a combination of language, form, lemma, and upos to remove obvious noise, but some noise is bound to have come through anyway. It is mainly foreign words that were given a regular upos tag (e.g. the English "and" has sneaked in among the German coordinating conjunctions). Another known case is the \code{contraction} stop class in English, which, among well-suited instances such as *ain't* includes uses of the so-called Saxonic genitive (e.g. *world's*).  
Many languages are represented by balanced and large corpora of standard written texts, but some are not; e.g. based mainly on a Bible translation or Wikipedia. Hence also their stopwords can be biased. }
}
}


\seealso{
\code{\link{list_supported_languages_names}}, \code{\link{multilingual_stoplist}}
}

\examples{
   

}
