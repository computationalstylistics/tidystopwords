\name{generate_stoplist}
\alias{generate_stoplist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
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
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a character vector encoded in escape-Unicode
}
\references{
\url{http://universaldependencies.org}

Nivre, Joakim; Agi\\u0107, \\u017deljko; Ahrenberg, Lars; et al., 2017, Universal Dependencies 2.1, LINDAT/CLARIN digital library at the Institute of Formal and Applied Linguistics (\\u00daFAL), Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Silvie Cinkova, Maciej Eder
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## sample(generate_stoplist(lang = "en", pos = c("INTJ", "ADP")), 10)
## [1] "no"       "ohh"      "across"   "farewell" "rather"   "x"       ....     

## sample(generate_stoplist(lang = "en"), 10)
##  [1] "before" "thy"  "whatever" "through"  "jeez"  "were" ..... 