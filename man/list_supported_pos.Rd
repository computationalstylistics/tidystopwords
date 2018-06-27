<<<<<<< HEAD
\name{list_supported_pos}
\alias{list_supported_pos}
\alias{generate_stoplist}
=======
\encoding{utf8}
\name{list_supported_pos}
\alias{list_supported_language_names}
\alias{list_supported_language_ids}
\alias{list_supported_pos}
\alias{generate_stoplist}
\alias{multilingual_stoplist}
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
<<<<<<< HEAD
Function to list supported parts of speech to include in stopword lists.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function gives you a character vector of parts of speech.

}
\usage{
> list_supported_pos()
[1] "ADP"   "AUX"   "DET"   "INTJ"  "PART"  "PRON"  "SCONJ"

=======
Listing of parts of speech to include in stopword lists you generate by  \code{generate_stoplist()}.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function gives you a character vector of supported parts of speech (e.g. prepositions). They are represented by abbreviations.
}
\usage{
> list_supported_pos()
[[1] "ADJ"   "ADP"   "ADV"   "AUX"   "CCONJ" 
"DET"   "INTJ"  "NOUN"  "NUM"  
[10] "PART"  "PRON"  "PROPN" "PUNCT" "SCONJ" "SYM"   "VERB"
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
<<<<<<< HEAD
The function lists parts of speech to fill 
the \code{pos} argument of the \code{generate_stoplist()} function. 
By default, all parts of speech are included. You can narrow down the 
selection by supplying your own vector of parts of speech. }
=======
The \code{stopwoRds} package relies on \code{multilingual_stoplist}, a large multilingual table with individual word forms as rows, derived from the Universal Dependencies treebanks. Each word form comes along with its lemma and part of speech, as well as with the language name and its ISO-639-nnnn code. This function gives you unique values from the \code{POS} column of \code{multilingual_stoplist}.
The parts of speech (POS) are common for all supported languages ("Universal Part-of-Speech tags"). 

}

>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a character vector 
}
\references{
\url{http://universaldependencies.org}

<<<<<<< HEAD
Nivre, Joakim; Agi\\u0107, \\u017deljko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (\\u00daFAL), 
=======
Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), 
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
%% ~put references to the literature/web site here ~
}

\author{
%%  ~~who you are~~
<<<<<<< HEAD
Silvie Cinkova, 
Maciej Eder
=======
Silvie Cinkova, Maciej Eder
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
<<<<<<< HEAD
=======
\code{\link{list_supported_language_names}}, \code{\link{list_supported_language_ids}}, \code{\link{generate_stoplist}}, \code{\link{multilingual_stoplist}} 
>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
}
\examples{
\code{## list_supported_pos()}

<<<<<<< HEAD
\code{## [1] "ADP"   "AUX"   "DET"   "INTJ"  "PART"  "PRON"  "SCONJ"}

## sample(generate_stoplist(lang = "en", pos = c("INTJ", "ADP")), 10)}

## [1] "no"       "ohh"      "across"   "farewell" "rather"   "x"       ....     

## sample(generate_stoplist(lang = "en"), 10)

##  [1] "before" "thy"  "whatever" "through"  "jeez"  "were" ..... }
=======
\code{## [[1] "ADJ"   "ADP"   "ADV"   "AUX"   "CCONJ" "DET"   "INTJ"  "NOUN"  "NUM"  
[10] "PART"  "PRON"  "PROPN" "PUNCT" "SCONJ" "SYM"   "VERB"

>>>>>>> 5ead87be4651c5a03e35d3aecade6ba7745f751a
