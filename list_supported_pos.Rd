\encoding{utf8}
\name{list_supported_pos}
\alias{list_supported_language_names}
\alias{list_supported_language_ids}
\alias{list_supported_pos}
\alias{generate_stoplist}
\alias{multilingual_stoplist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
Listing of parts of speech to include in stopword lists you generate by  \code{generate_stoplist()}.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function gives you a character vector of supported parts of speech (e.g. prepositions). They are represented by abbreviations.
}
\usage{
list_supported_pos()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
The \code{stopwoRds} package relies on \code{multilingual_stoplist}, a large multilingual table with individual word forms as rows, derived from the Universal Dependencies treebanks. Each word form comes along with its lemma and part of speech, as well as with the language name and its ISO-639-nnnn code. This function gives you unique values from the \code{POS} column of \code{multilingual_stoplist}.
The parts of speech (POS) are common for all supported languages ("Universal Part-of-Speech tags"). 

}


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

Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), 
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
%% ~put references to the literature/web site here ~
}

\author{
%%  ~~who you are~~
Silvie Cinkova, Maciej Eder
}



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{list_supported_language_names}}, \code{\link{list_supported_language_ids}}, \code{\link{generate_stoplist}}, \code{\link{multilingual_stoplist}} 
}
\examples{
list_supported_pos()
}

