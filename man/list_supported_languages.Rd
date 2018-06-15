\name{list_supported_languages}
\alias{list_supported_languages}
\alias{generate_stoplist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
Function to list supported languages to generate stopword lists from. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function gives you a character vector of language ids.

}
\usage{
list_supported_languages()
generate_stoplist(lang, pos = c("ADP", "AUX", "CCONJ", "DET", "INTJ", "PART", "PRON", "SCONJ"))

}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
The function lists ids of currently supported languages to fill 
the \code{lang} argument of the \code{generate_stoplist()} function.}

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

Nivre, Joakim; Agi\\u0107, \\u017deljko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (\\u00daFAL), 
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
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
\code{## list_supported_languages()}

\code{## [1] "cs"  "el"  "en"  "es"  "grc" "la"  "pl"}

## sample(generate_stoplist(lang = "en", pos = c("INTJ", "ADP")), 10)}

## [1] "no"       "ohh"      "across"   "farewell" "rather"   "x"       ....     

## sample(generate_stoplist(lang = "en"), 10)

##  [1] "before" "thy"  "whatever" "through"  "jeez"  "were" ..... }