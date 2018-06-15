\name{list_supported_pos}
\alias{list_supported_pos}
\alias{generate_stoplist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
%%  ~~function to do ... ~~
Function to list supported parts of speech to include in stopword lists.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function gives you a character vector of parts of speech.

}
\usage{
> list_supported_pos()
[1] "ADP"   "AUX"   "DET"   "INTJ"  "PART"  "PRON"  "SCONJ"

}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
The function lists parts of speech to fill 
the \code{pos} argument of the \code{generate_stoplist()} function. 
By default, all parts of speech are included. You can narrow down the 
selection by supplying your own vector of parts of speech. }

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
Silvie Cinkova, 
Maciej Eder
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\code{## list_supported_pos()}

\code{## [1] "ADP"   "AUX"   "DET"   "INTJ"  "PART"  "PRON"  "SCONJ"}

## sample(generate_stoplist(lang = "en", pos = c("INTJ", "ADP")), 10)}

## [1] "no"       "ohh"      "across"   "farewell" "rather"   "x"       ....     

## sample(generate_stoplist(lang = "en"), 10)

##  [1] "before" "thy"  "whatever" "through"  "jeez"  "were" ..... }