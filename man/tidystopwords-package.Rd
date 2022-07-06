\encoding{utf8}
\name{tidystopwords}
\alias{tidystopwords-package}
\alias{tidystopwords}
\docType{package}
\title{
Customisable Lists of Stop-Words in 110 Languages
}

\description{
The idea behind this package is to give the user control over the stop-word selection.
}

\details{
The idea behind this package is to give the user control over the stop-word 
selection. The core \code{generate_stoplist} function relies on 
\code{multilingual_stopwords}, a large data frame derived from the current 
release of the Universal Dependencies Treebanks. We have included all languages 
whose corpora totalled above 10,000 tokens -- large enough to cover all common 
closed-class words, such as prepositions, conjunctions, and auxiliary verbs.
The data comes encoded in UTF-8. 
}


\author{
Silvie Cinková, Maciej Eder
}

\seealso{
\code{\link{list_supported_languages}}, \code{\link{multilingual_stoplist}}
}

\references{
The data set is based on the official release of Version 2.1 of Universal Dependencies.

\url{https://universaldependencies.org}

Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, Universal Dependencies 2.1, LINDAT/CLARIN digital library at the Institute of Formal and Applied Linguistics (ÚFAL), Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}.  
}
