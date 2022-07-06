\encoding{utf8}
\name{list_supported_language_ids}
\alias{list_supported_language_ids}

\title{
Listing of language ids to include in stopword lists you generate by `generate_stoplist()`.
}

\description{
The function gives you a character vector of supported language ids, e.g. "en", "cs", "pl". 
}

\usage{
list_supported_language_ids()
}

\details{
The \code{stopwoRds} package relies on \code{multilingual_stoplist}, a large multilingual table with individual word forms as rows, derived from the Universal Dependencies treebanks. Each word form comes along with its lemma and part of speech, as well as with the language name and its ISO-639 code. This function gives you unique values from the \code{language_id} column of \code{multilingual_stoplist}. The current ids are a mix of different versions of ISO-639 language codes.
}

\value{
A character vector.
}

\references{
\url{http://universaldependencies.org}

Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), 
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
}

\author{
Silvie Cinková, Maciej Eder
}

\seealso{
\code{\link{list_supported_pos}}, \code{\link{list_supported_language_ids}}, \code{\link{generate_stoplist}}, \code{\link{multilingual_stoplist}} 
}


