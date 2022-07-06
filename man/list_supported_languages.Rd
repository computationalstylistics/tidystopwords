\encoding{utf8}
\name{list_supported_languages}
\alias{list_supported_languages}

\title{
Listing of languages supported by \code{\link{list_supported_languages}} by their names and ISO-639 codes in a data frame.
}
\description{
Generate a data frame containing language names and their corresponding ISO-639 codes, with numbers of stop words for the given language 
}
\usage{
list_supported_languages()
}

\arguments{No arguments.}

%\details{
%The data frame output has the following columns:
%\enumerate{
%\item \code{lang_name}: language name (case-sensitive)
%\item \code{lang_id}: ISO-639 language code;
%\item \code{stopwords}: number of stop words found for the given language in the underlying data (\code{\link{multilingual_stoplist}}). 

%}
%}
%}

\value{

A grouped tibble (data frame) with three columns:
%\enumerate{
%\item \code{lang_name}: language name (case-sensitive)
%\item \code{lang_id}: ISO-639 language code;
%\item \code{stopwords}: number of stop words found for the given language in the underlying data (\code{\link{multilingual_stoplist}}). 

%}
}

\references{
The underlying data frame `multilingual_stoplist` is based on the official release of Version 2.8 of Universal Dependencies.

\url{https://universaldependencies.org}

Zeman, Daniel; et al., 2021, 
Universal Dependencies 2.8.1, LINDAT/CLARIAH-CZ digital library at the Institute of Formal and Applied Linguistics (ÚFAL), Faculty of Mathematics and Physics, Charles University, http://hdl.handle.net/11234/1-3687.

}



\author{
Silvie Cinková, Maciej Eder
}



\seealso{
\code{\link{generate_stoplist}}, \code{\link{multilingual_stoplist}}
}

\examples{
list_supported_languages()
}
