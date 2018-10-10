\encoding{utf8}
\name{multilingual_stoplist}
\alias{multilingual_stoplist}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Multilingual stoplist
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("multilingual_stoplist")}
\format{
  A data frame with 213,526 observations on the following 10 variables, encoded in UTF-8.
  \describe{
    \item{\code{language_id}}{a character vector}
    \item{\code{language_name}}{a character vector}
    \item{\code{POS}}{a character vector}
    \item{\code{UFeat}}{a character vector}
    \item{\code{lemma}}{a character vector}
    \item{\code{word_form}}{a character vector}
    \item{\code{freq_formUFeatPOS}}{an integer vector}
    \item{\code{freq_formPOS}}{an integer vector}
    \item{\code{freq_form}}{an integer vector}
    \item{\code{prop_wforpos_wform}}{an integer vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
This data frame has been derived from an official release of the Universal Dependencies (UD) treebanks. 
Treebanks are text corpora with linguistic annotation. The UD syntactic annotation follows principles of dependency syntax.
The annotation encompasses for each text token:
\itemize{
\item{relevant morphological categories};
\item{lemma (the vocabulary form; e.g. active present infinitive in verbs)}
\item{a reference to its syntactically governing word in the clause; e.g. "house" governs "old" in "old house".}
\item{the type of the syntactic dependency between the word and its governing word; e.g. "attribute".}
}. 

The morphological categories in UD are divided into two groups: the coarse-grained, cross-linguistically universal part-of-speech tags, and the Universal Features. 


\code{language_id} - a mix of different versions of ISO-639 language codes as they were used in the source corpora;

\code{language_name} - the English name of the language, starting with capital letter;

\code{POS} - Universal part-of-speech tag;

\code{UFeat} - Universal Features tags separated by \code{|}; this is more fine-grained information derived from the national morphological tagsets that is too specific to apply to all languages. However, even these tags are cross-linguistically coordinated to ensure maximum cross-linguistic uniformity in markup.   

\code{lemma} - the basic (vocabulary) word form;

\code{word_form};

\code{freq_formUFeatPOS} - frequency of the given combination of word form, Universal Features and Universal Part of Speech. All frequencies are valid within the given language. Cross-lingual homonyms are kept separate (e.g. the preposition "in" existent in several Germanic languages is counted for each language separately, despite identical morphological markup) );

\code{freq_formPOS} - frequency of the given combination of word form and Universal Part of Speech for the given language;

\code{freq_form} - frequency of the given word form for the given language. (Mind that this number does not come from the addition of the former two!);

\code{prop_wforpos_wform} - proportion of the given Universal Part of Speech in the given word form in the given language. This can be helpful when you decide whether to include a word in your list that is homonymous - considering the proportions gives you a hint on how much content you lose vs. how much noise you preserve. Remember to check the UD documentation of the individual corpora to assess whether their corpora are comparable to your texts, though!
}
\source{
The data set is based on the official release of Version 2.1 of Universal Dependencies stored in the LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), Faculty of Mathematics and Physics, Charles University, Czech Republic, \url{http://hdl.handle.net/11234/1-2515}. 
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
To perform more sophisticated searches in this data frame, check the UD documentation. We have included all parts of speech except \code{X} (unknown POS). You can e.g. add to your stoplist all nouns above or below a given frequency threshold. 
%%  ~~ possibly secondary sources and usages ~~
\url{http://universaldependencies.org}

Nivre, Joakim; Agić, Željko; Ahrenberg, Lars; et al., 2017, 
Universal Dependencies 2.1, LINDAT/CLARIN digital library 
at the Institute of Formal and Applied Linguistics (ÚFAL), 
Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-2515}. 
}
\examples{
data(multilingual_stoplist)
## maybe str(multilingual_stoplist) ; plot(multilingual_stoplist) ...
\dontrun{
Observations: 213,526Variables: 10
$ language_id         <chr> "af", "af", "af", "af", "af", "af", "af...
$ language_name       <chr> "Afrikaans", "Afrikaans", "Afrikaans", ...
$ POS                 <chr> "ADJ", "ADJ", "ADJ", "ADJ", "ADJ", "ADJ...
$ UFeat               <chr> "AdjType=Attr|Case=Nom|Degree=Cmp", "Ad"..
$ lemma               <chr> "goed", "addisioneel", "afloop", "agbaa...
$ word_form           <chr> "beter", "addisionele", "afgelope", "ag...
$ freq_formUFeatPOS   <int> 12, 19, 23, 12, 117, 11, 22, 13, 17, 11...
$ freq_formPOS        <int> 13, 19, 23, 12, 117, 11, 22, 13, 21, 11...
$ freq_form           <int> 13, 19, 23, 12, 117, 11, 22, 13, 21, 11...
$ prop_wformpos_wform <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
}

}
\keyword{datasets}
