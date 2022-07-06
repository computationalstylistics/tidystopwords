\encoding{utf8}
\name{multilingual_stoplist}
\alias{multilingual_stoplist}
\docType{data}

\title{
Multilingual Stop-Word List
}

\description{
This dataset contains a dataframe with individual word forms in rows. You can control the part of speech and various frequency counts of your desired stop-word list.
}

\format{
A data frame encoded in UTF-8, with the following columns:
%\enumerate{
%\item \code{form}: word forms in lower case. 
%\item \code{lemma}: dictionary form of the word form in lower case.
%\item \code{stopclass}: one of the following: 
\itemize{
\item \code{abbreviation}: common abbreviations acting as adverbs or adjectives, for instance *e.g., etc., cf.*;  
\item \code{adposition}: prepositions or postpositions (e.g. *in*, *ago*);
\item \code{auxiliary_verb}: auxiliary or modal verb (e.g. *would*);
\item \code{conjunction_subordinator}: coordinating or subordinating conjunctions(e.g. *and*, *because*);
\item \code{contractions}: contracted forms (e.g. *'n'* or *she'd*);
\item \code{determiner_quantifier}: pronouns, articles, pronominal adverbs, and some numerals not written as digits - all acting as adjectives or adverbs, not nouns (e.g. *yours*, *the*, *both* ,*where*, *twofold*. Cf. \code{pronominals}; 
\item \code{interjection}: words denoting sounds and performative words like *yes*, *no*, *please*, *thanks*; 
\item	\code{particle}: either preposition-like words in phrasal verbs (e.g. in English) or diverse words indicating the speaker's attitude to the statement (e.g. *fortunately*);
\item	\code{pronominal}: pronouns acting as nouns (e.g. *we* - cf. \code{determiner_quantifier})
}
%\item \code{upos}: Universal part of speech of the given word form. Cf. documentation of Universal Dependencies.
%\item \code{ufeat}: Universal Features - here a selection of the original UD ufeat string containing complementary information to the universal part of speech tags, removing morphological categories that do not affect a word's eligibility for the stopword list (for instance case, gender, number, person, and tense). 
%\item \code{n_stopclasses}: the number of stop classes to which the given combination of form and lemma belongs. If greater than 1, the given combination of form and lemma is grammatically homonymous. For instance, *that* can act as a noun (corresponding to \code{pronominal}), determiner/adjective (corresponding to \code{determiner_quantifier}), and subordinating conjunction (corresponding to \code{conjunction_subordinator}), and hence it appears in three stopclasses;  
%item code{n_formlemma}: the frequency of the given combination of form and lemma across all UD treebanks currently available for the given language;
%\item \code{n_uposformlemma}: the frequency of the given combination of upos, form, and lemma across all UD treebanks currently available for the given language;
%\item \code{lang_id}: ISO-639 language code;
%\item \code{lang_name}: language name (case-sensitive).
}

\details{
This data frame has been derived from an official release of the Universal Dependencies (UD) treebanks. 
Treebanks are text corpora with linguistic annotation. The UD syntactic annotation follows the principles of dependency syntax.
The annotation encompasses for each text token:
\itemize{
\item{relevant morphological categories;}
\item{lemma (the vocabulary form; e.g. active present infinitive in verbs)}
\item{a reference to its syntactically governing word in the clause; e.g. "house" governs "old" in "old house".}
\item{the type of the syntactic dependency between the word and its governing word; e.g. "attribute".}
}
}

\source{
The data set is based on the official release of Version 2.8.1 of the Universal Dependencies stored in the LINDAT/CLARIN digital library at the Institute of Formal and Applied Linguistics (ÚFAL), Faculty of Mathematics and Physics, Charles University, Czech Republic, \url{http://hdl.handle.net/11234/1-3687}. 
}

\references{
 
\url{https://universaldependencies.org}

Zeman, Daniel; et al., 2021, 
Universal Dependencies 2.8.1, LINDAT/CLARIAH-CZ digital library at the Institute of Formal and Applied Linguistics (UFAL), Faculty of Mathematics and Physics, Charles University, \url{http://hdl.handle.net/11234/1-3687}.

}


\keyword{datasets}
