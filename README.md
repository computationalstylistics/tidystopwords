# stopwoRds

An R package containing Lists of stopwords in a few languages.

The idea behind this package is to give the user control over the stop-word 
selection. The core `generate_stoplist` function relies on 
`multilingual_stopwords`, a large data frame derived from the current 
release of the Universal Dependencies Treebanks. We have included all languages 
whose corpora totalled above 10,000 tokens – large enough to cover all common 
closed-class words, such as prepositions, conjunctions, and auxiliary verbs.
The data comes encoded in UTF-8. 
