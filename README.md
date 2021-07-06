# tidystopwords: R package for multilingual stopwords

**Authors:** Silvie Cinková<sup>*</sup>, Maciej Eder<br/>
**License:** [GPL-3](https://opensource.org/licenses/GPL-3.0)

An R package containing customizable lists of stopwords in multiple languages; it attempts to follow [tidy data principles](https://www.jstatsoft.org/article/view/v059i10).

The idea behind this package is to provide stopwords for less-resourced languages as well as give the user control over the stopword selection with respect to parts of speech. For the purposes of this package, stopwords are defined as forms of function words from closed parts of speech (e.g. prepositions, conjunctions, auxiliary verbs, and pronouns). The core `generate_stoplist()` function relies on  `multilingual_stopwords()`, a large data frame derived from the current release of the Universal Dependencies Treebanks. We have included all languages. The data comes encoded in UTF-8. The vocabulary coverage for each language depends on the size, textual diversity, and annotation quality of the available treebanks. No manual post-editing was performed. 

## Installation

Install the package directly from the GitHub repository:

```
library(devtools)
install_github("computationalstylistics/stopwoRds", build_vignettes = TRUE)
```

