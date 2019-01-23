# tidystopwords: R package for multilingual stopwords

**Authors:** Silvie Cinková<sup>*</sup>, Maciej Eder<br/>
**License:** [GPL-3](https://opensource.org/licenses/GPL-3.0)

An R package containing customizable lists of stopwords in multiple languages; it attempts to follow [tidy data principles](https://www.jstatsoft.org/article/view/v059i10).

The idea behind this package is to give the user control over the stopword selection. The core `generate_stoplist()` function relies on  `multilingual_stopwords()`, a large data frame derived from the current release of the Universal Dependencies Treebanks. We have included all languages whose corpora totalled above 10,000 tokens – large enough to cover all common closed-class words, such as prepositions, conjunctions, and auxiliary verbs. The data comes encoded in UTF-8. 

## Installation

Install the package directly from the GitHub repository:

```
library(devtools)
install_github("computationalstylistics/stopwoRds", build_vignettes = TRUE)
```

