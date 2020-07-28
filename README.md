# The Languages of Frida Kahlo

## The study

This project **compares the Spanish and English subtitles from the movie [Frida (2002)](https://www.imdb.com/title/tt0120679/)**.

## The data

There are **two subtitle files**:

* ***Frida_English.txt***: Subtitles in English.
* ***Frida_Spanish.txt***: Subtitles in Spanish.

## The analysis

The texts from the subtitle files were analyzed using different **Natural Language Processing (NLP)** tools and techniques:

* Tokenization
* Bi-grams
* Removing stopwords
* Word frequency analysis
* Sentiment analysis

Several **graphs** were created and can be found [here](https://view.genial.ly/5f205f79204af70d99163918/presentation-the-languages-of-frida-kahlo).

## Technical information

All analyses were conducted using [R](https://www.r-project.org) running in [RStudio](https://rstudio.com).

Packages used include:

* [*tidyverse*](https://www.tidyverse.org)
* [*tidytext*](https://cran.r-project.org/web/packages/tidytext/index.html)
* [*stringi*](https://cran.r-project.org/web/packages/stringi/index.html)
* [*igraph*](https://igraph.org/r/)
* [*ggraph*](https://cran.r-project.org/web/packages/ggraph/index.html)
* [*syuzhet*](https://cran.r-project.org/web/packages/syuzhet/index.html)

The code is documented in an R file.
