# easyPubMed fork (dev version)

With support added for scraping PubMed "My Bibliography" links, and (in the future) visualizations. 

## Installation

Install this fork from GitHub with:

```{r, eval = FALSE}
# install.packages("devtools")
devtools::install_github("ssmithm/easyPubMed")
```

## Usage

Explore the [easyPubMed vignettes](https://www.data-pulse.com/dev_site/easypubmed/) for use of the main package.

For scraping My Bibliography, the main function is `get_pubmed_bib()` which accepts the base URL of a My Bibliography page, and outputs a data.frame of citation data. 
