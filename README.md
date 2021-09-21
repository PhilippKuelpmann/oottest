# oottest
Out of treatment testing R package

### TODOs
- [ ] Find guide to writing R (non tidyverse?)
- [ ] Check [PKGS help](https://r-pkgs.org/intro.html)
- [ ] Use [Tidyverse codestyle](https://style.tidyverse.org/)? Using [Styler](https://style.tidyverse.org/)
- [ ] Add all necessary functions
- [ ] Add internal dataset (HDG & random?)
- [ ] Add tests - https://r-pkgs.org/tests.html / https://r-pkgs.org/r-cmd-check.html#r-cmd-check
- [ ] fix DESCRIPTION
- [ ] add vignette?
- [ ] follow [CRAN instructions](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package)
- [ ] follow [Journal submission guide](https://www.jstatsoft.org/pages/view/authors)



## Random notes
### Packages to load
```R
library(roxygen2) # In-Line Documentation for R 
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(covr)
library(usethis)  # Automate Package and Project Setup
```

### Name avalability
```R
library(available)
available::available("oottR", browse = FALSE)
```

### Docs
```R
devtools::document()
```


### News
```R
usethis::use_news_md
```


### Tests

Generate the test environment usethis::use_testthat. This generates a tests/ folder with another folder called testthat/ that later contains your tests as well as an R file testthat.R. We will only add tests to the tests/testthat/ folder and do not touch the R file.
Add test(s) as .R files. The filename does not matter, just choose whatever you find reasonable.
Run the tests using devtools::test(). To get an estimation of your test coverage, you can use devtools::test_coverage().


CRAN


rOpenSci
https://stats-devguide.ropensci.org/
https://devguide.ropensci.org/guide-for-authors.html

Journals:
  The Journal of Statistical Software
https://journal.r-project.org/                The R Journal
https://openresearchsoftware.metajnl.com/     The Journal of Open Research Software
https://joss.theoj.org/                       The Journal of Open Source Software
https://www.journals.elsevier.com/softwarex   SoftwareX
