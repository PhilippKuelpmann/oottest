# oottest
Out of treatment testing R package

## TODOs

### Philipp
- [ ] Send a single function to test

### Now
- [ ] (maybe) Read [PKGS help](https://r-pkgs.org/intro.html) (Ch 1-3)
- [ ] Set up RStudio Project + test package (with the function from Philipp): [instructions sec 1-5](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package)
- [ ] Add [Styler](https://style.tidyverse.org/) to RStudio Project
- [ ] Add basic functions
- [ ] (maybe) Set up CI (Github actions): [CI Guide, 2.3.1](https://devguide.ropensci.org/ci.html#ci)

## Roadmap
- [ ] Set up RStudio Project -> Github
- [ ] Set up "empty/test" package
- [ ] Set up CI: https://devguide.ropensci.org/ci.html
- [ ] Add basic oot_test function (+ everything that is required for it) (Philipp)
- [ ] add internal datasets (HDG 2x2, 3x3 & random?)
- [ ] add tests https://r-pkgs.org/tests.html / https://r-pkgs.org/r-cmd-check.html#r-cmd-check
- [ ] Comments/Documentation
- [ ] DESCRIPTION, help file, NEWS
- [ ] Licence (GPL-2 or GPL-3?)
- [ ] Add all other functions
- [ ] Submit to rOpenSci -> CRAN -> Journal? https://stats-devguide.ropensci.org/pkgdev.html
- [ ] follow [Journal submission guide](https://www.jstatsoft.org/pages/view/authors)

### Optional
- [ ] Vignette?
- [ ] Page for package: https://enpiar.com/2017/11/21/getting-down-with-pkgdown/?


### Broader picture
- [ ] Set up environment + Project + "empty/test" package (including CI)
- [ ] Add basic functions (using rOpenSci best practices)
- [ ] Tests, documentation, etc (using rOpenSci best practices)
- [ ] Check against rOpenSci checklist
- [ ] Add other functions
- [ ] Submit to rOpenSci
- [ ] Sublish on CRAN
- [ ] Send to journal


## Random notes

### Submit to rOpenSci and a journal?
We strongly suggest submitting your package for review before publishing on CRAN or submitting a software paper describing the package to a journal. Review feedback may result in major improvements and updates to your package, including renaming and breaking changes to functions. We do not consider previous publication on CRAN or in other venues sufficient reason to not adopt reviewer or editor recommendations.



https://devguide.ropensci.org/building.html
https://devguide.ropensci.org/marketing.html
https://devguide.ropensci.org/grooming.html

### JSS recommendations
- For R packages, we encourage inclusion of JSS submissions as vignettes in the package.
- For R packages with functions that create compound objects, a minimum expectation is that R's classes and methods systems are leveraged, e.g., by making use of S3 classes and providing standard methods (such as print, plot, and summary).
- Software authors may also find it helpful to follow the advice in the guidelines described in the rOpenSci Statistical Software Peer Review book. It is not necessary for a JSS submission to fully conform with all the standards described there, though.
- Code needs to include the GNU General Public Licence (GPL), versions GPL-2 or GPL-3, or a GPL-compatible license for publication in JSS.

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


### rOpenSci
https://stats-devguide.ropensci.org/
https://devguide.ropensci.org/guide-for-authors.html

### Journals
https://www.jstatsoft.org                     The Journal of Statistical Software
https://journal.r-project.org/                The R Journal
https://openresearchsoftware.metajnl.com/     The Journal of Open Research Software
https://joss.theoj.org/                       The Journal of Open Source Software
https://www.journals.elsevier.com/softwarex   SoftwareX
