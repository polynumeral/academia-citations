# Academia Citation Advantage Analysis

The `acadcites` package contains the data and functions used in Niyazov, et. al. "Open Access Meets Discoverability: Citations to Articles Posted to Academia.edu."

## Installing the R Package
The easiest way to install the package and its depdendencies is by using `install_local` from the `devtools` package. (http://cran.r-project.org/web/packages/devtools/index.html)

- From R:

   ```{R}
   install.packages('devtools') 
   devtools::install_github('polynumeral/academia-citations/acadcites')
   ```

## Importing data
The cleaned/combined dataset used for the analyses can be obtained by calling:

```{R}
library('acadcites')
cites <- importData()
```

or just `cites <- acadcites::importData()` without the `library` import.

## Reproducing tables from the article

Tables from the article can be reproduced with the `makeTable` function.

```{R}
# Make Table 1 from the article.
makeTable(2, cites)

# |Journal                                                | # Articles| % Total|
# |:------------------------------------------------------|----------:|-------:|
# |Analytical Chemistry                                   |      1,537|   3.44%|
# |PLoS One                                               |        492|   1.10%|
# |Anesthesia and Analgesia                               |        430|   0.96%|
# |Biological and Pharmaceutical Bulletin                 |        362|   0.81%|
# |Analytical Methods: advancing methods and applications |        339|   0.76%|
# |Analytical Biochemistry                                |        317|   0.71%|
# |Applied Mechanics and Materials                        |        303|   0.68%|
# |Bioconjugate Chemistry                                 |        299|   0.67%|
# |Applied Physics Letters                                |        190|   0.43%|
# |BioEssays                                              |        183|   0.41%|
```


## Reproducing figures from the article
The `makeFigure` function reproduces figures from the article. Like `makeTable`,
it takes a figure number and a citations data frame.

```{r}
makeFigure(1, cites)
```


## Package help

See `help(package='acadcites')` for more help files on individual functions, or
`vignette('acadcites')` for information similar to what's provided here.


## If your R session is crashing...

Our code relies heavily on the [dplyr](https://github.com/hadley/dplyr/) library. While testing under the most recent version of the library (0.4.2), we've experienced repeated segfaults. (We're not alone---see https://github.com/hadley/dplyr/issues/1231 .)

If your R session crashes when running our code, try downgrading to version 0.4.1.

To see what version of dplyr you're running, use:

```
library('dplyr')
devtools::session_info()
```

To downgrade to version 0.4.1, use:

```
packageurl = "http://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.4.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source", dependencies = TRUE)
```
