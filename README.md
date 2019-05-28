# ckbplotr

The goal of ckbplotr is to provide useful function for creating plots in R.

## Installation

To install the package, the
R packages `ggplot2`, `magrittr`, `readr`, `tibble`, `dplyr`, `purrr` and `rlang` must 
first be installed. These are part of the collection of tidyverse packages.
```{r, eval = FALSE}
# The easiest way is to install the whole tidyverse:
install.package("tidyverse")

# # Or install just these packages:
# install.packages(c("ggplot2", "readr", "dplyr", "purrr"))
```
Then `ckbplot` can be installed from its source package using the code:
```{r, eval = FALSE}
install.packages("ckbplotr.tar.gz", repos = NULL, type = "source")
```
Or, in RStudio, open the "Tools" menu and select "Install Packages...". In the "Install from..." box select "Package Archive File", and in the "Package archive" box browse to the ckbplotr.tar.gz file.
