# rnassqs_helper

Helper functions to download, clean, and manage USDA NASS Quick Stats data in a compact, reproducible workflow.

---

## Overview

**rnassqs_helper** provides small, focused utilities that simplify common NASS Quick Stats tasks such as querying monthly county‑level data, cleaning column names, and handling basic pagination and authentication. The package is intentionally minimal: add a vignette for each real use case and extract helper functions only when they are repeatedly useful.

---

## Installation

### From GitHub (recommended for development and internal use)

```r
# install remotes if needed
install.packages("remotes")

# install the package from your GitHub repo
remotes::install_github("your-org/rnassqs_helper")
```

### From local source (during development)

```r
# from inside the package project
devtools::load_all()
devtools::install()
```

### Dependencies

Install required packages if not already present:

```r
install.packages(c(
  "httr2", "jsonlite", "dplyr", "tidyr",
  "purrr", "tibble", "stringr", "rnassqs"
))
```

---

## Quick Start

Set your NASS API key as an environment variable before running queries:

```r
# set for the current session
Sys.setenv(NASS_API_KEY = "your_api_key_here")

# or add to your .Renviron for persistent use
# edit .Renviron and add: NASS_API_KEY=your_api_key_here
```

### Basic example using the package workflow

```r
library(rnassqs_helper)

# example state block
states <- c("TEXAS", "GEORGIA", "ALABAMA")

# simple query for monthly county cotton yield 1970-2025
res <- rnassqs_helper::nass_monthly_county(
  commodity = "COTTON",
  statistic = "YIELD",
  states = states,
  years = 1970:2025
)

# inspect results
head(res)
```

> **Note:** The example above is a minimal workflow. Replace `nass_monthly_county` with the actual helper function name once implemented; the vignette contains the working code used during development.

---

## Vignettes and Examples

Vignettes demonstrate real use cases and are the recommended way to learn package workflows.

- **Primary vignette:** `monthly-county-cotton-yield` — shows how to download monthly county‑level cotton yield for a block of states from 1970 to 2025.

To build and view vignettes locally:

```r
devtools::build_vignettes()
browseVignettes(package = "rnassqs_helper")
```

---

## Testing and Development

Run unit tests:

```r
devtools::test()
```

Document and update NAMESPACE:

```r
devtools::document()
```

Check the package:

```r
devtools::check()
```

Follow the vignette‑first workflow: write a vignette for a real task, extract repeated code into a helper function, add tests for that function, then document and export it.

---

## Contributing and License

- **License:** MIT. See the `LICENSE` file for details.  
- **Contributing:** Open an issue or submit a pull request in the repository. Keep changes small and focused, include tests for new behavior, and update or add a vignette when introducing new workflows.

---

## Contact

If you need help or want to propose a new vignette or helper function, open an issue in the repository or contact the package maintainer listed in `DESCRIPTION`.

---

## Quick reference commands

```r
# document and install locally
devtools::document()
devtools::install()

# run tests
devtools::test()

# run a vignette example interactively
rmarkdown::run("vignettes/monthly-county-cotton-yield.Rmd")
```






