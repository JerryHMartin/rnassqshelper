# rnassqs_helper

Helper functions to download, clean, and manage USDA NASS Quick Stats data 
in a reproducible workflow.

# Install

## From GitHub

install.packages("remotes") 
remotes::install_github("your-org/rnassqs_helper")

## From local source

devtools::load_all() 
devtools::install()

# Set NASS API Key

## For current R session:

Sys.setenv(NASS_API_KEY = "your_api_key_here") 

## For persistent use:

1.  Run: usethis::edit_r_environ()
2.  Add: NASS_API_KEY=your_api_key_here
3.  Restart R

# Quick Example

library(rnassqs_helper) 
states \<- c("TEXAS", "GEORGIA", "ALABAMA") 
res \<- rnassqs_helper::nass_monthly_county( 
  commodity = "COTTON", 
  statistic = "YIELD", 
  states = states, 
  years = 1970:2025 
) 
head(res)

# Development Commands

devtools::document() \# Update docs 
devtools::install() \# Install locally 
devtools::test() \# Run tests 
devtools::check() \# Check package 
devtools::build_vignettes() \# Build vignettes
