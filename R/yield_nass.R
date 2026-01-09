#' Get NASS QuickStats yield data
#'
#' @description
#' Retrieve yield statistics from USDA NASS QuickStats for one or more
#' state–year combinations. This wrapper keeps core crop/soil/water
#' workflows simple and consistent while relying on `rnassqs` under the hood.
#'
#' @details
#' - Defaults align with typical yield workflows:
#'   `statistic = "YIELD"`, `agg_level = "COUNTY"`, `freq = "MONTHLY"`.
#'   You can override any of these for other aggregations or frequencies.
#' - Accepts vectors for `state` and `year`; returns a single object combining
#'   all results for the cartesian product of inputs.
#' - Use `...` to pass additional QuickStats parameters such as
#'   `county_name`, `state_alpha`, `domain_desc`, `data_item`, etc.
#' - Authentication: either call `rnassqs::nassqs_auth(key = "<your key>")`
#'   once at session start, or provide `key` to this function.
#' - To minimize overhead, set `as_tibble = FALSE` to return a base
#'   `data.frame`. The default is a tibble for tidyverse-friendly workflows.
#'
#' @section Authentication:
#' The function will call `rnassqs::nassqs_auth(key = key)` if a non-empty
#' `key` is provided; otherwise, ensure you have authenticated earlier
#' in your session via `rnassqs::nassqs_auth()` and/or an environment variable.
#'
#' @param state Character vector of full state names as recognized by NASS
#'   (e.g., `"TEXAS"`, `"GEORGIA"`, `"SOUTH CAROLINA"`). Whitespace is trimmed.
#' @param year Integer or numeric vector of years (coerced to integer).
#' @param commodity Commodity description string. Default `"COTTON"`.
#'   Passed to `commodity_desc`.
#' @param statistic Statistic category. Default `"YIELD"`.
#'   Passed to `statisticcat_desc`.
#' @param agg_level Aggregation level description. Default `"COUNTY"`.
#'   Passed to `agg_level_desc`.
#' @param freq Frequency description. Default `"MONTHLY"`.
#'   Passed to `freq_desc`.
#' @param source_desc Source description. Default `"SURVEY"`.
#' @param sector_desc Sector description. Default `"CROPS"`.
#' @param group_desc Group description. Default `"FIELD CROPS"`.
#' @param key Optional NASS API key. If supplied and non-empty, the function
#'   will call `rnassqs::nassqs_auth(key = key)` before querying.
#' @param add_missing_cols Logical; if `TRUE`, ensures `state_name` and `year`
#'   columns exist in the returned data even if the API omitted them. Default `TRUE`.
#' @param sleep Numeric; seconds to pause between requests to be gentle with
#'   the API. Set to `0` to disable. Default `0.25`.
#' @param as_tibble Logical; if `TRUE` (default), return a tibble; if `FALSE`,
#'   return a base `data.frame`.
#' @param ... Additional named parameters forwarded to `rnassqs::nassqs()`
#'   (e.g., `county_name = "DORCHESTER"`, `state_alpha = "SC"`,
#'   `data_item = "COTTON, UPLAND - YIELD, MEASURED IN LB / ACRE"`).
#'
#' @return
#' A tibble (default) or base `data.frame` with zero or more rows, depending on
#' `as_tibble`. If a given query returns no records or an error occurs, an empty
#' object is returned for that query; all results are row-bound into a single
#' result.
#'
#' @examples
#' \donttest{
#' # Authenticate once (preferred)
#' rnassqs::nassqs_auth(Sys.getenv("NASS_API_KEY"))
#'
#' # Single state-year, default yield/monthly/county for cotton (tibble return)
#' tx_2010 <- get_nass_yield(state = "TEXAS", year = 2010)
#'
#' # Multi-state, multi-year (cartesian product), return data.frame
#' dat_df <- get_nass_yield(
#'   state = c("TEXAS", "GEORGIA"),
#'   year  = 2015:2016,
#'   as_tibble = FALSE
#' )
#'
#' # Filter to a specific county via `...`
#' sc_dorchester_2020 <- get_nass_yield(
#'   state = "SOUTH CAROLINA", year = 2020,
#'   county_name = "DORCHESTER"
#' )
#'
#' # Supply a key directly (alternative to calling nassqs_auth)
#' dat2 <- get_nass_yield(
#'   state = "TEXAS", year = 2018,
#'   key = Sys.getenv("NASS_API_KEY")
#' )
#' }
#'
#' @seealso [rnassqs::nassqs()]
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
get_nass_yield <- function(state, year,
                           commodity   = "COTTON",
                           statistic   = "YIELD",
                           agg_level   = "COUNTY",
                           freq        = "MONTHLY",
                           source_desc = "SURVEY",
                           sector_desc = "CROPS",
                           group_desc  = "FIELD CROPS",
                           key = NULL,
                           add_missing_cols = TRUE,
                           sleep = 0.25,
                           as_tibble = TRUE,
                           ...) {
  
  # ---- Authentication (optional) ----
  # If the user provided an API key, authenticate rnassqs with it.
  # This allows the function to be used without requiring a prior call
  # to rnassqs::nassqs_auth() in the session.
  if (!is.null(key) && nzchar(key)) {
    rnassqs::nassqs_auth(key = key)
  }
  
  # ---- Input validation and normalization ----
  # Ensure mandatory arguments are present.
  if (missing(state) || missing(year)) {
    stop("`state` and `year` are required.", call. = FALSE)
  }
  # Trim whitespace from state names to avoid mismatches (e.g., " TEXAS " -> "TEXAS").
  state <- trimws(state)
  # Coerce years to integers to match the API expectations (numeric vectors are common in R).
  year  <- as.integer(year)
  # If any years become NA after coercion, signal an error.
  if (any(is.na(year))) {
    stop("`year` must be coercible to integer.", call. = FALSE)
  }
  
  # ---- Build the query grid (cartesian product) ----
  # Create all combinations of state × year so a single call can process
  # multiple states and multiple years. This keeps the API interaction
  # simple and predictable.
  grid <- expand.grid(state = state, year = year, stringsAsFactors = FALSE)
  
  # ---- Prepare a container for per-query results ----
  # We'll store each API response (a data frame/tibble) in a list and
  # row-bind them at the end for a single combined result.
  results <- vector("list", nrow(grid))
  
  # ---- Iterate over each state-year pair and query NASS ----
  for (i in seq_len(nrow(grid))) {
    
    # Assemble the parameter list for rnassqs::nassqs().
    # The names here match NASS QuickStats API fields exactly:
    # - *_desc fields are categorical filters (e.g., source_desc = "SURVEY").
    # - state_name and year select the geographic/temporal subset.
    # The `...` allows callers to provide additional filters (e.g., county_name).
    params <- c(
      list(
        source_desc       = source_desc,       # Data source (SURVEY vs CENSUS)
        sector_desc       = sector_desc,       # Sector (CROPS, ANIMALS, ECONOMICS...)
        group_desc        = group_desc,        # Group within sector (FIELD CROPS)
        commodity_desc    = commodity,         # Commodity of interest (e.g., COTTON)
        statisticcat_desc = statistic,         # Statistic category (e.g., YIELD)
        agg_level_desc    = agg_level,         # Aggregation level (e.g., COUNTY)
        freq_desc         = freq,              # Reporting frequency (e.g., MONTHLY)
        state_name        = grid$state[i],     # Full state name (NASS expects uppercase)
        year              = grid$year[i]       # Calendar year of interest
      ),
      # Additional filters provided by the caller go here (e.g., county_name)
      list(...)
    )
    
    # Perform the API call. If rnassqs throws (network error, bad params),
    # return an empty tibble so the loop continues gracefully.
    out <- tryCatch(
      rnassqs::nassqs(params),
      error = function(e) tibble::tibble()
    )
    
    # Ensure consistency: some responses may omit state_name or year.
    # If requested, add them back so downstream code can rely on their presence.
    if (add_missing_cols && nrow(out) > 0) {
      if (!"state_name" %in% names(out)) out$state_name <- grid$state[i]
      if (!"year" %in% names(out))       out$year       <- grid$year[i]
    }
    
    # Be gentle with the API: small inter-request delay to avoid rate limits.
    # Set `sleep = 0` if you need maximum throughput.
    if (is.numeric(sleep) && sleep > 0) {
      Sys.sleep(sleep)
    }
    
    # Store the per-query result (may be empty) in the results list.
    results[[i]] <- out
  }
  
  # ---- Combine all responses into a single object ----
  # Bind rows from all queries into a single tibble (default) or data.frame.
  combined <- dplyr::bind_rows(results)
  
  # Convert to base data.frame if the caller wants to minimize overhead
  # or avoid a tibble dependency in their downstream pipeline.
  if (!as_tibble) combined <- as.data.frame(combined)
  
  # Return the final combined object.
  combined
}
  