
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
#' - Progress:
#'   * By default, shows one cumulative progress bar (`progress = TRUE`).
#'   * `verbose = TRUE` prints the current state-year and a simple ETA.
#'   * `use_progressr = TRUE` uses the {progressr} ecosystem for richer,
#'     customizable progress UIs (falls back to base progress bar if unavailable).
#' - `rnassqs_progress_bar = FALSE` disables rnassqs’ *internal* per-call bar,
#'   preventing repeated `100%` lines during bulk downloads. (See rnassqs docs.) 
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
#' @param progress Logical; if `TRUE`, display one cumulative progress bar
#'   for all queries. Default `TRUE`.
#' @param verbose Logical; if `TRUE`, print the current state-year being queried
#'   and a simple ETA. Default `FALSE`.
#' @param use_progressr Logical; if `TRUE`, use the {progressr} framework
#'   for progress (requires the package). Default `FALSE`.
#' @param rnassqs_progress_bar Logical; passed to `rnassqs::nassqs(progress_bar = ...)`.
#'   Default `FALSE` to suppress rnassqs’ internal bar so only the cumulative bar shows.
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
#'   as_tibble = FALSE,
#'   progress  = TRUE,
#'   verbose   = TRUE
#' )
#'
#' # With {progressr} (if installed) for richer progress output:
#' # progressr::handlers(global = TRUE)  # choose a handler (e.g., cli, rstudio)
#' # dat <- get_nass_yield(
#' #   state = c("SOUTH CAROLINA", "GEORGIA"),
#' #   year  = 2018:2019,
#' #   use_progressr = TRUE
#' # )
#'
#' # Supply a key directly (alternative to calling nassqs_auth)
#' dat2 <- get_nass_yield(
#'   state = "TEXAS", year = 2018,
#'   key = Sys.getenv("NASS_API_KEY")
#' )
#' }
#'
#' @seealso [rnassqs::nassqs()] for the underlying API call and its `progress_bar`
#'   parameter; [progressr::progressor()] for customizable progress reporting.
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom utils txtProgressBar setTxtProgressBar
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
                           progress = TRUE,
                           verbose = FALSE,
                           use_progressr = FALSE,
                           rnassqs_progress_bar = FALSE,
                           ...) {
  
  # ---- Authentication (optional) ----
  if (!is.null(key) && nzchar(key)) {
    rnassqs::nassqs_auth(key = key)
  }
  
  # ---- Input validation and normalization ----
  if (missing(state) || missing(year)) {
    stop("`state` and `year` are required.", call. = FALSE)
  }
  state <- trimws(state)
  year  <- as.integer(year)
  if (any(is.na(year))) {
    stop("`year` must be coercible to integer.", call. = FALSE)
  }
  
  # ---- Build the query grid (cartesian product) ----
  grid  <- expand.grid(state = state, year = year, stringsAsFactors = FALSE)
  total <- nrow(grid)
  
  # ---- Progress setup ----
  # Prefer {progressr} when requested AND available; otherwise use base progress bar.
  use_pr <- isTRUE(use_progressr) && requireNamespace("progressr", quietly = TRUE)
  
  # If using base progress bar:
  if (!use_pr && isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    on.exit({
      # Ensure the bar closes even if an error occurs.
      try(close(pb), silent = TRUE)
    }, add = TRUE)
  }
  
  start_time <- Sys.time()
  results    <- vector("list", total)
  
  # ---- Helper: update base progress bar and verbose ETA ----
  .update_progress <- function(i) {
    if (!use_pr && isTRUE(progress)) utils::setTxtProgressBar(pb, i)
    if (isTRUE(verbose)) {
      elapsed   <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      avg_per   <- elapsed / i
      remaining <- avg_per * (total - i)
      message(sprintf("  ↳ Done. Elapsed: %.1fs | ETA: %.1fs", elapsed, remaining))
    }
  }
  
  # ---- Query loop ----
  if (use_pr) {
    # With progressr, open a progress context and emit step updates with a message.
    progressr::with_progress({
      p <- progressr::progressor(steps = total)
      for (i in seq_len(total)) {
        cur_state <- grid$state[i]
        cur_year  <- grid$year[i]
        
        if (isTRUE(verbose)) {
          message(sprintf("[%-4d/%4d] Querying %s %d ...", i, total, cur_state, cur_year))
        }
        p(sprintf("Querying %s %d", cur_state, cur_year))
        
        params <- c(
          list(
            source_desc       = source_desc,
            sector_desc       = sector_desc,
            group_desc        = group_desc,
            commodity_desc    = commodity,
            statisticcat_desc = statistic,
            agg_level_desc    = agg_level,
            freq_desc         = freq,
            state_name        = cur_state,
            year              = cur_year
          ),
          list(...)
        )
        
        out <- tryCatch(
          rnassqs::nassqs(params, progress_bar = rnassqs_progress_bar),
          error = function(e) {
            if (isTRUE(verbose)) {
              message(sprintf("  ↳ Error for %s %d: %s", cur_state, cur_year, conditionMessage(e)))
            }
            tibble::tibble()
          }
        )
        
        if (add_missing_cols && nrow(out) > 0) {
          if (!"state_name" %in% names(out)) out$state_name <- cur_state
          if (!"year"       %in% names(out)) out$year       <- cur_year
        }
        
        results[[i]] <- out
        .update_progress(i)
        
        if (is.numeric(sleep) && sleep > 0) Sys.sleep(sleep)
      }
    })
  } else {
    # Base progress bar path (no extra dependency)
    for (i in seq_len(total)) {
      cur_state <- grid$state[i]
      cur_year  <- grid$year[i]
      
      if (isTRUE(verbose)) {
        message(sprintf("[%-4d/%4d] Querying %s %d ...", i, total, cur_state, cur_year))
      }
      
      params <- c(
        list(
          source_desc       = source_desc,
          sector_desc       = sector_desc,
          group_desc        = group_desc,
          commodity_desc    = commodity,
          statisticcat_desc = statistic,
          agg_level_desc    = agg_level,
          freq_desc         = freq,
          state_name        = cur_state,
          year              = cur_year
        ),
        list(...)
      )
      
      out <- tryCatch(
        rnassqs::nassqs(params, progress_bar = rnassqs_progress_bar),
        error = function(e) {
          if (isTRUE(verbose)) {
            message(sprintf("  ↳ Error for %s %d: %s", cur_state, cur_year, conditionMessage(e)))
          }
          tibble::tibble()
        }
      )
      
      if (add_missing_cols && nrow(out) > 0) {
        if (!"state_name" %in% names(out)) out$state_name <- cur_state
        if (!"year"       %in% names(out)) out$year       <- cur_year
      }
      
      results[[i]] <- out
      .update_progress(i)
      
      if (is.numeric(sleep) && sleep > 0) Sys.sleep(sleep)
    }
  }
  
  # ---- Combine results ----
  combined <- dplyr::bind_rows(results)
  if (!as_tibble) combined <- as.data.frame(combined)
  
  combined
}
