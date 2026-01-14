
#' @title Set and Retrieve the USDA NASS API Key
#' @name nass_api_key
#'
#' @description
#' Lightweight helpers to set, persist, and retrieve the `NASS_API_KEY`
#' used by USDA NASS Quick Stats API via `{rnassqs}`. Designed for local,
#' personal workflows with minimal ceremony, consistent with Occam's razor.
#'
#' @details
#' - `set_nass_key()` sets the key for the current session, your process env,
#'   or writes it to `~/.Renviron` (or a custom path). It optionally calls
#'   `rnassqs::nassqs_auth()` to authenticate immediately.
#' - `get_nass_key()` retrieves the key from the session (`rnassqs` auth),
#'   environment variables, or by reading `.Renviron`.
#' - `ensure_nass_key()` is a convenience wrapper that tries to get a key
#'   (from session/env/`.Renviron`) and, if found, authenticates it. Use this
#'   in vignettes and scripts for a clean, single-line setup.
#'
#' Security note: messages never print the full key; they show a masked preview.
#'
#' @section Sources checked by `get_nass_key()`:
#' 1. "session": if `{rnassqs}` is installed and has an active key
#' 2. "env": `Sys.getenv("NASS_API_KEY")`
#' 3. "renviron": load from `~/.Renviron` (or a custom path) via `readRenviron()`
#'
#' @seealso [rnassqs::nassqs_auth]
#' @examples
#' \dontrun{
#' # Set for the session only (recommended for interactive use)
#' set_nass_key("abcdef123456", persist = "session")
#'
#' # Persist to environment (this R process only)
#' set_nass_key("abcdef123456", persist = "env")
#'
#' # Persist to ~/.Renviron (survives restarts)
#' set_nass_key("abcdef123456", persist = "renviron", overwrite = TRUE)
#'
#' # Retrieve the key from common sources
#' key <- get_nass_key()
#'
#' # One-liner for vignettes/scripts
#' key <- ensure_nass_key(persist = "session")
#' }
NULL

# ---- Internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.mask_key <- function(key, show = 4) {
  if (!nzchar(key)) return("<empty>")
  n <- nchar(key)
  if (n <= show) return(key)
  paste0(substr(key, 1, show), strrep("*", max(0, n - show)))
}

#' @keywords internal
#' @noRd
.renviron_has_key <- function(path) {
  if (!file.exists(path)) return(FALSE)
  any(grepl("^\\s*NASS_API_KEY\\s*=", readLines(path, warn = FALSE)))
}

#' @keywords internal
#' @noRd
.write_renviron_key <- function(path, key, overwrite = FALSE) {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  has_line <- any(grepl("^\\s*NASS_API_KEY\\s*=", lines))
  if (has_line && !overwrite) {
    stop("NASS_API_KEY already exists in ", path, ". Set overwrite = TRUE to replace.", call. = FALSE)
  }
  new_line <- paste0("NASS_API_KEY=", key)
  if (has_line) {
    lines <- sub("^\\s*NASS_API_KEY\\s*=.*$", new_line, lines)
  } else {
    lines <- c(lines, new_line)
  }
  # Ensure file ends with a newline for robustness
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con)
  invisible(path)
}

# ---- Public API -------------------------------------------------------------

#' Retrieve the NASS API key from common sources
#'
#' @param sources Character vector of sources to check in order.
#'   Defaults to `c("session", "env", "renviron")`.
#' @param renviron_path Path to `.Renviron` for fallback retrieval. Default `~/.Renviron`.
#' @param error_if_missing Logical; if `TRUE` (default), error when no key is found.
#' @return A character string containing the API key (length ≥ 1).
#' @export
get_nass_key <- function(sources = c("session", "env", "renviron"),
                         renviron_path = "~/.Renviron",
                         error_if_missing = TRUE) {
  sources <- match.arg(sources, c("session", "env", "renviron"), several.ok = TRUE)
  
  # 1) session via rnassqs (if available)
  if ("session" %in% sources) {
    if (requireNamespace("rnassqs", quietly = TRUE)) {
      # rnassqs stores key internally; no public getter, but honor env first
      # If user authenticated earlier, prefer env as the 'source of truth'
      k <- Sys.getenv("NASS_API_KEY", unset = "")
      if (!nzchar(k)) {
        # If env isn't set, try to accept that the session might still be authenticated
        # We can't read rnassqs's internal key; so rely on env or renviron next.
      } else {
        return(k)
      }
    }
  }
  
  # 2) environment
  if ("env" %in% sources) {
    k <- Sys.getenv("NASS_API_KEY", unset = "")
    if (nzchar(k)) return(k)
  }
  
  # 3) renviron file
  if ("renviron" %in% sources) {
    # Load values from the specified .Renviron file into current session
    try(readRenviron(renviron_path), silent = TRUE)
    k <- Sys.getenv("NASS_API_KEY", unset = "")
    if (nzchar(k)) return(k)
  }
  
  if (isTRUE(error_if_missing)) {
    stop(
      "NASS_API_KEY not found in session/env/.Renviron.\n",
      "Set for this session:\n  Sys.setenv(NASS_API_KEY = \"your_api_key_here\")\n",
      "Or persist to .Renviron:\n  usethis::edit_r_profile() or write directly to ~/.Renviron.\n",
      call. = FALSE
    )
  }
  
  ""
}

#' Set the NASS API key and optionally persist/authenticate
#'
#' @param key Character; your NASS Quick Stats API key.
#' @param persist One of `"session"`, `"env"`, or `"renviron"`. Controls where
#'   the key is stored. Default `"session"`.
#' @param renviron_path Path to `.Renviron` when `persist = "renviron"`. Default `~/.Renviron`.
#' @param overwrite Logical; when `persist = "renviron"`, overwrite existing entry if present.
#' @param auth Logical; call `rnassqs::nassqs_auth(key)` after setting. Default `TRUE`.
#' @return Invisibly returns the key.
#' @export
set_nass_key <- function(key,
                         persist = c("session", "env", "renviron"),
                         renviron_path = "~/.Renviron",
                         overwrite = FALSE,
                         auth = TRUE) {
  persist <- match.arg(persist)
  
  if (!is.character(key) || !nzchar(key)) {
    stop("`key` must be a non-empty character string.", call. = FALSE)
  }
  
  # Persist behavior
  if (persist == "session") {
    # Optionally set env for convenience in the session
    Sys.setenv(NASS_API_KEY = key)
    if (isTRUE(auth) && requireNamespace("rnassqs", quietly = TRUE)) {
      rnassqs::nassqs_auth(key = key)
    }
    message("NASS API key set for session (", .mask_key(key), ").")
  } else if (persist == "env") {
    Sys.setenv(NASS_API_KEY = key)
    if (isTRUE(auth) && requireNamespace("rnassqs", quietly = TRUE)) {
      rnassqs::nassqs_auth(key = key)
    }
    message("NASS API key set in process environment (", .mask_key(key), ").")
  } else if (persist == "renviron") {
    .write_renviron_key(renviron_path, key, overwrite = overwrite)
    # Load into session so downstream calls work immediately
    readRenviron(renviron_path)
    Sys.setenv(NASS_API_KEY = key)
    if (isTRUE(auth) && requireNamespace("rnassqs", quietly = TRUE)) {
      rnassqs::nassqs_auth(key = key)
    }
    message("NASS API key written to ", renviron_path, " (", .mask_key(key), ").")
  }
  
  invisible(key)
}

#' Ensure a NASS API key is available and authenticated
#'
#' @description
#' Convenience wrapper: tries to retrieve the key (session/env/.Renviron) and
#' authenticates. If not found, errors with clear guidance.
#'
#' @param persist Default `"session"`. If a key is found, it will be ensured
#'   in the selected persistence target for consistency.
#' @param renviron_path Path to `.Renviron` when using persistence. Default `~/.Renviron`.
#' @return The key as a character scalar.
#' @export
ensure_nass_key <- function(persist = c("session", "env", "renviron"),
                            renviron_path = "~/.Renviron") {
  persist <- match.arg(persist)
  key <- get_nass_key(error_if_missing = TRUE, renviron_path = renviron_path)
  set_nass_key(key, persist = persist, renviron_path = renviron_path, overwrite = FALSE, auth = TRUE)
  key
}

