#' Retrieve the NASS API key from environment variables
#'
#' This internal helper reads the `NASS_API_KEY` environment variable.
#' It is used by all functions that communicate with the USDA NASS
#' Quick Stats API. If the key is not found, the function throws
#' an informative error instructing the user how to set it.
#'
#' @return A character string containing the API key.
#' @keywords internal
#' @noRd
get_nass_key <- function() {
  key <- Sys.getenv("NASS_API_KEY", unset = "")
  
  if (identical(key, "")) {
    stop(
      "NASS_API_KEY is not set.\n",
      "Set it for this session with:\n",
      "  Sys.setenv(NASS_API_KEY = \"your_api_key_here\")\n\n",
      "Or set it persistently by adding this line to your .Renviron:\n",
      "  NASS_API_KEY=your_api_key_here\n",
      call. = FALSE
    )
  }
  
  key
}
