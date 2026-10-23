#' Read CSV data from file or URL
#'
#' @param source Local file path or HTTP(S) URL.
#'
#' @returns A tibble.
#'
#' @export
read_data <- function(source) {
  
  stopifnot(
    is.character(source),
    length(source) == 1L,
    !is.na(source)
  )
  
  logger::log_info("Reading data from '{source}'")
  
  file <- resolve_source(source)
  
  validate_csv(file)
  
  out <- readr::read_csv(
    file,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  logger::log_info(
    "Successfully read {nrow(out)} rows and {ncol(out)} columns."
  )
  
  out
}