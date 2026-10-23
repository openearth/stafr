
is_url <- function(x) {
  grepl("^https?://", x, ignore.case = TRUE)
}

resolve_source <- function(source) {
  
  if (is_url(source)) {
    return(download_source(source))
  }
  
  validate_file(source)
  
  normalizePath(
    source,
    winslash = "/",
    mustWork = TRUE
  )
}

validate_file <- function(path) {
  
  if (!file.exists(path)) {
    stop(
      sprintf("File does not exist: %s", path),
      call. = FALSE
    )
  }
  
  if (file.access(path, 4) != 0) {
    stop(
      sprintf("File is not readable: %s", path),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

download_source <- function(url) {
  
  logger::log_info(
    "Downloading file from URL '{url}'."
  )
  
  tmp <- tempfile(fileext = ".csv")
  
  tryCatch({
    
    httr2::request(url) |>
      httr2::req_timeout(60) |>
      httr2::req_perform(path = tmp)
    
  }, error = function(e) {
    
    stop(
      sprintf(
        "Could not download URL '%s': %s",
        url,
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
  
  validate_file(tmp)
  
  tmp
}

validate_csv <- function(path) {
  
  result <- tryCatch({
    
    suppressWarnings(
      readr::read_csv(
        path,
        n_max = 10,
        show_col_types = FALSE,
        progress = FALSE
      )
    )
    
  }, error = function(e) {
    
    stop(
      sprintf(
        "File is not a valid CSV: %s",
        path
      ),
      call. = FALSE
    )
  })
  
  if (ncol(result) < 1) {
    stop(
      "CSV contains no columns.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


