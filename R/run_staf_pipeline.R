#' Run complete workflow
#'
#' @param environment dev/test/accept/prod
#'
#' @export
run_pipeline <- function(environment = "dev") {
  
  setup_logging()
  
  cfg <- load_config(environment)
  
  raw <- read_staf_output(cfg)
  
  validated <- validate_staf(raw, cfg)
  
  transformed <- transform_staf(validated, cfg)
  
  write_staf_tidy(transformed, cfg)
  
  invisible(TRUE)
}