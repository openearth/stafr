setup_logging <- function() {
  
  logger::log_appender(
    logger::appender_stdout
  )
  
  logger::log_layout(
    logger::layout_glue_colors
  )
  
  logger::log_threshold("INFO")
}


