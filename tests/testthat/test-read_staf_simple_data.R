test_that("read_staf_simple reads a CSV file", {
  
  path <- system.file(
    "extdata",
    "staf_output_simple.csv",
    package = "stafr"
  )
  
  dat <- read_staf_simple(path)
  
  expect_s3_class(dat, "data.frame")
  
  expect_equal(
    nrow(dat),
    1
  )
  
  expect_equal(
    ncol(dat),
    35
  )
  
  expect_equal(
    names(dat),
    c("SN", "Year", "Month", "Day", "Hour", "Minute", "Second", "Sample \xb0C", 
      "System \xb0C", "PEC", "Fv416", "Fv542", "Fv473", "Fv495", "Fv534", 
      "Fv594", "Fv622", "S416", "S542", "S473", "S495", "S534", "S594", 
      "S622", "Fo", "Fm", "Fv/Fmc", "Alpha", "Ek", "Beta", "EkBeta", 
      "rPm", "JVPIIm", "GOPIIm", "High E")
  )
  
})
