context("Progress bars")

library(plyr)

test_that("unknown progress bar raised warning, not error", {
  expect_warning(
    llply(1:10, identity, .progress = "blah"),
    "Cannot find progress bar"
  )
})

test_that("progress bar finishes clean", {
    l_ply(1:100, function(x) Sys.sleep(.01), .progress = "time")
})

