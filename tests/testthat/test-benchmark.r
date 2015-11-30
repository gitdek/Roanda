library(microbenchmark)
library(RCurl)
library(jsonlite)
library(testthat)

context("api benchmark calls")
options(microbenchmark.unit = "relative")



.token        <- getOption('Roanda.oandatoken')
.accountID    <- getOption('Roanda.oandaaccountid')
.accountType  = 'live'
.lastInstrument = "EUR_USD"


api_call <- function() {
  #x <-
  Roanda::authorize(accountType = .accountType,accountID = .accountID,token = .token)
  #instruments <-
  Roanda::getInstruments(accountType = .accountType , token = .token,accountID = .accountID)

  # instruments
}

test_that("api authorize", {
  a <-
    Roanda::authorize(accountType = .accountType,accountID = .accountID,token = .token)
  expect_is(as.Date.ts(a),"Date",label  = "expecting datetime when authorized successfully.")
})


library('quantmod')

test_that("api instruments", {
  i1 <-
    Roanda::getInstruments(accountType = .accountType , token = .token,accountID = .accountID)
  expect_false(is.null(i1))
  i2 <-
    Roanda::getInstruments(accountType = .accountType , token = .token,accountID = .accountID)
  expect_false(is.null(i2))
  expect_equivalent(i1,i2)
})

#
# test_that("repeated authentication.", {
#   # R -d valgrind --vanilla < test-benchmark.r
#   res <- expect_output(str(microbenchmark(api_call(), times = 3)),"3 obs")
#   #str(res)
# })

