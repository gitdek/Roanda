library(microbenchmark)
library(Roanda)
library(RCurl)
library(jsonlite)
options(microbenchmark.unit = "relative")


context("api benchmark calls")

.token        = Sys.getenv("OANDA_API_TOKEN")
.accountID    = Sys.getenv("OANDA_API_ACCOUNT_ID")
.accountType  = 'live'
.lastInstrument = "EUR_USD"


api_call <- function() {
  x <-
    Roanda::authorize(accountType = .accountType,accountID = .accountID,token = .token)
  instruments <-
    Roanda::getInstruments(AccountType = .accountType , Token = .token,AccountID = .accountID)
  NULL
}

test_that("api authorize", {
  a <-
    Roanda::authorize(accountType = .accountType,accountID = .accountID,token = .token)
  expect_is(as.Date.ts(a),"Date",label  = "expecting datetime when authorized successfully.")
})


test_that("api instruments", {
  i1 <-
    Roanda::getInstruments(AccountType = .accountType , Token = .token,AccountID = .accountID)
  expect_false(is.null(i1))
  i2 <-
    Roanda::getInstruments(AccountType = .accountType , Token = .token,AccountID = .accountID)
  expect_false(is.null(i2))
  expect_equivalent(i1,i2)
})


# R -d valgrind --vanilla < test-benchmark.r
#res <- microbenchmark(api_call(), times = 200)
#print(res)
