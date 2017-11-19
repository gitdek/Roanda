###############################################################################
# package: Roanda source
#   By puglisij Copyright (C) 2015, All rights reserved.
#
###############################################################################

library(RCurl)
library(jsonlite)
library(httr)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
library(plyr)


.token        <- getOption('Roanda.oandatoken')
.accountID    <- getOption('Roanda.oandaaccountid')
.accountType  <- getOption('Roanda.oandaaccounttype')
if(is.null(.accountType) || length(.accountType) < 1 {
  .accountType = "practice" 
 }
.lastInstrument = "EUR_USD"
.time_authorized = NULL
.instrument_cache = NULL
.last_request = NULL
.active_orders = NULL
.pending_orders = NULL


#' Connect to oanda authentication server.
#'
#' @param accountType Account Type (live or practice)
#' @param accountID API Account ID
#' @param token API token
#' @export
authorize <-
  function(accountType = c('live','practice'), accountID = .accountID, token = .accountID) {

    if (is.null(.time_authorized) || length(.time_authorized) < 1) {
      .accountType <-
        if (is.null(accountType))
          .accountType
      else
        accountType
      .accountID <-
        if (is.null(accountID))
          .accountID
      else
        accountID
      .token <- if (is.null(token))
        .token
      else
        token

      ret <- accountInfo(.accountType,.accountID,.token)

      print(paste("authorize-> Account information received"))#, ret))

      if (is.null(ret)) {
        .time_authorized = NULL
        return
      }

      .time_authorized <- Sys.time()

      print(paste("authorize-> Successfully authorized at", .time_authorized))

    } else {

      print(paste("Authenticated already! Timestamp of ", .time_authorized))

    }


    .time_authorized
  }

#' Get all available instruments for trade.
#'
#' @param accountType Account Type (live or practice)
#' @param accountID API Account ID
#' @param token API token
#' @description Retrieve all available forex instruments available to trade.
#' @export
getInstruments <-
  function(accountType = .accountType, token = .token, accountID = .accountID) {
    if (accountType == "practice") {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    auth       <- c(Authorization = paste("Bearer",token,sep = " "))
    Queryhttp  <-
      paste(httpaccount,"/v1/instruments?accountId=",sep = "")
    QueryInst  <- paste(Queryhttp,accountID,sep = "")
    QueryInst1 <-
      getURL(
        QueryInst,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
          auth
      )
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
    return(InstJson)
  }

# -- -------------------------------------------------------------------------------------------- #
# -- Actual Price Request ----------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
getPrices <- function(accountType,token,Instrument) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth         <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp    <-
    paste(httpaccount,"/v1/prices?instruments=",sep = "")
  QueryPrec    <- paste(Queryhttp,Instrument,sep = "")
  InstPrec     <-
    getURL(
      QueryPrec,cainfo = system.file("CurlSSL","cacert.pem",
                                     package = "RCurl"),httpheader =
        auth
    )
  InstPrecjson <- fromJSON(InstPrec, simplifyDataFrame = TRUE)
  DateTime     <- as.POSIXct(substr(InstPrecjson[[1]]$time,12,19),
                             format = "%H:%M:%S")
  Date <- as.character(substr(DateTime,1,10))
  Time <- as.character(substr(DateTime,12,19))
  DataJSON    <-
    data.frame(paste(Date,Time,sep = " "),InstPrecjson[[1]]$bid,InstPrecjson[[1]]$ask)
  colnames(DataJSON) <- c("TimeStamp","Bid","Ask")
  DataJSON$TimeStamp <-
    as.POSIXct(DataJSON$TimeStamp,origin = "1970-01-01")
  return(DataJSON)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Historical Prices Request ------------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

#' @export
historicalLookup  <-
  function(accountType,Granularity,DayAlign,TimeAlign,token,Instrument,Start,End) {
    if (accountType == "practice") {
      httpaccount  <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount  <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    #ifelse(Count = is.NULL, qcount  <- paste("count=",Count,sep=""), break)

    qstart <- paste("start=",Start,sep = "")
    qend   <- paste("end=",End,sep = "")

    qcandleFormat  <- "candleFormat=midpoint"
    qgranularity   <- paste("granularity=",Granularity,sep = "")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep = "")
    qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep = "")

    auth           <- c(Authorization = paste("Bearer",token,sep = " "))
    QueryHistPrec  <-
      paste(httpaccount,"/v1/candles?instrument=",sep = "")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep = "")
    QueryHistPrec2 <-
      paste(
        QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity,
        qdailyalignment,qalignmentTimezone,sep = "&"
      )
    InstHistP <-
      getURL(
        QueryHistPrec2,cainfo = system.file("CurlSSL","cacert.pem",
                                            package = "RCurl"),httpheader =
          auth
      )
    InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
    Prices        <- data.frame(InstHistPjson[[3]])
    Prices$time <-
      paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep = " ")
    colnames(Prices) <-
      c("TimeStamp","Open","High","Low","Close","TickVolume","Complete")
    Prices$TimeStamp <-
      as.POSIXct(Prices$TimeStamp,"%d/%m/%y %H:%M:%S",origin = "1970-01-01")
    return(Prices)
  }

# -- -------------------------------------------------------------------------------------------- #
# -- Accounts per given username  --------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
getAccounts <- function(accountType,token,UserName) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth         <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts?username=",sep = "")
  QueryInst  <- paste(Queryhttp,UserName,sep = "")
  QueryInst1 <-
    getURL(
      QueryInst,cainfo = system.file("CurlSSL","cacert.pem",
                                     package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Account Information  ----------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #
#' @export
accountInfo   <- function(accountType,accountID,token) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  QueryInfo   <- paste(Queryhttp,accountID,sep = "")
  CtaInfo     <- getURL(
    QueryInfo,cainfo = system.file("CurlSSL",
                                   "cacert.pem",package =
                                     "RCurl"),httpheader = auth
  )
  CtaInfoJson <- fromJSON(CtaInfo, simplifyDataFrame = TRUE)

  CtaNombre <-
    CtaInfoJson$accountName     # Nombre de la cta que se esta consultando
  CtaBalanc <- CtaInfoJson$balance         # Balance de la cta
  Ctaunreal <-
    CtaInfoJson$unrealizedPl    # Ganancia/Perdida sin tomar
  Ctareal   <-
    CtaInfoJson$realizedPl      # Ganancia/Perdida ya tomada
  Ctamgenut <- CtaInfoJson$marginUsed      # Margen utilizado
  Ctamgendi <- CtaInfoJson$marginAvail     # Margen disponible
  Ctamgenrt <-
    CtaInfoJson$marginRate      # Tasa de margen utilizada
  CtaOperac <- CtaInfoJson$openTrades      # Operaciones abiertas
  CtaOrdens <- CtaInfoJson$openOrders      # Ordenes abiertas
  datos     <-
    data.frame(
      CtaNombre,CtaBalanc,Ctaunreal,Ctareal,Ctamgenut,
      Ctamgendi,Ctamgenrt,CtaOperac,CtaOrdens
    )
  return(datos)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Actual order in the account ---------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
orders  <- function(accountType,accountID,token,Instrument) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/orders?instrument=",sep = "")
  Querythttp3  <- paste(Querythttp2,Instrument,sep = "")
  Querythttp4  <- paste(Querythttp3,"&count=2",sep = "")
  QueryInst1   <-
    getURL(
      Querythttp4,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Place a new order -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
execOrder <-
  function(accountType,token,Instrument,accountID,Count,Side,OrderType) {
    if (accountType == "practice") {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    auth       <- c(Authorization = paste("Bearer",token,sep = " "))
    Queryhttp  <- paste(httpaccount,"/v1/accounts/",sep = "")
    Queryhttp1 <- paste(Queryhttp,accountID,sep = "")
    Queryhttp2 <- paste(Queryhttp1,"/orders",sep = "")

    postForm(
      Queryhttp2,style = "POST",.params = c(
        instrument = Instrument,units = Count,upperBound = slipUp,
        lowerBound = slipDown,takeProfit =
          takeProfit,stopLoss = stopLoss,side = Side,type = OrderType
      ),
      opts = list(httpheader = auth,ssl.verifypeer = FALSE)
    )
  }

# -- -------------------------------------------------------------------------------------------- #
# -- Information about a particular order ------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
orderInfo  <- function(accountType,accountID,token,OrderNum) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/orders/",sep = "")
  Querythttp3  <- paste(Querythttp2,OrderNum,sep = "")
  QueryInst1   <-
    getURL(
      Querythttp3,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- List of open trades ------------------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

#' @export
openTrades  <- function(accountType,accountID,token,Instrument) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/trades?instrument=",sep = "")
  Querythttp3  <- paste(Querythttp2,Instrument,sep = "")
  Querythttp4  <- paste(Querythttp3,"&count=100",sep = "")
  QueryInst1   <-
    getURL(
      Querythttp4,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- A particular trade's Information  ---------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
tradeInfo  <- function(accountType,accountID,token,TradeNumber) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/trades/",sep = "")
  Querythttp3  <- paste(Querythttp2,TradeNumber,sep = "")
  QueryInst1   <-
    getURL(
      Querythttp3,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Account's Open Positions List -------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
getAllPositions  <- function(accountType,accountID,token) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/positions",sep = "")
  QueryInst1   <-
    getURL(
      Querythttp2,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Position respect a particular instrument --------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
getPosition  <- function(accountType,accountID,token,Instrument) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/positions/",sep = "")
  Querythttp3  <- paste(Querythttp2,Instrument,sep = "")
  QueryInst1   <-
    getURL(
      Querythttp3,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Historical of transactions ----------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
historicalTransactions  <-
  function(accountType,accountID,token,Instrument,Count) {
    if (accountType == "practice") {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    auth        <- c(Authorization = paste("Bearer",token,sep = " "))
    Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
    Querythttp1  <- paste(Queryhttp,accountID,sep = "")
    Querythttp2  <-
      paste(Querythttp1,"/transactions?instrument=",sep = "")
    Querythttp3  <- paste(Querythttp2,Instrument,sep = "")
    Querythttp4  <- paste(Querythttp3,"&count=",sep = "")
    Querythttp5  <- paste(Querythttp4,Count,sep = "")
    QueryInst1   <-
      getURL(
        Querythttp5,cainfo = system.file("CurlSSL","cacert.pem",
                                         package = "RCurl"),httpheader =
          auth
      )
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
    return(InstJson)
  }

# -- -------------------------------------------------------------------------------------------- #
# -- A particular transaction info  ------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
transactionInfo  <-
  function(accountType,accountID,token,TransactionNum) {
    if (accountType == "practice") {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    auth        <- c(Authorization = paste("Bearer",token,sep = " "))
    Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
    Querythttp1  <- paste(Queryhttp,accountID,sep = "")
    Querythttp2  <- paste(Querythttp1,"/transactions/",sep = "")
    Querythttp3  <- paste(Querythttp2,TransactionNum,sep = "")
    QueryInst1   <-
      getURL(
        Querythttp3,cainfo = system.file("CurlSSL","cacert.pem",
                                         package = "RCurl"),httpheader =
          auth
      )
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
    return(InstJson)
  }

# -- -------------------------------------------------------------------------------------------- #
# -- General Info about all transactions of the account ----------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
allTransactions  <- function(accountType,accountID,token) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth        <- c(Authorization = paste("Bearer",token,sep = " "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep = "")
  Querythttp1  <- paste(Queryhttp,accountID,sep = "")
  Querythttp2  <- paste(Querythttp1,"/alltransactions",sep = "")
  QueryInst1   <-
    getURL(
      Querythttp2,cainfo = system.file("CurlSSL","cacert.pem",
                                       package = "RCurl"),httpheader =
        auth
    )
  # InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(QueryInst1)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Economic Calendar -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
calendar <- function(accountType,token,Instrument,Period) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth  <- c(Authorization = paste("Bearer",token,sep = " "))
  auth1 <- paste("Authorization:",auth,sep = " ")

  Queryhttp  <-
    paste(httpaccount,"/labs/v1/calendar?instrument=",sep = "")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep = "")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep = "&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep = "")

  CalenH  <- getURL(
    Queryhttp3,cainfo = system.file("CurlSSL",
                                    "cacert.pem",package =
                                      "RCurl"),httpheader = auth
  )
  Calend  <- fromJSON(CalenH, simplifyDataFrame = TRUE)
  Calend  <- subset(Calend, select = -c(currency,region,impact))
  Calend  <- Calend[complete.cases(Calend[,]),]
  Calend$timestamp <-
    as.POSIXct(Calend$timestamp,origin = "1970-01-01")
  return(Calend)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Historical posistion ratios in OANDA ------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
historicalPositionRatios <-
  function(accountType,token,Instrument,Period) {
    if (accountType == "practice") {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    auth  <- c(Authorization = paste("Bearer",token,sep = " "))
    auth1 <- paste("Authorization:",auth,sep = " ")

    Queryhttp  <-
      paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep =
              "")
    Queryhttp1 <- paste(Queryhttp,Instrument,sep = "")
    Queryhttp2 <- paste(Queryhttp1,"period=",sep = "&")
    Queryhttp3 <- paste(Queryhttp2,Period,sep = "")

    ratios     <- getURL(
      Queryhttp3,cainfo = system.file("CurlSSL",
                                      "cacert.pem",package =
                                        "RCurl"),httpheader = auth
    )
    ratios     <- data.frame(fromJSON(ratios))
    ratios[,2] <- as.POSIXct(ratios[,2],origin = "1970-01-01")
    return(ratios)
  }

# -- -------------------------------------------------------------------------------------------- #
# -- Current OANDA's Clients Spreads ------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

#' @export
getSpreads <- function(accountType,token,Instrument,Period) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth  <- c(Authorization = paste("Bearer",token,sep = " "))
  auth1 <- paste("Authorization:",auth,sep = " ")

  Queryhttp  <-
    paste(httpaccount,"/labs/v1/spreads?instrument=",sep = "")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep = "")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep = "&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep = "")

  spread <- getURL(
    Queryhttp3,cainfo = system.file("CurlSSL",
                                    "cacert.pem",package =
                                      "RCurl"),httpheader = auth
  )
  spread <- fromJSON(spread)
  return(spread)
}


# -- -------------------------------------------------------------------------------------------- #
# -- Order Book --------------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

getOrderBook <- function(accountType,token,Instrument,Period) {
  if (accountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else
    if (accountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else
      print("Account type error. Must be practice or live")

  auth  <- c(Authorization = paste("Bearer",token,sep = " "))
  auth1 <- paste("Authorization:",auth,sep = " ")

  Queryhttp  <-
    paste(httpaccount,"/labs/v1/orderbook_data?instrument=",sep = "")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep = "")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep = "&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep = "")

  orderbook  <- getURL(
    Queryhttp3,cainfo = system.file("CurlSSL",
                                    "cacert.pem",package =
                                      "RCurl"),httpheader = auth
  )
  orderbook  <- fromJSON(orderbook)
  return(orderbook)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Autochartist "Our Favorites" Signals ------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#' @export
getAutochartist <-
  function(accountType,token,Instrument,Period,Type) {
    if (accountType == "practice") {
      httpaccount <- "https://api-fxpractice.oanda.com"
    } else
      if (accountType == "live") {
        httpaccount <- "https://api-fxtrade.oanda.com"
      } else
        print("Account type error. Must be practice or live")

    auth  <- c(Authorization = paste("Bearer",token,sep = " "))
    auth1 <- paste("Authorization:",auth,sep = " ")

    Queryhttp  <-
      paste(httpaccount,"/labs/v1/signal/autochartist?instrument=",sep = "")
    Queryhttp1 <- paste(Queryhttp,Instrument,sep = "")
    Queryhttp2 <- paste(Queryhttp1,"period=",sep = "&")
    Queryhttp3 <- paste(Queryhttp2,Period,sep = "")
    Queryhttp4 <- paste(Queryhttp3,"type=",sep = "")
    Queryhttp5 <- paste(Queryhttp4,Type,sep = "")

    Autochart  <- getURL(
      Queryhttp5,cainfo = system.file("CurlSSL",
                                      "cacert.pem",package =
                                        "RCurl"),httpheader = auth
    )
    Autochart  <- fromJSON(Autochart)
    return(Autochart)
  }

# -- PENDING Close an order --------------------------------------------------------------------- #

# -- PENDING Close a trade ---------------------------------------------------------------------- #

# -- PENDING Close existing position ------------------------------------------------------------ #

# -- PENDING Modify parameters of an order ------------------------------------------------------ #

# -- PENDING Modify parameters of a trade ------------------------------------------------------- #
