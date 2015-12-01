library(rredis)
# library(rcppredis) # Need to Implement
library(xts)
library(ptproc)
options(digits.secs=14) #options(digits=14)

dropEpochSeconds <- function(x) { return(as.integer(x/10000) * 10000) }
epoch2posix <- function(x) { return(as.POSIXct(x/1000, origin="1970-01-01", tz="UTC" ) ) }
dropSeconds <- function(x) { return( trunc(x,"mins"))}
dropSecondsList <- function(x) {
  if (class(x) == "numeric") {
    return( lapply( epoch2posix(x),  trunc, "mins" ) ) #lapply(as.list(x), trunc,  "mins") )
    } else { return( lapply(as.list(index(x)), trunc,  "mins") ) }
}
getThinnedRatio <- function(symbolRaw, then, now) {
  #########################################
  # Gather Data
  bidKey <- paste(symbolRaw,":0",sep="")
  askKey <- paste(symbolRaw,":1",sep="")
  bidsDF <- (do.call(cbind, redisZRangeByScore(bidKey, then, now, withscores=TRUE ) ) )
  bid_times <- do.call( rbind, bidsDF[,2] ); #bid_records <- do.call( rbind, bidsDF[,1] )
  bid_timestamps <- as.POSIXct(bid_times/1000, origin="1970-01-01", tz="UTC" )
  bid_split <- strsplit( do.call( rbind, bidsDF[,1] ), "," )
  bid_rates    <- as.numeric(unlist(bid_split)[2*(1:length(bid_split))-1]); #bid_times    <- unlist(bid_split)[2*(1:length(bid_split))  ]
  bidsXTS <- merge(xts(  bid_rates, order.by=bid_timestamps ), xts(  bid_times, order.by=bid_timestamps ))
  bidsXTS <- `colnames<-`(bidsXTS,c("rate","time"))
  bids_thinned <- bidsXTS[bidsXTS$rate != lag(bidsXTS$rate,1) ]

  asksDF <- (do.call(cbind, redisZRangeByScore(askKey, then, now, withscores=TRUE ) ) )
  ask_times <- do.call( rbind, asksDF[,2] ); #ask_records <- do.call( rbind, asksDF[,1] )
  ask_timestamps <- as.POSIXct(ask_times/1000, origin="1970-01-01", tz="UTC")
  ask_split <- strsplit( do.call( rbind, asksDF[,1] ), "," )
  ask_rates    <- as.numeric(unlist(ask_split)[2*(1:length(ask_split))-1]); #ask_times    <- as.numeric(unlist(ask_split)[2*(1:length(ask_split))  ])
  asksXTS <- merge(xts(  ask_rates, order.by=ask_timestamps ), xts(  ask_times, order.by=ask_timestamps ))
  asksXTS <- `colnames<-`(asksXTS,c("rate","time"))
  asks_thinned <- asksXTS[asksXTS$rate != lag(asksXTS$rate,1) ]
  bid_times <- bids_thinned$time
  ask_times <- asks_thinned$time

  df_length <- max(c(length(bid_times), length(ask_times)))
  bid_length <- length(bid_times);
  ask_length <- length(ask_times)
  if (length(ask_times) > length(bid_times)) { max_times <- ask_times; max_timestamps <- index(asks_thinned) } else { max_times <- bid_times; max_timestamps <- index(bids_thinned) }

  df_mins <- do.call( c, unique(lapply(index(max_times),trunc,"mins") ) ) #unique( lapply(as.list(index(max_times)),trunc,"mins"))
  mins_length <- length(df_mins)

  bid_counts <- xts(  matrix(1,bid_length,1), order.by= index(bids_thinned ) )
  ask_counts <- xts(  matrix(1,ask_length,1), order.by= index(asks_thinned ) )

  bid_sums <- vector("list", mins_length)
  ask_sums <- vector("list", mins_length)
  last <- function(x) { return( x[length(x)] ) }
  for (i in 1:mins_length) {
    if ( i == mins_length ) {
      bid_sums[i] <- sum(bid_counts[ index(bid_counts) >= df_mins[mins_length] ] )
      ask_sums[i] <- sum(ask_counts[ index(ask_counts) >= df_mins[mins_length] ] )
    } else {
      bid_sums[i] <- sum(bid_counts[ index(bid_counts) >= df_mins[i] & index(bid_counts) <= df_mins[i+1]  ] )
      ask_sums[i] <- sum(ask_counts[ index(ask_counts) >= df_mins[i] & index(ask_counts) <= df_mins[i+1]  ] )
    }
  }

  bid_empirical_1min <- xts( do.call(rbind, bid_sums) , order.by=df_mins )
  ask_empirical_1min <- xts( do.call(rbind, ask_sums) , order.by=df_mins )
  colnames(bid_empirical_1min) <- c("counts")
  colnames(ask_empirical_1min) <- c("counts")

  returnValue <- last( (bid_empirical_1min / ask_empirical_1min) )
  return (returnValue)
}

getBacktestData <- function(symbolRaw, startTime, endTime, period='minutes') {
  # Gather Data
  redisConnect()
  bidKey <- paste(symbolRaw,":0",sep="")
  askKey <- paste(symbolRaw,":1",sep="")
  bidsDF <- (do.call(cbind, redisZRangeByScore(bidKey, startTime, endTime, withscores=TRUE ) ) )
  bid_times <- do.call( rbind, bidsDF[,2] );
  bid_timestamps <- as.POSIXct(bid_times/1000, origin="1970-01-01", tz="UTC" )
  bid_split <- strsplit( do.call( rbind, bidsDF[,1] ), "," )
  bid_rates    <- as.numeric(unlist(bid_split)[2*(1:length(bid_split))-1]); #bid_times    <- unlist(bid_split)[2*(1:length(bid_split))  ]
  bidsXTS <- merge(xts(  bid_rates, order.by=bid_timestamps ), xts(  bid_times, order.by=bid_timestamps ))
  bidsXTS <- `colnames<-`(bidsXTS,c("rate","time"))

  asksDF <- (do.call(cbind, redisZRangeByScore(askKey, startTime, endTime, withscores=TRUE ) ) )
  ask_times <- do.call( rbind, asksDF[,2] );
  ask_timestamps <- as.POSIXct(ask_times/1000, origin="1970-01-01", tz="UTC")
  ask_split <- strsplit( do.call( rbind, asksDF[,1] ), "," )
  ask_rates    <- as.numeric(unlist(ask_split)[2*(1:length(ask_split))-1]); #ask_times    <- as.numeric(unlist(ask_split)[2*(1:length(ask_split))  ])
  asksXTS <- merge(xts(  ask_rates, order.by=ask_timestamps ), xts(  ask_times, order.by=ask_timestamps ))
  asksXTS <- `colnames<-`(asksXTS,c("rate","time"))
  if (period == 'minutes') {
    data = align.time( to.minutes( (asksXTS$rate + bidsXTS$rate)/2 , 1, name='Mid'), n=60)
  } else {
    data = align.time( to.period( (asksXTS$rate + bidsXTS$rate)/2 , 1, name='Mid',  period='seconds'), n=1) } #OHLC=FALSE,
  colnames(data) <- c("Open", "High", "Low", "Close"); data$Volume = 0
  redisClose()
  return(data)
}

getConditionalIntensity01 <- function(symbolRaw, then, now){
  redisConnect()
  #########################################
  # Gather Data
  bidKey <- paste(symbolRaw,":0",sep="")
  askKey <- paste(symbolRaw,":1",sep="")
  bidsDF <- (do.call(cbind, redisZRangeByScore(bidKey, then, now, withscores=TRUE ) ) )
  bid_times <- do.call( rbind, bidsDF[,2] ); #bid_records <- do.call( rbind, bidsDF[,1] )
  bid_timestamps <- as.POSIXct(bid_times/1000, origin="1970-01-01", tz="UTC" )
  bid_split <- strsplit( do.call( rbind, bidsDF[,1] ), "," )
  bid_rates    <- as.numeric(unlist(bid_split)[2*(1:length(bid_split))-1]); #bid_times    <- unlist(bid_split)[2*(1:length(bid_split))  ]
  bidsXTS <- merge(xts(  bid_rates, order.by=bid_timestamps ), xts(  bid_times, order.by=bid_timestamps ))
  bidsXTS <- `colnames<-`(bidsXTS,c("rate","time"))
  bids_thinned <- bidsXTS[bidsXTS$rate != lag(bidsXTS$rate,1) ] # -1 or 1?

  asksDF <- (do.call(cbind, redisZRangeByScore(askKey, then, now, withscores=TRUE ) ) )
  ask_times <- do.call( rbind, asksDF[,2] ); #ask_records <- do.call( rbind, asksDF[,1] )
  ask_timestamps <- as.POSIXct(ask_times/1000, origin="1970-01-01", tz="UTC")
  ask_split <- strsplit( do.call( rbind, asksDF[,1] ), "," )
  ask_rates    <- as.numeric(unlist(ask_split)[2*(1:length(ask_split))-1]); #ask_times    <- as.numeric(unlist(ask_split)[2*(1:length(ask_split))  ])
  asksXTS <- merge(xts(  ask_rates, order.by=ask_timestamps ), xts(  ask_times, order.by=ask_timestamps ))
  asksXTS <- `colnames<-`(asksXTS,c("rate","time"))
  asks_thinned <- asksXTS[asksXTS$rate != lag(asksXTS$rate,1) ] # -1 or 1?

  #renaming bid_times/ask_times here
  bid_times <- bids_thinned$time
  ask_times <- asks_thinned$time

  df_length <- max(c(length(bid_times), length(ask_times)))
  bid_length <- length(bid_times);
  ask_length <- length(ask_times);
  if (length(ask_times) > length(bid_times)) { max_times <- ask_times; max_timestamps <- index(asks_thinned) } else { max_times <- bid_times; max_timestamps <- index(bids_thinned) }

  #########################################
  # evaluation of intensity over the given timeframe.
  intensitySeq <- seq(
    min( c( min(bid_times), min(ask_times))),
    max( c( max(bid_times), max(ask_times))),
    len=50000) #df_length)

  fitModelBid <- fit(0.5, bid_times)
  fitModelAsk <- fit(0.5, ask_times)
  #########################################
  conditionalIntensityBid <- evalCIF(fitModelBid, xpts = intensitySeq)
  conditionalIntensityAsk <- evalCIF(fitModelAsk, xpts = intensitySeq)
  intensityBid <- data.frame(index=intensitySeq, data=conditionalIntensityBid)
  intensityAsk <- data.frame(index=intensitySeq, data=conditionalIntensityAsk)
  rownames(intensityBid) <- epoch2posix(intensitySeq)
  rownames(intensityAsk) <- epoch2posix(intensitySeq)

  df_mins <- do.call( c, unique(lapply(index(max_times),trunc,"mins") ) ) #unique( lapply(as.list(index(max_times)),trunc,"mins"))
  mins_length <- length(df_mins)

  fitBid_counts <- xts( as.matrix( intensityBid$data ), order.by= as.POSIXct(intensityBid$index/1000, origin="1970-01-01", tz="UTC" ) )
  fitAsk_counts <- xts( as.matrix( intensityAsk$data ), order.by= as.POSIXct(intensityAsk$index/1000, origin="1970-01-01", tz="UTC" ) )
  bid_counts <- xts(  matrix(1,bid_length,1), order.by= index(bids_thinned ) )
  ask_counts <- xts(  matrix(1,ask_length,1), order.by= index(asks_thinned ) )

  bid_sums <- vector("list", mins_length)
  ask_sums <- vector("list", mins_length)
  fitBid_sums <- vector("list", mins_length)
  fitAsk_sums <- vector("list", mins_length)


  for (i in 1:mins_length) {
    if ( i == mins_length )
    {
      bid_sums[i] <- sum(bid_counts[ index(bid_counts) >= df_mins[mins_length] ] )
      ask_sums[i] <- sum(ask_counts[ index(ask_counts) >= df_mins[mins_length] ] )
      fitBid_sums[i] <- sum(fitBid_counts[ index(fitBid_counts) >= df_mins[mins_length] ] )
      fitAsk_sums[i] <- sum(fitAsk_counts[ index(fitAsk_counts) >= df_mins[mins_length] ] )
    } else {
      bid_sums[i] <- sum(bid_counts[ index(bid_counts) >= df_mins[i] & index(bid_counts) <= df_mins[i+1]  ] )
      ask_sums[i] <- sum(ask_counts[ index(ask_counts) >= df_mins[i] & index(ask_counts) <= df_mins[i+1]  ] )
      fitBid_sums[i] <- sum(fitBid_counts[ index(fitBid_counts) >= df_mins[i] & index(fitBid_counts) <= df_mins[i+1]  ] )
      fitAsk_sums[i] <- sum(fitAsk_counts[ index(fitAsk_counts) >= df_mins[i] & index(fitAsk_counts) <= df_mins[i+1]  ] )
    }
  }

  bid_empirical_1min <- xts( do.call(rbind, bid_sums) , order.by=df_mins )
  ask_empirical_1min <- xts( do.call(rbind, ask_sums) , order.by=df_mins )
  bid_intensity_1min <- xts( do.call(rbind, fitBid_sums) , order.by=df_mins )
  ask_intensity_1min <- xts( do.call(rbind, fitAsk_sums) , order.by=df_mins )
  colnames(bid_empirical_1min) <- c("counts")
  colnames(ask_empirical_1min) <- c("counts")
  colnames(bid_intensity_1min) <- c("counts")
  colnames(ask_intensity_1min) <- c("counts")


  bidRatio <- ( bid_empirical_1min / bid_intensity_1min )
  askRatio <- ( ask_empirical_1min / ask_intensity_1min )

  returnValue <- last( (bidRatio/askRatio) )

  #####################################################
  # Tidy Up
  #redisClose()
  return(returnValue)
}

fit <- function(rate, data) {
  pstart <- c(mu = rate, C = 1, a = 0.1)
  ppm <- ptproc(pts = data, cond.int = hawkes.cond.int, params = pstart)
  condition(ppm) <- penalty(code = NULL, condition = quote(any(params < 0)))
  f <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5, hessian = TRUE)

  return (f)
}
fitQuotes <- function(rate, data, data2) {
  pstart <- c(mu = rate, C = 1, a = 0.1)
  ppm <- ptproc(pts = data, cond.int = hawkes.cond.int, params = pstart, data=data2)
  condition(ppm) <- penalty(code = NULL, condition = quote(any(params < 0)))
  f <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5, hessian = TRUE)
  return (f)}
last <- function(x) { return( x[length(x)] ) }

mainTest <- function() {
  #########################################
  # Initialize and Setup Variables
  redisConnect()
  debugging <- TRUE
  symbol = 'EUR/CHF'
  millisecondsBack = 900*1000 #was 900*1000 for 15min

  #########################################
  # Setup Times
  if (debugging) { now = 1448314572000 #now <- as.double( as.POSIXct( Sys.time() , tz="CST") )*1000; then <- now - millisecondsBack;
  } else {  now <- as.double( as.POSIXct( Sys.time() , tz="CST") )*1000;  }
  then <- now - millisecondsBack;

  returnValue = getConditionalIntensity01(symbol, then, now)

  if (returnValue > 1.5) { print(paste(returnValue,"Go Short",sep=" - ") )
    } else if (returnValue < 0.5) { print(paste(returnValue,"Go Long",sep=" - ") )
    } else { print(paste(returnValue,"Go Flat",sep=" - ") )}
  #print (c( last(bid_empirical_1min), last(ask_empirical_1min) )) #, last(fitted_1min), last(empirical_1min)


  #####################################################
  # Tidy Up
  redisClose()
}
