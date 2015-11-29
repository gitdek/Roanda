###############################################################################
# Roanda: storage source
#   By puglisij Copyright (C) 2015, All rights reserved.
#
###############################################################################

# package.require('rzmq')
# package.require('AWS.tools')

# cl <-
#   startCluster(
#     ami = "ami-a531fccc",key = "my-key",instance.count = 4,instance.type = "m1.large"
#   )
# master.node <- get.master(cl)
#
# estimatePi <- function(seed) {
#   set.seed(seed)
#   numDraws <- 1e6
#
#   r <- .5
#   x <- runif(numDraws, min = -r, max = r)
#   y <- runif(numDraws, min = -r, max = r)
#   inCircle <- ifelse((x ^ 2 + y ^ 2) ^ .5 < r , 1, 0)
#
#   sum(inCircle) / length(inCircle) * 4
# }
#
# print(system.time(
#   ans <- zmq.lapply(
#     as.list(1:1e4),
#     estimatePi,
#     execution.server = paste("tcp://",master.node$PublicDNS,":6000",sep =
#                                ""),
#     sink.server = paste("tcp://",master.node$PublicDNS,":6001",sep =
#                           "")
#   )
# ))
#
# res <- terminateCluster(cl)
