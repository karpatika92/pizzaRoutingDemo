rndTime <- function(N, st="2012/01/01", et="2012/12/31") {
       st <- as.POSIXct(st)
       et <- as.POSIXct(et)
      dt <- as.numeric(difftime(et,st,unit="sec"))
      ev <- sort(runif(N, 0, dt))
      rt <- st + ev
}
