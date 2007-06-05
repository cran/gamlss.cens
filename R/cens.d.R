cens.d <- function(family = "NO", type = c( "right", "left", "interval"),...)
  {
    #if (!is.Surv(y)) stop(paste("the y variable is not a Surv object"))
     type <- match.arg(type)
    if (type=="counting") stop(paste("gamlss has no support for counting"))
   fname <- family
    if (mode(family) != "character" && mode(family) != "name")
   fname <- as.character(substitute(family))
 distype <- eval(call(family))$type
    dfun <- paste("d",fname,sep="")
    pfun <- paste("p",fname,sep="")
     pdf <- eval(parse(text=dfun))
     cdf <- eval(parse(text=pfun))
fun <- if (type=="left")  
       function(y, log = FALSE, ...)
        {
         if (!is.Surv(y)) stop(paste("the y variable is not a Surv object"))
        dfun <- ifelse(y[,"status"]==1, pdf(y[,1],log = TRUE,...),log(cdf(y[,1],...)))
        dfun <- if (log == TRUE) dfun else exp(dfun)
        dfun
       }
     else if (type=="right")
      function(y, log = FALSE, ...)
       {
        if (!is.Surv(y)) stop(paste("the y variable is not a Surv object"))
        dfun <- ifelse(y[,"status"]==1, pdf(y[,1],log = TRUE,...),log(1-cdf(y[,1],...)))
        dfun <- if (log == TRUE) dfun else exp(dfun)
        dfun
       } 
     else if (type=="interval")    
      function(y, log = FALSE, ...)
       {
        if (!is.Surv(y)) stop(paste("the y variable is not a Surv object"))
        dfun0 <-ifelse(y[,"status"]==0, cdf(y[,1], lower.tail=F, log.p=T, ...),0) # right  equivalent: log(1-cdf(y[,1],...)) cdf(y[,1], lower.tail=F, log.p=T, ...) 
        dfun1 <-ifelse(y[,"status"]==1, pdf(y[,1],log = TRUE,...),0)# death
        dfun2 <-ifelse(y[,"status"]==2, cdf(y[,1],  log.p=T, ...),0)# left  
        suppressWarnings(dfun3 <-ifelse(y[,"status"]==3, log(cdf(y[,2],...)-cdf(y[,1],...)),0))# interval
        dfun <- dfun0+dfun1+dfun2+dfun3
        dfun <- if (log == TRUE) dfun else exp(dfun)
        dfun
       }
  fun
  }
