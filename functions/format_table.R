format_excess_func <- function(excess.obj, syndrome1){
  
  dates1 <-
    excess.obj[[1]][[1]][[1]]$date
  
  unexplained.cases <-
    excessExtract(ds = excess.obj,
                  syndrome = syndrome1,
                  extract.quantity = "unexplained.cases")
  
  pred <- 
    excessExtract(ds = excess.obj,
                  syndrome = syndrome1,
                  extract.quantity = "pred")
  
  upi <- 
    excessExtract(ds = excess.obj,
                  syndrome = syndrome1,
                  extract.quantity = "upi")
  
  lpi <- 
    excessExtract(ds = excess.obj,
                  syndrome = syndrome1,
                  extract.quantity = "lpi")
  
  obs <- 
    excessExtract(ds = excess.obj,
                  syndrome = syndrome1,
                  extract.quantity = "y")
  
  denom <- 
    excessExtract(ds = excess.obj,
                  syndrome = syndrome1,
                  extract.quantity = "denom")
  
  res1 <- abind(pred[,,1],lpi[,,1],upi[,,1], obs[,,1],unexplained.cases[,,1],denom[,,1], along=3)
  dimnames(res1)[[1]] <-
    as.character(dates1)
  dimnames(res1)[[3]] <-
    c('pred','lpi','upi','obs','unexplained.cases','denom')
  
  res1.m <- melt(res1)
  res1.c <- dcast(res1.m,  Var1+Var2~Var3)
  names(res1.c)[1:2] <-c('week_start','state')
  res1.c$week_start <- as.Date(res1.c$week_start)
  res1.c$week_end <- as.date(res1.c$week_start) +days(6)
  mmwr.rs <- mmwr_week(res1.c$week_start)
  res2<- cbind.data.frame(mmwr.rs, res1.c)
  
  return(res2)
}