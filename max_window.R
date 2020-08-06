##Calculate range when 90% of excess deaths occur by state
library(zoo)
library(lubridate)
d1 <- read.csv('./outputs/national_and_state_summary.csv')
 

optimal_mid_range <- function(states){
    #for(states in test.states ){
    #print(states)
 d1.state <- d1[d1$state==states & d1$year==2020,]
 d1.state$excess_all_cause_deaths[d1.state$excess_all_cause_deaths < 0] <- 0
 max.window <- nrow(d1.state)
 total_excess <- sum(d1.state$excess_all_cause_deaths, na.rm=T)
 window.length <- 2:max.window
 maxes <- t(sapply(window.length, function(x) rollsum(d1.state$excess_all_cause_deaths, k=x, fill=NA, align='left')))
 maxes[maxes/total_excess < 0.90 ] <- NA
 colnames(maxes) <- d1.state$week_end_date
 rownames(maxes) <- window.length
 maxes.not.miss <- maxes[rowSums(maxes, na.rm=T)>0,]
 maxes.not.miss <- maxes.not.miss[1,, drop=F]
 best.window.length <-  as.numeric(row.names(maxes.not.miss))
 
 best.start.week<- as.Date(  d1.state$week_end_date[which(maxes.not.miss == max(maxes.not.miss,na.rm=T))] )
 best.end.week <- as.Date(best.start.week) + lubridate::weeks(best.window.length)
 range_90_pct <- as.character(c(best.start.week, best.end.week))
  #}
 
  return(range_90_pct) 
}
 

test.states <- unique(d1$state)
test.states <- test.states[test.states!='US.agg']
ranges <- t(sapply( FUN=optimal_mid_range,test.states))
ranges <- cbind.data.frame(test.states, ranges)
names(ranges) <- c('state','start','end')
ranges$start <- as.Date(ranges$start)
ranges$end <- as.Date(ranges$end)
ranges$order <- rank(ranges$start, ties.method='first')

plot( y=ranges$order, x=ranges$start, xlim=as.Date(c('2020-01-01', '2020-08-01')), col='white', yaxt='n')
arrows(y0=ranges$order,  x0=ranges$start, x1=ranges$end , length=0)
text(y=ranges$order, x=ranges$start, ranges$state, cex=0.5)
#axis(side=2, at=ranges$order, ranges$state, cex.axis=0.5)

write.csv(ranges,'./outputs/short_window.csv')
