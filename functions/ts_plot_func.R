ts.plot.func <-function(ds.plot=jh3, death.var='deaths' ,states.plot=states.cdc ){
  
y2.range.test <- range(ds.plot$test.week.per.capita, na.rm=T)
for(i in 1:length(states.plot)){
  ds.select <- ds.plot[ds.plot$state==states.plot[i],]
  y.range1<-range(c(ds.select[,death.var], ds.select$excess_pi/ds.select$percent_complete), na.rm=T)
  ds.select$death.early <- ds.select[,death.var]
  ds.select$death.early[is.na(ds.select$excess_pi)] <- NA
  par(new=FALSE)
  plot(ds.select$date         ,
       ds.select[,death.var],
       type='l',
       col='#e41a1c',
       ylim=c(0,max(y.range1)),
       bty='l',
       lty=3,
       xlab='',
       ylab='N Deaths',
       #main=paste(states.plot[i] )
       )
  
  points(ds.select$date         ,
         ds.select$death.early, type='l', col='#e41a1c',
         lty=1, lwd=2)
  
  points(ds.select$date         ,
         ds.select$excess_pi/ds.select$percent_complete, type='l', col='#377eb8', lwd=2)
#  abline(h=1, col='black')
  state.name.plot <-  state.name[match(states.plot[i],state.abb)]
 upper <- ds.select$excess_deaths.lpi/ds.select$percent_complete
 lower <- ds.select$excess_deaths.upi/ds.select$percent_complete
 pis <- cbind(upper, lower)
 cl.max <- apply(pis,1, max) 
 cl.max <-cl.max[!is.na(cl.max)]
 cl.min <- apply(pis,1, min) 
 cl.min <- cl.min[!is.na(cl.min)]
 date.ci <- ds.select$date[1:length(cl.min)]
  polygon(c(date.ci, rev(date.ci)),
          c(cl.min, rev(cl.max)),
          col = rgb(0, 0, 1, alpha = 0.05), 
          border = NA)
    par(new=TRUE)
  plot(ds.select$date, ds.select$test.week.per.capita, ylim=y2.range.test, type='l', lty=2, lwd=0.5, col='gray', yaxt='n',xaxt='n', ylab='', xlab='')
  axis(side=4,at=c(0,1,2,3,4,5) , labels=F)
  mtext("Tests per 1000", side=4, line=0.2, col='gray', cex=0.75)
  text(as.Date('2020-01-15'), y2.range.test[2]*0.9,state.name.plot, pos=4)
  
  box(lty = '1111', col = 'lightgray')
}

}