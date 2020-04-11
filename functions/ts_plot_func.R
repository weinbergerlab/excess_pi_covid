ts.plot.func <-function(ds.plot=jh3,ylim.adj=0.7, death.var='deaths' ,states.plot=states.cdc ){
  
  max.test.all.state<-max(ds.plot$test.week.per.capita, na.rm=T)
 
 # states.cdc.order <- c(states.cdc[states.cdc %in% state.abb],
 #                       states.cdc[!(states.cdc %in% state.abb)]) 
  #plot.state.indices <- match( states.cdc.order,unique(ds.plot$state)  )
  
  plot.state.rank <- cbind.data.frame(state.index=1:dim(rr)[2],state.rank= rank(-rr[dim(rr)[1],,1]))
  plot.state.rank <- plot.state.rank[order(plot.state.rank$state.rank),]
  plot.state.indices <- plot.state.rank$state.index
  
for(i in plot.state.indices){
  ds.select <- ds.plot[ds.plot$state==states.cdc[i],]
  max.test.this.state <- max(ds.select$test.week.per.capita, na.rm=T)
  ave_pi <- mean(ds.select$total_pi, na.rm=T)
  y.range1<-range(c(ds.select[,death.var], (ds.select$excess_pi/ds.select$percent_complete)), na.rm=T)
  
  #Makes all have same relative range
  #ave.range <- c(-ave_pi*0.2, ave_pi*ylim.adj)
  
  ave.range <-y.range1 
  
  ds.select$death.early <- ds.select[,death.var]
  ds.select$death.early[is.na(ds.select$excess_pi)] <- NA
  par(new=FALSE)
  plot(ds.select$date+6         ,
       ds.select[,death.var],
       type='l',
       col='#377eb8',
       ylim=ave.range,
       bty='l',
       lty=3,
       xlab='',
       ylab='',
       #main=paste(states.plot[i] )
       )
  abline(h=0, col='grey', lty=2)
  
  points(ds.select$date+6         ,
         ds.select$death.early, type='l', col='#377eb8',
         lty=1, lwd=1)
  
  points(ds.select$date+6         ,
         ds.select$excess_pi/ds.select$percent_complete, type='l', col='#e41a1c', lwd=1)
  #state.name.plot <-  state.name[match(states.cdc[i],state.abb)]
  if(states[i] %in% state.abb ){
    state.name.plot <-    
      state.name[match(states.cdc[i],state.abb)]
  }else{
    state.name.plot <- states.cdc[i]
  }
 upper <- ds.select$excess_deaths.lpi/ds.select$percent_complete
 lower <- ds.select$excess_deaths.upi/ds.select$percent_complete
 pis <- cbind(upper, lower)
 cl.max <- apply(pis,1, max) 
 cl.max <-cl.max[!is.na(cl.max)]
 cl.min <- apply(pis,1, min) 
 cl.min <- cl.min[!is.na(cl.min)]
 date.ci <- ds.select$date[1:length(cl.min)]
  polygon(c(date.ci+6, rev(date.ci+6)),
          c(cl.min, rev(cl.max)),
          col = rgb(1, 0,0, alpha = 0.05), 
          border = NA)
  
  #Scales axis 2
  scale.factor.test <- ave.range[2]/max.test.all.state
  #scale.factor.test <-1
  
  points(ds.select$date+6, scale.factor.test*ds.select$test.week.per.capita, type='l', lty=2, lwd=2, col='gray', yaxt='n',xaxt='n', ylab='', xlab='')
  
  axis(side=4,at=(c(0,2,4,6)*(scale.factor.test)) , labels=c(0,2,4,6), col.ticks='gray', col='gray', col.lab='gray' ,col.axis='gray')
  mtext("", side=4, line=2, col='gray', cex=0.75)
  text(as.Date('2020-02-06')+6, ave.range[2]*0.9,state.name.plot, pos=4, cex=0.85)
  
  box(lty = '1111', col = 'lightgray')
}

}
