ts.plot.func <-function(ds.plot=jh3,ylim.adj=0.7, death.var='deaths' ,states.plot=states.cdc , plot.order=F,plot.excess='pic' ){
  
  max.test.all.state<-max(ds.plot$test.week.per.capita[ds.plot$state %in% states.plot], na.rm=T)
 
#for(i in plot.state.indices){
for(i in 1:4){
    #print(i)
  if(plot.order){
    states.cdc = states.plot
  }
  ds.select <- ds.plot[ds.plot$state==states.cdc[i],]
  max.test.this.state <- max(ds.select$test.week.per.capita, na.rm=T)
  ave_pi <- mean(ds.select$pneumonia_influenza_covid, na.rm=T)
  if(plot.excess=='pic'){
  y.range1<-range(c(ds.select[,death.var], (ds.select$excess_pneumonia_influenza_covid)), na.rm=T)
  }else{
    y.range1<-range(c(ds.select[,death.var], (ds.select$excess_all_cause_death)), na.rm=T)
    
  }
  #Makes all have same relative range
  #ave.range <- c(-ave_pi*0.2, ave_pi*ylim.adj)
  
  ave.range <-y.range1 
  
   par(new=FALSE)
  plot(ds.select$week_end_date         ,
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
  
  if(plot.excess=='pic'){
    
  points(ds.select$week_end_date        ,
         ds.select$excess_pneumonia_influenza_covid, type='l', col='#e41a1c', lwd=1)
  }else{
    points(ds.select$week_end_date        ,
           ds.select$excess_all_cause_deaths, type='l', col='#e41a1c', lwd=1)
    
  }
    
    #state.name.plot <-  state.name[match(states.cdc[i],state.abb)]
  if(states[i] %in% state.abb ){
    state.name.plot <-    
      state.name[match(states.cdc[i],state.abb)]
  }else{
    state.name.plot <- states.cdc[i]
  }
  
  if(plot.excess=='pic'){
 upper <-  ds.select$pneumonia_influenza_covid  - ds.select$baseline_pi_upper  
 lower <-  ds.select$pneumonia_influenza_covid - ds.select$baseline_pi_lower 
  }else{
 upper <-  ds.select$all_cause_deaths  - ds.select$baseline_all_cause_lower  
 lower <-  ds.select$all_cause_deaths - ds.select$baseline_all_cause_upper 
    
  }
 
 pis <- cbind(upper, lower)
 cl.max <- apply(pis,1, max) 
 cl.max <-cl.max[!is.na(cl.max)]
 cl.min <- apply(pis,1, min) 
 cl.min <- cl.min[!is.na(cl.min)]
 date.ci <- ds.select$week_end_date[1:length(cl.min)]
  polygon(c(date.ci, rev(date.ci)),
          c(cl.min, rev(cl.max)),
          col = rgb(1, 0,0, alpha = 0.05), 
          border = NA)
  
  #Scales axis 2
  scale.factor.test <- ave.range[2]/max.test.all.state
  #scale.factor.test <-1
  
  points(ds.select$week_end_date, scale.factor.test*ds.select$test.week.per.capita, type='l', lty=2, lwd=2, col='gray', yaxt='n',xaxt='n', ylab='', xlab='')
  
  axis(side=4,at=(c(0,4, 8,  12)*(scale.factor.test)) , labels=c(0,4, 8,  12), col.ticks='gray', col='gray', col.lab='gray' ,col.axis='gray')
  mtext("", side=4, line=2, col='gray', cex=0.75)
  text(as.Date('2020-03-01')+6, ave.range[2]*0.9,state.name.plot, pos=4, cex=0.85)
  
  if(i==1){
    legend(as.Date('2020-03-05'),1000, legend=c('Excess deaths','COVID-19 deaths', 'Tests'), 
           col=c('#e41a1c','#377eb8','gray'),
           inset=0.01, lty=c(1,3,2),box.lty=0 ,lwd=c(1,1,2),
           bg="transparent", cex = 0.75)
  }
  
  box(lty = '1111', col = 'lightgray')
}


}
