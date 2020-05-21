#Ds should have the following columns: 
# 'obs', 
# 'pred', 
# 'covid.death.hybrid', 
# 'week_start'
# 'excess_pic'

stack_plot_func <- function(ds, legend=T){
ds <- ds[!is.na(ds$obs),]
ds$excess <- ds$obs - ds$pred

ds$excess_pi_unexp <- ds$excess_pic - ds$covid.death.hybrid
ds$excess_pi_unexp[ds$excess_pi_unexp<0] <-0

ds$band1 <- ds$pred +  ds$covid.death.hybrid
#ds$band1[ds$band1>= ds$obs] <- ds$obs[ds$band1>= ds$obs]

ds$band1.full <- ds$pred +  ds$covid.death.hybrid

ds$band2 <- ds$band1 +
  ds$excess_pi_unexp

ds$band2[ds$band2> ds$obs] <- ds$obs[ds$band2> ds$obs]

ds <- ds[!is.na(ds$pred),]

ds <- ds[ds$week_start>= as.Date('2020-03-01'),]

ds$week_end <- ds$week_start + days(6)

yrange.pneu <- range(c(ds$pred, ds$obs),0)

plot(ds$week_end, ds$pred, type='l', ylim=yrange.pneu, col='darkgray', bty='l', ylab='All-cause Deaths', xlab='', lty=2)


polygon(c(ds$week_end, rev(ds$week_end)), c(ds$band1, rev(ds$band2)),col = rgb(252/255,141/255,98/255, alpha = 0.9), border = NA )

polygon(c(ds$week_end, rev(ds$week_end)), c(ds$pred, rev(ds$band1)),col =rgb(141/255,160/255,203/255, alpha = 0.9), border = NA )

polygon(c(ds$week_end, rev(ds$week_end)), c(ds$band2, rev(ds$obs)),col = rgb(102/255,194/255,165/255, alpha = 0.9), border = NA )

#mask any polygons above observed
polygon(c(ds$week_end, rev(ds$week_end)), c(ds$obs, rev(rep(max(yrange.pneu),length(ds$obs) ))),col = 'white', border = NA )

#add back on the reported covids that are> than observed
polygon(c(ds$week_end, rev(ds$week_end)), c(ds$pred, rev(ds$band1.full)),col =rgb(141/255,160/255,203/255, alpha = 0.1), border = NA )

#bottom part of plot shaded
#polygon(c(ds$week_end, rev(ds$week_end)), c(rep(0, nrow(ds)), rev(ds$pred)),col ='lightgray', border = NA )


points(ds$week_end, ds$pred, type='l', ylim=yrange.pneu, col='black', lty=2, lwd=2)
points(ds$week_end, ds$obs, type='l', col='black', bty='l')

if(legend==T){
legend('bottomleft',inset=0.01, 
       pch=c(15,15,15, NA, NA),
       lty=c(NA, NA, NA, 1,2), 
       lwd=c(NA, NA, NA, 1,2), 
       legend=c(  'Unexplained Excess','Other Pneumonia & Influenza','Reported COVID-19', 'Observed Deaths', 'Expected Deaths'),
       col=c( rgb(102/255,194/255,165/255,alpha = 0.9),
              rgb(252/255,141/255,98/255, alpha = 0.9), 
              rgb(141/255,160/255,203/255, alpha = 0.9), 'black', 'black'),
       box.lty=0 ,
       bg="transparent")
}
}