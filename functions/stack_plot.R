#Ds should have the following columns: 
# 'obs', 
# 'pred', 
# 'covid.track.death', 
# 'week_start'
# 'excess_pic'

stack_plot_func <- function(ds, legend=T){
ds$excess <- ds$obs - ds$pred

ds$band1 <- ds$pred +
  ds$covid.track.death

#ds$band2 <- ds$pred +
#  ds$excess_pic

ds <- ds[!is.na(ds$pred),]

ds <- ds[ds$week_start>= as.Date('2020-03-01'),]

ds$week_end <- ds$week_start + days(6)

yrange.pneu <- range(c(ds$pred, ds$obs))

plot(ds$week_end, ds$pred, type='l', ylim=yrange.pneu, col='darkgray', bty='l', ylab='All-cause Deaths', xlab='', lty=2)

points(ds$week_end, ds$obs, type='l', col='black', bty='l')

polygon(c(ds$week_end, rev(ds$week_end)), c(ds$pred, rev(ds$band1)),col =rgb(141/255,160/255,203/255, alpha = 0.9), border = NA )

# polygon(c(ds$week_end, rev(ds$week_end)), c(ds$band1, rev(ds$band2)),col = rgb(252/255,141/255,98/255, alpha = 0.9), border = NA )
# 
# polygon(c(ds$week_end, rev(ds$week_end)), c(ds$band2, rev(ds$obs)),col = rgb(102/255,194/255,165/255, alpha = 0.9), border = NA )

points(ds$week_end, ds$pred, type='l', ylim=yrange.pneu, col='black', lty=2, lwd=2)

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