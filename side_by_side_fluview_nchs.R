ds1 <- readRDS('./Data/cdc_covid_data/2020_05_15_16_00.rds')
ds1$end_week <- as.Date(substr(ds1$end_week,1,10))
ds1$total_deaths <- as.numeric(ds1$total_deaths)

fv1 <- wk19

fv1$TOTAL.DEATHS <-
  gsub(',','',fv1$TOTAL.DEATHS) 
fv1$TOTAL.DEATHS <-  as.numeric(as.character(fv1$TOTAL.DEATHS))
fv1$epiyr <-
  as.numeric(as.character(substr(fv1$SEASON,1,4)))

fv1$year <- fv1$epiyr
fv1$year[fv1$WEEK<=26] <-
  fv1$epiyr[fv1$WEEK<=26] +1
fv1$week.death <-
  mmwr_week_to_date(fv1$year, fv1$WEEK)+6
fv1 <- fv1[,c('SUB.AREA','year','week.death','TOTAL.DEATHS')]
names(fv1) <-c('SUB.AREA','year','week.death','fluview.deaths')

ds2 <- merge( ds1, fv1, by.x=c('state','end_week'), 
              by.y=c('SUB.AREA','week.death'))
ds2$nchs.higher <- 0
ds2$nchs.higher[ds2$fluview.deaths<ds2$total_deaths] <- 1
table(ds2$nchs.higher, ds2$state)
ds2 <- ds2[ds2$end_week <= as.Date('2020-04-18'),]
table(ds2$state, ds2$nchs.higher)
ds2.agg <- aggregate(ds2[,c('total_deaths', 'fluview.deaths')], 
                     by=list('state'=ds2$state), 
                     FUN=sum)
ds2.agg$diff <- ds2.agg$total_deaths - ds2.agg$fluview.deaths

fl1 <- ds2[ds2$state=='Florida',]
plot(fl1$total_deaths, type='l')
points(fl1$fluview.deaths, type='l', col='red')

#What if we look at aggregate deaths by week across all states

ds2.nat <- aggregate(ds2[,c('total_deaths', 'fluview.deaths')], 
                                by=list( 'date'=ds2$end_week), 
                                FUN=sum, na.rm=T)
ds2.nat$diff <- ds2.nat$total_deaths - ds2.nat$fluview.deaths

