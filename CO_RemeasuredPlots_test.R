COcond<-read.csv("New R Files/COND/COcond2000")
COtree<-read.csv("New R Files/TREE/COtree2000.csv")

library(tidyverse)
library(dplyr)


### Identify resampled plots on Cond table

### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
COcond$SUCP<- apply(COcond[,cols], 1, paste, collapse="_")

## create subset of plots w cycle 3
cycle3plots<- unique(COcond[COcond$CYCLE==3,]$SUCP)

## use subset to get all remeasured plots
newplotsCOND<- COcond %>% filter(CYCLE==3|(CYCLE==2 & SUCP%in%cycle3plots))
dim(newplotsCOND)
summary(newplotsCOND)
length(unique(newplotsCOND$SUCP))
table(newplotsCOND$SUCP)


# check to see how many are in cycle 2
cycle2<- COcond %>% filter(CYCLE==2 & SUCP%in%cycle3plots)
dim(cycle2)




### Use same method to identify remeasured plots in Tree table

### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
COtree$SUCP<- apply(COtree[,cols], 1, paste, collapse="_")

## create subset of plots w cycle 3
cycle3plots<- unique(COtree[COtree$CYCLE==3,]$SUCP)

## use subset to get all remeasured plots
newplotsTREE<- COtree %>% filter(CYCLE==3|(CYCLE==2 & SUCP%in%cycle3plots))
dim(newplotsTREE)
summary(newplotsTREE)
length(unique(newplotsTREE$SUCP))
table(newplotsTREE$SUCP)

###*** new dataframe of only resmpled plots in CO: newplotsTREE



### filter by species of interest, status of 1/2
COtree<- newplotsTREE %>% 
  group_by(INVYR, STATECD, CYCLE, SPCD, STATUSCD)%>% filter(SPCD %in% c(15, 106,122, 202, 746, 814)) %>%
  filter(STATUSCD %in% c(1, 2))
summary(COtree)



### basal area
COtree$dbhsquared<- COtree$DIACALC^2 
COtree$TreeBasalAreaSqft<- COtree$dbhsquared*0.005454
COtree$BasalAreaPerAcreSqft<- COtree$TreeBasalAreaSqft*6.018046

####****Going to need to figure out how to apply a different expansion factor to all trees that are <5.0 dbh

BAtotalbyspp<- COtree %>% group_by(STATECD, CYCLE, SPCD) %>% summarise(BAtotalacre=sum(BasalAreaPerAcreSqft))

plotcount<- COtree %>% group_by(STATECD, PLOT) %>% summarise(BAtotalplot=sum(BasalAreaPerAcreSqft))

plotcount$PLOT<- factor(plotcount$PLOT)
summary(plotcount)
dim(plotcount)
#### *** there are 2178 plots

BAtotalbyspp$totalBAacre<- BAtotalbyspp$BAtotalacre/2178


### Cant say that of 2178 , all species were present. 


