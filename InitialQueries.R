### 1. resampled plots. 2. remove treated plots. 3. filter species

library(tidyverse)
library(dplyr)

utcond2000<-read.csv("UTcond2000.csv")
utcond2000_1<- utcond2000 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1',
                                                          CYCLE==3 ~ '2'))
nmcond2000<-read.csv("NMcond2000.csv")
NMcond2000_1<- nmcond2000 %>% mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                                                          CYCLE==4 ~ '2'))
cocond2000<-read.csv("COcond2000.csv")
cocond2000_1<- cocond2000 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1',
                                                          CYCLE==3 ~ '2'))
azcond2000<-read.csv("AZcond2000.csv")

azcond2000_1<- azcond2000 %>% mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                                                          CYCLE==4 ~ '2'))


fcorners1<- rbind(utcond2000_1, NMcond2000_1, cocond2000_1, azcond2000_1)


write.csv(fcorners1, "4corners_Condcycleaw.csv")


### Identify resampled plots on Cond table

### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fcorners1$SUCP<- apply(fcorners1[,cols], 1, paste, collapse="_")

## create subset of plots w cycle 3
cycle2plots<- unique(fcorners1[fcorners1$cycle_aw==2,]$SUCP)

## use subset to get all remeasured plots
fcorners2<- fcorners1 %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%cycle2plots))

dim(fcorners2)
summary(fcorners2)
length(unique(newplotsCOND$SUCP))
table(newplotsCOND$SUCP)

fcorners2$SUCP<- factor(fcorners2$SUCP)
summary(fcorners2)

# check to see how many are in cycle 2
cycle2<- fcorners1 %>% filter(cycle_aw==1 & SUCP%in%cycle2plots)
dim(cycle2)

# re-order columns
fcorners3<- fcorners2[,c(1:7, 38, 37, 8:36)]


write.csv(fcorners3, "4corners_Condresampled.csv")



## Remove plots with treatment
condresampled<- read.csv("4corners_Condresampled.csv")

treatment<- condresampled %>% filter(condresampled$TRTCD1==10 | TRTCD2==10 | TRTCD3==10)

notreatmentcond<-condresampled[!(condresampled$SUCP %in% treatment$SUCP),]
## this tells us of the 64,990 plots+conditions, 322 had a treatment. This removes plots+conditions from any time (cycle 1 and 2) that contain SUCP in treatment. the new 
## "notreatmentcond" has 64,258. So, 64,990-64,258= 732. 732 is greater than 322*2=644, presumably from plots that have more than one condition (88 extra). I think this is valid.
## So, we can apply the same method to resampled tree data frame below. 

write.csv(notreatmentcond, "nontreatedCOND.csv")





## Identify resampled plots for Tree table

uttree2000<-read.csv("UTtree2000.csv")

uttree2000_1<- uttree2000 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1',
                                                          CYCLE==3 ~ '2'))
nmtree2000<-read.csv("NMtree2000.csv")
NMtree2000_1<- nmtree2000 %>% mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                                                          CYCLE==4 ~ '2'))
cotree2000<-read.csv("COtree2000.csv")
cotree2000_1<- cotree2000 %>% mutate(cycle_aw = case_when(CYCLE==2 ~ '1',
                                                          CYCLE==3 ~ '2'))
aztree2000<-read.csv("AZtree2000.csv")
aztree2000_1<- aztree2000 %>% mutate(cycle_aw = case_when(CYCLE==3 ~ '1',
                                                          CYCLE==4 ~ '2'))
cotree2000_1<- cotree2000_1[,-1]
fcornerstrees1<- rbind(uttree2000_1, NMtree2000_1, cotree2000_1, aztree2000_1)

write.csv(fcornerstrees1, "fcorners_trees.csv")


### create new variable that pastes together state, unit, countycd, plot: "SUCP"
cols<- c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')
fcornerstrees1$SUCP<- apply(fcornerstrees1[,cols], 1, paste, collapse="_")

## create subset of plots w cycle 2
cycle2plots<- unique(fcornerstrees1[fcornerstrees1$cycle_aw==2,]$SUCP)

## use subset to get all remeasured plots
fcornerstrees2<- fcornerstrees1 %>% filter(cycle_aw==2|(cycle_aw==1 & SUCP%in%cycle2plots))



# re-order columns
fcornerstrees_resampled<- fcornerstrees2[,c(1:9, 27, 26, 10:25)]


write.csv(fcornerstrees_resampled, "fcorners_trees_resampled.csv")


## Remove plots that had treatment/cutting
treeresampled<- read.csv("fcorners_trees_resampled.csv")
condresampled<- read.csv("4corners_Condresampled.csv")

treatment<- condresampled %>% filter(condresampled$TRTCD1==10 | TRTCD2==10 | TRTCD3==10)

notreatmenttree<-treeresampled[!(treeresampled$SUCP %in% treatment$SUCP),]



## Select plots with PIPO and associated species:

# what species do we have?
notreatmenttree$SPCD<- factor(notreatmenttree$SPCD)
summary(notreatmenttree)
# how many?
length(unique(notreatmenttree$SPCD))
table(notreatmenttree$SPCD)
unique(notreatmenttree$SPCD)

#subset of PIPO, keeping SUCP
pipoplots<- unique(notreatmenttree[notreatmenttree$SPCD==122,]$SUCP)

#Create dataframe that contains plots that have had PIPO at any time, and the associated species
pipoplots2<- notreatmenttree %>% filter(SPCD==122|(SPCD==814 & SUCP%in%pipoplots|
                                                             SPCD==15 & SUCP%in%pipoplots |
                                                             SPCD==18 & SUCP%in%pipoplots |
                                                             SPCD==19 & SUCP%in%pipoplots |
                                                             SPCD==51 & SUCP%in%pipoplots |
                                                             SPCD==59 & SUCP%in%pipoplots |
                                                             SPCD==63 & SUCP%in%pipoplots |
                                                             SPCD==65 & SUCP%in%pipoplots | 
                                                             SPCD==66 & SUCP%in%pipoplots |
                                                             SPCD==69 & SUCP%in%pipoplots |
                                                             SPCD==93 & SUCP%in%pipoplots |
                                                             SPCD==96 & SUCP%in%pipoplots |
                                                             SPCD==102 & SUCP%in%pipoplots |
                                                             SPCD==106 & SUCP%in%pipoplots |
                                                             SPCD==108 & SUCP%in%pipoplots |
                                                             SPCD==113 & SUCP%in%pipoplots |
                                                             SPCD==114 & SUCP%in%pipoplots |
                                                             SPCD==118 & SUCP%in%pipoplots |
                                                             SPCD==133 & SUCP%in%pipoplots |
                                                             SPCD==134 & SUCP%in%pipoplots |
                                                             SPCD==140 & SUCP%in%pipoplots |
                                                             SPCD==142 & SUCP%in%pipoplots |
                                                             SPCD==143 & SUCP%in%pipoplots |
                                                             SPCD==202 & SUCP%in%pipoplots |
                                                             SPCD==313 & SUCP%in%pipoplots |
                                                             SPCD==322 & SUCP%in%pipoplots |
                                                             SPCD==353 & SUCP%in%pipoplots |
                                                             SPCD==362 & SUCP%in%pipoplots |
                                                             SPCD==374 & SUCP%in%pipoplots |
                                                             SPCD==475 & SUCP%in%pipoplots |
                                                             SPCD==746 & SUCP%in%pipoplots))
summary(pipoplots2)
unique(pipoplots2$SPCD)
table(pipoplots2$SPCD)

## resampled, non treated, pipo&associated species plots:
write.csv(pipoplots2, "nontrmt_resampled_pipoassoc.csv")



### Get the plots identified in pipoplots into "notreatedcond"  ??????

nontreatedcond<- read.csv("nontreatedCOND.csv")

CONDpiponontreat<-nontreatedcond[!(nontreatedcond$SUCP %in% pipoplots2$SUCP),]

# this only removed 4,000 plots+conditions?







