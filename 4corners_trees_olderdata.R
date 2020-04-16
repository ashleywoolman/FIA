library(tidyverse)
library(ggplot2)



Utah<-  read.csv("Older_4corners_Treedata/UTTreesCond.csv")
Colorado<- read.csv("Older_4corners_Treedata/COTreesCond.csv")
Arizona<- read.csv("Older_4corners_Treedata/AZTreesCond.csv")
NewMexico<- read.csv("Older_4corners_Treedata/NMTreesCond.csv")

Fourcorners<- rbind(Utah, Colorado, Arizona, NewMexico)


### subset data
subset<- c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "COND_CYCLE", "PLOT", "SPCD", "STATUSCD", "DIACALC")
Fcorners<- Fourcorners[subset]

Fcorners$STATECD<- factor(Fcorners$STATECD)
Fcorners$UNITCD<- factor(Fcorners$UNITCD)
Fcorners$COUNTYCD<-factor(Fcorners$COUNTYCD)
Fcorners$COND_CYCLE<- factor(Fcorners$COND_CYCLE)
Fcorners$PLOT<- factor(Fcorners$PLOT)
Fcorners$STATUSCD<- factor(Fcorners$STATUSCD)
Fcorners$SPCD<- factor(Fcorners$SPCD)

summary(Fcorners)

### make tree count variable by year, state, cycle, species, status
Fcorners$treeexists<- as.logical(Fcorners$DIACALC)

treecount<- Fcorners %>% group_by(INVYR, STATECD, UNITCD, COUNTYCD, COND_CYCLE, PLOT, SPCD, STATUSCD) %>% 
  summarise(treecount=sum(treeexists))



### filter by species of interest, status of 1/2
treectspp<- treecount %>% 
  group_by(INVYR, STATECD, COND_CYCLE, SPCD, STATUSCD)%>% filter(SPCD %in% c(15, 106,122, 202, 746, 814)) %>%
  filter(STATUSCD %in% c(1, 2))
summary(treectspp)



####changing variables names
treectspp$STATECD<-factor(treectspp$STATECD, levels=c(4, 49, 8, 35), labels=c("AZ", "UT", "CO", "NM"))

treectspp$SPCD<-factor(treectspp$SPCD, levels=c(15, 106, 122, 202, 746, 814), 
                      labels=c("WhiteFir", "PinyonPine", "PonderosaPine", "DouglasFir", "Aspen", "GambelOak"))

treectspp$STATUSCD<-factor(treectspp$STATUSCD, levels=c(1, 2),
                          labels=c("Live", "Dead"))



### Rename Columns
NewTreeCountLabels<-treectspp %>%
  rename(InventoryYear=INVYR, State=STATECD, Cycle=COND_CYCLE, Species=SPCD, Status=STATUSCD)
summary(NewTreeCountLabels)


##ggplot with pre 2000
ggplot(data=NewTreeCountLabels, mapping=aes(x=InventoryYear, y=treecount, linetype=Status, color=State))+
  geom_line()+ labs(x="Inventory year")+ labs(y="Tree Count")+
  facet_wrap(facets = vars(Species))


##ggplot only post 2000

after2000<- NewTreeCountLabels[treectspp$INVYR>2000, ]

ggplot(data=after2000, mapping=aes(x=InventoryYear, y=treecount, linetype=Status, color=State))+
  geom_line()+ labs(x="Inventory year")+ labs(y="Tree Count")+
  facet_wrap(facets = vars(Species))


summary(after2000)




##Basal Area by state, year, status, species

subset<- c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "COND_CYCLE", "PLOT", "SPCD", "STATUSCD", "DIACALC")
Fcorners<- Fourcorners[subset]


FcornersBA<- Fcorners %>% 
  group_by(INVYR, STATECD, COUNTYCD, UNITCD, COND_CYCLE, SPCD, STATUSCD)%>% filter(SPCD %in% c(15, 106,122, 202, 746, 814)) %>%
  filter(STATUSCD %in% c(1, 2)) %>% filter(INVYR>1999)

summary(FcornersBA)


### basal area
FcornersBA$dbhsquared<- FcornersBA$DIACALC^2 
FcornersBA$TreeBasalAreaSqft<- FcornersBA$dbhsquared*0.005454
FcornersBA$BasalAreaPerAcreSqft<- FcornersBA$TreeBasalAreaSqft*6.018046

####****Going to need to figure out how to apply a different expansion factor to all trees that are <5.0 dbh

### basal area/acre totals
BAtotals<- FcornersBA %>% group_by(STATECD, INVYR, COND_CYCLE, STATUSCD, SPCD) %>% 
  summarise(BAtotals=sum(BasalAreaPerAcreSqft))
summary(BAtotals)


### make PLOT count variable by year, state, species, status

### filter by species of interest, status of 1/2 BY STATE, UNIT, COUNTY, PLOT
treectspp_plot<- treecount %>% 
  group_by(INVYR, STATECD, UNITCD, COUNTYCD, PLOT, COND_CYCLE, SPCD, STATUSCD)%>% filter(SPCD %in% c(15, 106,122, 202, 746, 814)) %>%
  filter(STATUSCD %in% c(1, 2)) %>% filter(INVYR>1999)
treectspp_plot$plotexists<- as.logical(treectspp_plot$treecount)
allplotct<- treectspp_plot %>% group_by(STATECD, COND_CYCLE, STATUSCD, SPCD) %>% summarise(plotcount=sum(plotexists))
summary(allplotct)



# merge two data frames by state, year, species, status 
BAcounts <- merge(BAtotals,allplotct,by=c("STATECD", "COND_CYCLE", "STATUSCD", "SPCD"))


### now we have BA totals/acre and plot count. need to divide by plot count.
BAcounts$BAperacreSqft<- (BAcounts$BAtotals/BAcounts$plotcount) 


BAcounts$STATUSCD<- factor(BAcounts$STATUSCD)
BAcounts$STATECD<- factor(BAcounts$STATECD)
BAcounts$SPCD<- factor(BAcounts$SPCD)

ggplot(data=BAcounts, mapping=aes(x=COND_CYCLE, y=BAperacreSqft, linetype=STATUSCD, color=STATECD))+
  geom_line()+ labs(x="Cycle")+ labs(y="BA/acre Sqft")+
  facet_wrap(facets = vars(SPCD))

### this includes NM and AZ cycle 2 which was still annualized inventory and cannot be compared to their cycle 3 & 4...

