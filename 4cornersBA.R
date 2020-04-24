

## resampled, nontreated, pipo & associated sp.
piporesamptrt<- read.csv("nontrmt_resampled_pipoassoc.csv")

##basal area!

# Trees >4.9 dbh, live, only PIPO
Treequery1<- piporesamptrt %>% filter(DIACALC>4.9) %>% filter(STATUSCD==1| STATUSCD==2) %>% filter(SPCD==122)

### basal area
Treequery1$dbhsquared<- Treequery1$DIACALC^2 
Treequery1$BA_Tree_Sqft<- Treequery1$dbhsquared*0.005454
Treequery1$BA_PerAcre_Sqft<- Treequery1$BA_Tree_Sqft*6.018046

Treequery2<- Treequery1 %>% group_by(STATECD, cycle_aw, SUCP, SPCD, STATUSCD) %>% summarise(BAplotTotal_PerAcre_Sqft=sum(BA_PerAcre_Sqft))


 

ggplot(Treequery2, aes(x= cycle_aw, y=BAplotTotal_PerAcre_Sqft)) +
  geom_line()+
  facet_wrap(~ STATECD)

ggplot(data=Treequery2, mapping=aes(x=cycle_aw, y=BAplotTotal_PerAcre_Sqft, group=PLOT, color=PLOT))+
  geom_jitter()+
  geom_line()+ 
  geom_point()+
  facet_wrap(facets = vars(STATECD))


##by state (not plot)
Treequery3<- Treequery2 %>% group_by(STATECD, cycle_aw, SPCD, STATUSCD) %>% summarise(BAstateTotal_PerAcre_Sqft=sum(BAplotTotal_PerAcre_Sqft))

Treequery3$STATUSCD<-factor(Treequery3$STATUSCD)

ggplot(data=Treequery3, mapping=aes(x=cycle_aw, y=BAstateTotal_PerAcre_Sqft, linetype=STATUSCD))+
  geom_line()+ 
  geom_point()+
  facet_wrap(facets=vars(STATECD))





Treequery4<- Treequery1 %>% group_by(STATECD, cycle_aw, SUCP, SPCD, STATUSCD) %>% summarise(BAplotTotal_PerAcre_Sqft=sum(BA_PerAcre_Sqft))
Treequery4.5<- Treequery4 %>% group_by(STATECD, cycle_aw, SPCD, STATUSCD) %>% summarise(BAstateTotal_PerAcre_Sqft=sum(BAplotTotal_PerAcre_Sqft))

# Get plot count to divide
Treequery4$plotexists<- as.logical(Treequery4$BAplotTotal_PerAcre_Sqft)
plotcount<- Treequery4 %>% group_by(STATECD, cycle_aw, SPCD, STATUSCD) %>% summarise(plotcount=sum(plotexists))

# Merge two data frames by state, year, species, status 
Treequery5<- merge(Treequery4.5,plotcount,by=c("STATECD", "cycle_aw", "SPCD", "STATUSCD"))

### now we have BA totals/acre and plot count. need to divide by plot count.
Treequery5$BA_Avg_acre_sqft<- (Treequery5$BAstateTotal_PerAcre_Sqft/Treequery5$plotcount)

Treequery5$STATUSCD<- factor(Treequery5$STATUSCD)



ggplot(data=Treequery5, mapping=aes(x=cycle_aw, y=BA_Avg_acre_sqft, linetype=STATUSCD))+
  geom_line()+ 
  geom_point()+
  facet_wrap(facets=vars(STATECD))


####changing variables
Treequery5$STATECD<-factor(Treequery5$STATECD, levels=c(4, 49, 8, 35), labels=c("AZ", "UT", "CO", "NM"))


Treequery5$STATUSCD<-factor(Treequery5$STATUSCD, levels=c(1, 2),
                            labels=c("Live", "Dead"))
