Teams<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthApp/MLBTeamsAT")
BatSzn<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthApp/MLBBattersAT")
PitchSzn<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthApp/MLBPitchersAT")
hit<-BatSzn
df<-hit%>%
  group_by(playerID)%>%
  arrange(yearID)%>%
  mutate(Exp = c(1:length(G)))%>%
  ungroup()%>%
  group_by(yearID)%>%
  mutate(G = G/max(G))%>%
  mutate(carval = NA)%>%
  ungroup()%>%
  mutate(Era = ifelse(yearID<1920,1,NA))%>%
  mutate(Era = ifelse((yearID<1942&is.na(Era)==TRUE),2,Era))%>%
  mutate(Era = ifelse((yearID<1961&is.na(Era)==TRUE),3,Era))%>%
  mutate(Era = ifelse((yearID<1977&is.na(Era)==TRUE),4,Era))%>%
  mutate(Era = ifelse((yearID<1994&is.na(Era)==TRUE),5,Era))%>%
  mutate(Era = ifelse((yearID<2006&is.na(Era)==TRUE),6,Era))%>%
  mutate(Era = ifelse((is.na(Era)==TRUE),7,Era)) %>%
  mutate(Era = as.factor(Era)) %>%
  mutate(FS = .95*OffFS+.05*DefFS)
for (i in c(2:25)){
  dfni <- df %>% filter(Exp !=i)
  dfi<- df %>% 
    arrange(Exp)%>%
    filter(Exp <=i)%>%
    group_by(playerID)%>%
    mutate(carval = sum(FloValue[1:(i-1)])/(i-1))%>%
    ungroup%>%
    filter(Exp == i)
  df<- rbind(dfni,dfi)
}
lastyear <- df %>%
  mutate(yearID = yearID +1)%>%
  mutate(lag1G = G)%>%
  mutate(lag1Val= FS)%>%
  select(playerID,yearID,lag1G,lag1Val)
twoyear <- df %>%
  mutate(yearID = yearID +2)%>%
  mutate(lag2G = G)%>%
  mutate(lag2Val= FS)%>%
  select(playerID,yearID,lag2G,lag2Val)
df <- left_join(df,lastyear, by = c("yearID","playerID"))
df <- left_join(df,twoyear, by = c("yearID","playerID"))

dfmissedlast <- df %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==TRUE&is.na(lag2Val)==FALSE)
linml<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag2G+lag2Val,data = dfmissedlast)
dfmissedlast<-dfmissedlast %>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  mutate(pred=predict(linml))

dfrook <- df %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==TRUE&is.na(lag2Val)==TRUE)%>%
  mutate(carval = ifelse(is.na(carval)==TRUE,0,carval))
linrook<-lm(FloValue~factor(Exp)*carval+teamID+Era,data = dfrook)
dfrook<-dfrook %>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  mutate(pred=predict(linrook))
dfsecyr <- df  %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==TRUE)
linsy<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag1G+lag1Val,data = dfsecyr)
dfsecyr<-dfsecyr %>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  mutate(pred=predict(linsy))


df<- df %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==FALSE)
dfmod<-df%>%
  mutate(Age = factor(yearID-birthYear))%>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  na.omit()%>%
  mutate(change = lag1Val-lag2Val)
lin<- lm(FloValue~Era+lag1Val+lag2Val+carval*factor(Exp)+lag1Val*lag1G+lag2Val*lag2G+teamID+change*factor(Exp), data = dfmod)
summary(lin)
dfreshit<- dfmod %>%
  mutate(pred = predict(lin))%>%
  select(-change)
dfreshit <- rbind(dfreshit,dfsecyr,dfmissedlast,dfrook)
sals<- read.csv("~/Downloads/baseballdatabank-2022.2/contrib/Salaries.csv")%>%
  select(-teamID,-lgID)
dfsal<-left_join(dfreshit,sals,by=c("playerID","yearID"))%>%
  filter(is.na(salary)==FALSE)%>%
  group_by(yearID)%>%
  mutate(salary = (salary-mean(salary))/sd(salary))%>%
  ungroup%>%
  mutate(salary=(salary+1.5)^.15)
newlin<- lm(salary~Era+lag1Val+lag2Val+carval*factor(Exp)+lag1Val*lag1G+lag2Val*lag2G,data = dfsal)
summary(newlin)
#dffullhit<-dfsal%>%
 # mutate(predsal = predict(newlin))%>%
  #mutate(predsal = (predsal)^(20/3)-1.5)%>%
  #mutate(salary = (salary)^(20/3)-1.5)%>%
  #mutate(howgood= predsal-salary)
#########################################################################
########Pitching#########################################################
#########################################################################
df<-PitchSzn%>%
  group_by(playerID)%>%
  arrange(yearID)%>%
  mutate(Exp = c(1:length(G)))%>%
  ungroup()%>%
  group_by(yearID)%>%
  mutate(IP = IP/max(IP))%>%
  mutate(carval = NA)%>%
  ungroup()%>%
  mutate(Era = ifelse(yearID<1920,1,NA))%>%
  mutate(Era = ifelse((yearID<1942&is.na(Era)==TRUE),2,Era))%>%
  mutate(Era = ifelse((yearID<1961&is.na(Era)==TRUE),3,Era))%>%
  mutate(Era = ifelse((yearID<1977&is.na(Era)==TRUE),4,Era))%>%
  mutate(Era = ifelse((yearID<1994&is.na(Era)==TRUE),5,Era))%>%
  mutate(Era = ifelse((yearID<2006&is.na(Era)==TRUE),6,Era))%>%
  mutate(Era = ifelse((is.na(Era)==TRUE),7,Era)) %>%
  mutate(Era = as.factor(Era)) 
for (i in c(2:28)){
  dfni <- df %>% filter(Exp !=i)
  dfi<- df %>% 
    arrange(Exp)%>%
    filter(Exp <=i)%>%
    group_by(playerID)%>%
    mutate(carval = sum(FloValue[1:(i-1)])/(i-1))%>%
    ungroup%>%
    filter(Exp == i)
  df<- rbind(dfni,dfi)
}
lastyear <- df %>%
  mutate(yearID = yearID +1)%>%
  mutate(lag1G = IP)%>%
  mutate(lag1Val= FloStrength)%>%
  select(playerID,yearID,lag1G,lag1Val)
twoyear <- df %>%
  mutate(yearID = yearID +2)%>%
  mutate(lag2G = IP)%>%
  mutate(lag2Val= FloStrength)%>%
  select(playerID,yearID,lag2G,lag2Val)
df <- left_join(df,lastyear, by = c("yearID","playerID"))
df <- left_join(df,twoyear, by = c("yearID","playerID"))

dfmissedlast <- df %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==TRUE&is.na(lag2Val)==FALSE)%>%
  filter(is.na(carval)==FALSE)
linml<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag2G+lag2Val,data = dfmissedlast)
dfmissedlast<-dfmissedlast %>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  mutate(pred=predict(linml))

dfrook <- df %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==TRUE&is.na(lag2Val)==TRUE)%>%
  filter(is.na(teamID)==FALSE)%>%
  mutate(carval = ifelse(is.na(carval)==TRUE,0,carval))
linrook<-lm(FloValue~factor(Exp)*carval+teamID+Era,data = dfrook)
dfrook<-dfrook %>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  mutate(pred=predict(linrook))
dfsecyr <- df  %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==TRUE)
linsy<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag1G+lag1Val,data = dfsecyr)
dfsecyr<-dfsecyr %>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  mutate(pred=predict(linsy))


df<- df %>%
  select(-HOF)%>%
  filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==FALSE)
dfmod<-df%>%
  mutate(Age = factor(yearID-birthYear))%>%
  select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
  na.omit()
lin<- lm(FloValue~Era+lag1Val+lag2Val+carval*Exp+lag1Val*lag1G+lag2Val*lag2G+teamID, data = dfmod)
summary(lin)
dfrespit<- dfmod %>%
  mutate(pred = predict(lin))
dfrespit <- rbind(dfrespit,dfsecyr,dfmissedlast,dfrook)
sals<- read.csv("~/Downloads/baseballdatabank-2022.2/contrib/Salaries.csv")%>%
  select(-teamID,-lgID)
dfsal<-left_join(dfrespit,sals,by=c("playerID","yearID"))%>%
  filter(is.na(salary)==FALSE)%>%
  group_by(yearID)%>%
  mutate(salary = (salary-mean(salary))/sd(salary))%>%
  ungroup%>%
  mutate(salary=(salary+1.5)^.15)
newlin<- lm(salary~Era+lag1Val+lag2Val+carval*factor(Exp)+lag1Val*lag1G+lag2Val*lag2G,data = dfsal)
summary(newlin)
#dffullpitch<-dfsal%>%
 # mutate(predsal = predict(newlin))%>%
  #mutate(predsal = (predsal)^(20/3)-1.5)%>%
  #mutate(salary = (salary)^(20/3)-1.5)%>%
  #mutate(howgood= predsal-salary)

dffull <- dfreshit %>%
  select(yearID,teamID,pred,Exp)%>%
  group_by(teamID, yearID)%>%
  mutate(PlayerVal = sum(pred))%>%
  mutate(HExp = sum(Exp))%>%
  slice(1)%>%
  select(yearID,teamID,"HV"="PlayerVal",HExp)%>%
  ungroup()
dffull1 <- dfrespit %>%
  select(yearID,teamID,pred,Exp)%>%
  group_by(teamID, yearID)%>%
  mutate(PlayerVal = sum(pred))%>%
  mutate(PExp = sum(as.numeric(Exp)))%>%
  slice(1)%>%
  select(yearID,teamID,"PV"="PlayerVal",PExp)%>%
  ungroup()
dffull <- left_join(dffull,dffull1, by = c("yearID","teamID"))
teamsdf <- Teams %>%
  select(teamID,LgWin,Wpct,FloStrength,yearID,Era)
df <- left_join(dffull,teamsdf, by = c("yearID","teamID"))
lag1team <- df %>%
  mutate(lag1team = FloStrength)%>%
  mutate(lag1wpct= Wpct)%>%
  mutate(yearID = yearID+1)%>%
  select(yearID, lag1wpct,lag1team,teamID)
df <- left_join(df,lag1team, by = c("yearID","teamID"))
lag2team <- df %>%
  mutate(lag2team = FloStrength)%>%
  mutate(lag2wpct = Wpct)%>%
  mutate(yearID = yearID+2)%>%
  select(yearID, lag2wpct,lag2team,teamID)
df <- left_join(df,lag2team, by = c("yearID","teamID"))%>%
  na.omit()%>%
  mutate(HExp = log(HExp))%>%
  mutate(PExp = log(PExp))
write_csv(df, "/Users/seanfloersch/FloStrength/FloStrengthApp/mlbpredmod")
lin<-lm(Wpct~(HV+PV+lag1team+lag2team+lag1wpct+lag2wpct+HExp+PExp)*factor(Era),data = df)
summary(lin)
dfteampred <- df%>%
  mutate(predteam = predict(lin))
ou22<- dfteampred %>% filter(yearID == 2022)%>%
  mutate(predwins = predteam*162)%>%
  mutate(actwins = Wpct*162)
test<- dfteampred %>%
  group_by(yearID)%>%
  mutate(testpv = mean(PV))%>%
  mutate(testexpp = mean(PExp))%>%
  slice(1)%>%
  select(yearID, testpv,testexpp)
