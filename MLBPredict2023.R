
## get current rosters
BatSzn<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBBattersAT")

teams <- c("HOU", "TOR", "LAA", "BOS", "WSH", "CHW", "CIN", "SD", "KC", "COL", "SF", "MIN", "LAD", "ATL", "PHI", "DET", "PIT", "BAL", "NYM", "NYY", "MIA", "ARI", "OAK", "TB", "STL", "CLE", "CHC", "TEX", "MIL", "SEA")
get_roster_data <- function(team){
  url <- str_c("https://www.espn.com/mlb/team/roster/_/name/",team, sep="")
  h <- read_html(url) 
  name <- html_nodes(h, ".Table__TD+ .Table__TD .AnchorLink") %>% html_text
  position <- html_nodes(h, ".Table__TD:nth-child(3) .inline") %>% html_text
  a <- data.frame(name, position) %>%
    mutate(Team = team)
  return(a)
}
roster <- map_df(.x= teams, .f=get_roster_data)
dups <- which(duplicated(roster$name)==TRUE)
dups <- roster[dups,]
dups <- unique(dups$name)
uniroster = roster%>% filter(!name %in% dups)%>%
  rename("Name"="name")%>%
  mutate(Name = str_remove(Name, "Jr."))%>%
  mutate(Name = str_remove(Name, "Sr."))%>%
  mutate(Name = str_remove_all(Name, "\\."))%>%
  mutate(Name = trimws(Name))
dupsroster <- roster%>% filter(name %in% dups)
####################################
#Hitters
####################################
predhitters<- function(uniroster){
  ## get past
  #####last two seasons
  lastplayers<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBBattersAT")%>%
    filter(yearID>2020)
  hitter21<- lastplayers %>%
    filter(yearID==2021)%>%
    mutate(FS21 = .95*OffFS+.05*DefFS)%>%
    mutate(G21 =G /162)%>%
    select(playerID,Name,FS21,G21)
  hitter22<- lastplayers %>%
    filter(yearID==2022)%>%
    mutate(FS22 = .95*OffFS+.05*DefFS)%>%
    mutate(G22 =G /162)%>%
    select(playerID,Name,FS22,G22)
  lastplayers <- full_join(hitter21,hitter22, by = c("playerID"))%>%
    mutate(Name = ifelse(is.na(Name.x)==TRUE,Name.y,Name.x))%>%
    select(-Name.x,-Name.y)
  ######career
  hitcar<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBBattersAT")%>%
    group_by(playerID)%>%
    mutate(Exp = length(FloValue))%>%
    mutate(FloValue = sum(FloValue) /Exp)%>%
    slice(1)%>%
    ungroup()%>%
    select(playerID, FloValue, Exp)
  hitpred <- left_join(lastplayers,hitcar, by = "playerID")%>%
    mutate(Era =as.factor(7))%>%
    mutate(Name = str_remove(Name,"\\.\\s"))%>%
    mutate(Name = str_remove(Name,"\\."))%>%
    mutate(Name = str_replace(Name,"ñ","n"))%>%
    mutate(Name = str_replace(Name,"á","a"))%>%
    mutate(Name = str_replace(Name,"ó","o"))%>%
    mutate(Name = str_replace(Name,"ú","u"))%>%
    mutate(Name = str_replace(Name,"é","e"))%>%
    mutate(Name = str_replace(Name,"í","i"))%>%
    mutate(Name = trimws(Name))
  #complete
  hitcom23<- left_join(uniroster,hitpred, by = "Name")%>%
    na.omit()%>%
    mutate(lag1Val = FS22)%>%
    mutate(lag2Val = FS21)%>%
    mutate(lag1G = G22)%>%
    mutate(lag2G = G21)%>%
    select(Name, lag1Val,lag2Val,lag1G,lag2G,Era,"carval"="FloValue",Exp,"teamID"="Team")%>%
    mutate(teamID = ifelse(teamID=="KC","KCR",teamID))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    mutate(Exp = Exp +1)%>%
    mutate(Exp = factor(Exp))%>%
    mutate(change = lag1Val-lag2Val)
  #missedlast
  hitmissedlast23<- left_join(uniroster,hitpred, by = "Name")%>%
    filter(is.na(FS22)==TRUE&is.na(FS21)==FALSE)%>%
    mutate(lag2Val = FS21)%>%
    mutate(lag2G = G21)%>%
    select(Name,lag2Val,lag2G,Era,"carval"="FloValue",Exp,"teamID"="Team")%>%
    mutate(teamID = ifelse(teamID=="KC","KCR",teamID))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    mutate(Exp = Exp +1)%>%
    mutate(Exp = factor(Exp))
  #second year
  hitsecondyear <- hitpred%>%
    filter(is.na(FS21)==TRUE)%>%
    select(-FS21,-G21)%>%
    mutate(Name = str_remove_all(Name,"\\s"))%>%
    mutate(Name = ifelse(Name =="MichaelHarris","MichaelHarrisII",Name))
  unirosteredit <- uniroster%>%
    mutate(realname = Name)%>%
    mutate(Name =str_remove_all(Name,"\\s"))
  hitsecondyear23<- left_join(unirosteredit,hitsecondyear, by = "Name")%>%
    na.omit()%>%
    mutate(lag1Val = FS22)%>%
    mutate(lag1G = G22)%>%
    select("Name"="realname", lag1Val,lag1G,Era,"carval"="FloValue",Exp,"teamID"="Team")%>%
    mutate(teamID = ifelse(teamID=="KC","KCR",teamID))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    mutate(Exp = Exp +1)%>%
    mutate(Exp = factor(Exp))
  #rookies
  complayers <- hitcom23$Name
  mlplayers <- hitmissedlast23$Name
  syplayers <- hitsecondyear23$Name
  alreadypred <- c(complayers,mlplayers,syplayers)
  hitrook23<-uniroster%>%
    filter(!Name%in%alreadypred)%>%
    filter(position != "SP"&position!="RP")%>%
    mutate(teamID = ifelse(Team=="KC","KCR",Team))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    select(Name, teamID)%>%
    mutate(Exp =factor(1))%>%
    mutate(carval = 0)%>%
    mutate(Era = factor(7))
  
  ## get models
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
  HitMissLastMod<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag2G+lag2Val,data = dfmissedlast)
  
  
  dfrook <- df %>%
    select(-HOF)%>%
    filter(is.na(lag1Val)==TRUE&is.na(lag2Val)==TRUE)%>%
    mutate(carval = ifelse(is.na(carval)==TRUE,0,carval))
  HitRookMod<-lm(FloValue~factor(Exp)*carval+teamID+Era,data = dfrook)
  dfsecyr <- df  %>%
    select(-HOF)%>%
    filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==TRUE)
  HitSecYrMod<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag1G+lag1Val,data = dfsecyr)
  
  
  df<- df %>%
    select(-HOF)%>%
    filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==FALSE)
  dfmod<-df%>%
    mutate(Age = factor(yearID-birthYear))%>%
    select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID)%>%
    na.omit()%>%
    mutate(change = lag1Val-lag2Val)
  HitComMod<- lm(FloValue~Era+lag1Val+lag2Val+carval*factor(Exp)+lag1Val*lag1G+lag2Val*lag2G+teamID+change*factor(Exp), data = dfmod)
  
  ## predict
  predictfull = predict(HitComMod, newdata = hitcom23)
  hitcom23 <- hitcom23 %>%
    mutate(PredVal = predictfull)%>%
    select(Name, teamID, PredVal,Exp)
  predictsy = predict(HitSecYrMod, newdata = hitsecondyear23)
  hitsecondyear23 <- hitsecondyear23 %>%
    mutate(PredVal = predictsy)%>%
    select(Name, teamID, PredVal,Exp)
  predictml = predict(HitMissLastMod, newdata = hitmissedlast23)
  hitmissedlast23 <- hitmissedlast23 %>%
    mutate(PredVal = predictml)%>%
    select(Name, teamID, PredVal,Exp)
  predictrook = predict(HitRookMod, newdata = hitrook23)
  hitrook23 <- hitrook23 %>%
    mutate(PredVal = predictrook)%>%
    select(Name, teamID, PredVal,Exp)
  hitpred23 <- rbind(hitrook23,hitsecondyear23,hitmissedlast23,hitcom23)
  return(hitpred23)
}
predhit23<- predhitters(uniroster)
teamhit23<- predhit23%>%
  mutate(Exp = as.numeric(Exp))%>%
  arrange(-PredVal)%>%
  group_by(teamID)%>%
  mutate(HV = sum(PredVal))%>%
  mutate(HExp = log(sum(as.numeric(Exp))))%>%
  slice(1)%>%
  ungroup()%>%
  select(teamID,HV,HExp)
####################################
#Pitchers
####################################
predpitchers <- function(uniroster){
  PitchSzn<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBPitchersAT")
  ## get past
  #####last two seasons
  lastplayers<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBPitchersAT")%>%
    filter(yearID>2020)
  pitcher21<- lastplayers %>%
    filter(yearID==2021)%>%
    mutate(FS21 = FloStrength)%>%
    mutate(G21 =IP /max(IP))%>%
    select(playerID,Name,FS21,G21)
  pitcher22<- lastplayers %>%
    filter(yearID==2022)%>%
    mutate(FS22 = FloStrength)%>%
    mutate(G22 =IP /max(IP))%>%
    select(playerID,Name,FS22,G22)
  lastplayers <- full_join(pitcher21,pitcher22, by = c("playerID"))%>%
    mutate(Name = ifelse(is.na(Name.x)==TRUE,Name.y,Name.x))%>%
    select(-Name.x,-Name.y)
  ######career
  pitcar<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBPitchersAT")%>%
    group_by(playerID)%>%
    mutate(Exp = length(FloValue))%>%
    mutate(FloValue = sum(FloValue) /Exp)%>%
    slice(1)%>%
    ungroup()%>%
    select(playerID, FloValue, Exp)
  pitpred <- left_join(lastplayers,pitcar, by = "playerID")%>%
    mutate(Era =as.factor(7))%>%
    mutate(Name = str_replace(Name,"ñ","n"))%>%
    mutate(Name = str_replace(Name,"á","a"))%>%
    mutate(Name = str_replace(Name,"ó","o"))%>%
    mutate(Name = str_replace(Name,"ú","u"))%>%
    mutate(Name = str_replace(Name,"é","e"))%>%
    mutate(Name = str_replace(Name,"í","i"))%>%
    mutate(Name = trimws(Name))
  pitcom23<- left_join(uniroster,pitpred, by = "Name")%>%
    na.omit()%>%
    mutate(lag1Val = FS22)%>%
    mutate(lag2Val = FS21)%>%
    mutate(lag1G = G22)%>%
    mutate(lag2G = G21)%>%
    select(Name, lag1Val,lag2Val,lag1G,lag2G,Era,"carval"="FloValue",Exp,"teamID"="Team")%>%
    mutate(teamID = ifelse(teamID=="KC","KCR",teamID))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    mutate(Exp = Exp +1)%>%
    mutate(Exp = factor(Exp))%>%
    mutate(change = lag1Val-lag2Val)
  pitmissedlast23<- left_join(uniroster,pitpred, by = "Name")%>%
    filter(is.na(FS22)==TRUE&is.na(FS21)==FALSE)%>%
    mutate(lag2Val = FS21)%>%
    mutate(lag2G = G21)%>%
    select(Name,lag2Val,lag2G,Era,"carval"="FloValue",Exp,"teamID"="Team")%>%
    mutate(teamID = ifelse(teamID=="KC","KCR",teamID))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    mutate(Exp = Exp +1)%>%
    mutate(Exp = factor(Exp))
  #second year
  pitsecondyear <- pitpred%>%
    filter(is.na(FS21)==TRUE)%>%
    select(-FS21,-G21)%>%
    mutate(Name = str_remove_all(Name,"\\s"))
  unirosteredit <- uniroster%>%
    mutate(realname = Name)%>%
    mutate(Name =str_remove_all(Name,"\\s"))
  pitsecondyear23<- left_join(unirosteredit,pitsecondyear, by = "Name")%>%
    na.omit()%>%
    mutate(lag1Val = FS22)%>%
    mutate(lag1G = G22)%>%
    select("Name"="realname", lag1Val,lag1G,Era,"carval"="FloValue",Exp,"teamID"="Team")%>%
    mutate(teamID = ifelse(teamID=="KC","KCR",teamID))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    mutate(Exp = Exp +1)%>%
    mutate(Exp = factor(Exp))
  #rookies
  complayers <- pitcom23$Name
  mlplayers <- pitmissedlast23$Name
  syplayers <- pitsecondyear23$Name
  alreadypred <- c(complayers,mlplayers,syplayers)
  pitrook23<-uniroster%>%
    filter(!Name%in%alreadypred)%>%
    filter(position == "SP"|position=="RP")%>%
    mutate(teamID = ifelse(Team=="KC","KCR",Team))%>%
    mutate(teamID = ifelse(teamID=="SD","SDP",teamID))%>%
    mutate(teamID = ifelse(teamID=="SF","SFG",teamID))%>%
    mutate(teamID = ifelse(teamID=="TB","TBR",teamID))%>%
    select(Name, teamID)%>%
    mutate(Exp =factor(1))%>%
    mutate(carval = 0)%>%
    mutate(Era = factor(7))
  #get models
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
  PitMLMod<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag2G+lag2Val,data = dfmissedlast)
  dfrook <- df %>%
    select(-HOF)%>%
    filter(is.na(lag1Val)==TRUE&is.na(lag2Val)==TRUE)%>%
    filter(is.na(teamID)==FALSE)%>%
    mutate(carval = ifelse(is.na(carval)==TRUE,0,carval))
  PitRookMod<-lm(FloValue~factor(Exp)*carval+teamID+Era,data = dfrook)
  dfsecyr <- df  %>%
    select(-HOF)%>%
    filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==TRUE)
  PitSYMod<- lm(FloValue~factor(Exp)*carval+teamID+Era+lag1G+lag1Val,data = dfsecyr)
  df<- df %>%
    select(-HOF)%>%
    filter(is.na(lag1Val)==FALSE&is.na(lag2Val)==FALSE)%>%
    mutate(change = lag1Val-lag2Val)
  dfmod<-df%>%
    mutate(Age = factor(yearID-birthYear))%>%
    select(Name,playerID,Era,yearID,FloValue,lag1Val,lag2Val,carval,Exp,lag1G,lag2G,teamID,change)%>%
    na.omit()
  PitComMod<- lm(FloValue~Era+lag1Val+lag2Val+carval*factor(Exp)+lag1Val*lag1G+lag2Val*lag2G+teamID+change*factor(Exp), data = dfmod)
  #predict
  predcom<- predict(PitComMod, newdata=pitcom23)
  pitcom23<- pitcom23%>%
    mutate(PredVal = predcom)%>%
    select(Name,teamID,PredVal,Exp)%>%
    mutate(Exp = as.numeric(Exp)+2)
  predml<- predict(PitMLMod, newdata=pitmissedlast23)
  pitmissedlast23<- pitmissedlast23%>%
    mutate(PredVal = predml)%>%
    select(Name,teamID,PredVal,Exp)%>%
    mutate(Exp = as.numeric(Exp)+1)
  predsy<- predict(PitSYMod, newdata=pitsecondyear23)
  pitsecondyear23<- pitsecondyear23%>%
    mutate(PredVal = predsy)%>%
    select(Name,teamID,PredVal,Exp)%>%
    mutate(Exp = as.numeric(Exp)+1)
  predrook<- predict(PitRookMod, newdata=pitrook23)
  pitrook23<- pitrook23%>%
    mutate(PredVal = predrook)%>%
    select(Name,teamID,PredVal,Exp)%>%
    mutate(Exp = as.numeric(Exp))
  predpitchers<- rbind(pitrook23,pitsecondyear23,pitmissedlast23,pitcom23)
  return(predpitchers)
}
predpit23 <- predpitchers(uniroster)%>%
  mutate(Exp = as.numeric(Exp))
teampit23<- predpit23%>%
  arrange(-PredVal)%>%
  group_by(teamID)%>%
  mutate(PV = sum(PredVal))%>%
  mutate(PExp = log(sum(as.numeric(Exp))))%>%
  slice(1)%>%
  ungroup()%>%
  select(teamID,PV,PExp)
####################################
#Team
####################################
teampred23<- left_join(teampit23,teamhit23, by = "teamID")%>%
  mutate(teamID = ifelse(teamID=="WSH","WSN",teamID))
Teams<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLBTeamsAT")
lastyear <- Teams %>% 
  filter(yearID == 2022)%>%
  mutate(lag1wpct = Wpct)%>%
  mutate(lag1team = FloStrength)%>%
  select(teamID, lag1team,lag1wpct)
twoyear <- Teams %>% 
  filter(yearID == 2021)%>%
  mutate(lag2wpct = Wpct)%>%
  mutate(lag2team = FloStrength)%>%
  select(teamID, lag2team,lag2wpct)
teampred23<- left_join(teampred23, lastyear, by ="teamID")
teampred23<- left_join(teampred23, twoyear, by ="teamID")%>%
  mutate(Era = factor(7))
moddf<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthApp/mlbpredmod")
teams<- teampred23$teamID
lin<-lm(Wpct~(HV+PV+lag1team+lag2team+lag1wpct+lag2wpct+HExp+PExp)*factor(Era),data = moddf)
pred23 <- data.frame(predict(lin, newdata = teampred23, interval= "prediction"))%>%
  mutate(teamID =teams)%>%
  mutate(PredWpct= fit-(mean(fit)-.5))%>%
  mutate(lb= lwr-(mean(fit)-.5))%>%
  mutate(ub= upr-(mean(fit)-.5))%>%
  select(teamID,PredWpct,lb,ub)
overunder<- c(74.5,96.5,76.5,78.5,76.5,83.5,65.5,86.5,65.5,68.5,97.5,68.5,81.5,96.5,75.5,86.5,84.5,94.5,94.5,59.5,89.5,67.5,93.5,86.5,81.5,88.5,89.5,81.5,90.5,59.5)
teampred23<- left_join(teampred23,pred23, by = "teamID") %>%
  mutate(ou = overunder)%>%
  mutate(PredW = PredWpct*162)

write_csv(teampred23, "/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBteampred23")
write_csv(predhit23, "/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBhitpred23")
write_csv(predpit23, "/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBpitpred23")

                       
                       
                       