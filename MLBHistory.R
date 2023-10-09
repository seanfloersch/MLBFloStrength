library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
library(readxl)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console



##############################################################################################################################
########Teams
##############################################################################################################################
#get 2023
h <- read_html("https://www.baseball-reference.com/leagues/majors/2023.shtml#all_teams_standard_batting") 
stats <- html_nodes(h, "#teams_standard_batting thead+ tbody .left , #teams_standard_batting tbody .right") %>% html_text
hitdf<- as.data.frame(matrix(NA, nrow =31, ncol=29))
for (i in c(1:31)) {
  for (j in c(1:29)){
    hitdf[i,j] = stats[29 * (i-1)+j]
  }
}
hitdf <- hitdf[1:30,]
colnames(hitdf) <- html_nodes(h, "#teams_standard_batting .poptip") %>% html_text
hitdf<- hitdf %>%
  mutate(yearID = 2023) %>%
  select("teamID"= "Tm",yearID, G,R,AB,H, "X2B"= "2B", "X3B"= "3B",HR,BB,SO,SB,CS,HBP)
pitchdf <- read_excel("~/Downloads/MLBPitchingTeam23.xls") %>%
  mutate(yearID = 2023) %>%
  mutate(IPouts = round(IP *3)) %>%
  select("teamID"=Tm, yearID, W, "RA"="R", ER,ERA, CG, SHO= "cSho", SV,IPouts, "HA"="H", "HRA"="HR", "BBA"="BB", "SOA"="SO", DP, "FP"="Fld%")
df <- left_join(hitdf, pitchdf, by = c("teamID", "yearID"))
for ( i in c(2:28)){
  df[,i]= as.numeric(df[,i])
}
df<- df  %>%
  mutate(Hitting = (4*HR+3*X3B+2*X2B+(H-HR-X2B-X3B)-2*SO+BB+SB-CS)/IPouts)%>%
  mutate(Pitching = -1*(4*HRA+1.5*(HA-HRA)-2*SOA+BBA)/IPouts) %>%
  mutate(Runs = (R-RA)/G) %>%
  group_by(yearID) %>%
  mutate(Hitting = (Hitting - mean(Hitting))/sd(Hitting)) %>%
  mutate(Pitching = (Pitching - mean(Pitching))/sd(Pitching)) %>%
  mutate(Fielding = (FP - mean(FP))/sd(FP)) %>%
  ungroup() %>%
  mutate(WSWin = ifelse(teamID=="Houston Astros",0,0))%>%
  mutate(LgWin = ifelse(teamID=="Houston Astros"|teamID== "Philadelphia Phillies",0,0)) %>%
  mutate(Wpct = W /G) %>%
  mutate(Era = as.factor(7))
Teams <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Teams.csv") %>%
  filter(yearID > 2010) %>%
  group_by(name) %>%
  slice(1) %>% 
  ungroup() %>%
  select("teamID"=name, franchID,teamIDBR) %>%
  mutate(teamID =ifelse(teamID == "Los Angeles Angels of Anaheim","Los Angeles Angels", teamID))%>%
  mutate(teamID =ifelse(teamID == "Cleveland Indians","Cleveland Guardians", teamID))
df23 <- left_join(Teams, df, by = "teamID") %>% 
  na.omit() %>%
  select(-teamID)
#get 2022
h <- read_html("https://www.baseball-reference.com/leagues/majors/2022.shtml#all_teams_standard_batting") 
  stats <- html_nodes(h, "#teams_standard_batting thead+ tbody .left , #teams_standard_batting tbody .right") %>% html_text
  hitdf<- as.data.frame(matrix(NA, nrow =31, ncol=29))
  for (i in c(1:31)) {
    for (j in c(1:29)){
      hitdf[i,j] = stats[29 * (i-1)+j]
    }
  }
  hitdf <- hitdf[1:30,]
  colnames(hitdf) <- html_nodes(h, "#teams_standard_batting .poptip") %>% html_text
  hitdf<- hitdf %>%
    mutate(yearID = 2022) %>%
    select("teamID"= "Tm",yearID, G,R,AB,H, "X2B"= "2B", "X3B"= "3B",HR,BB,SO,SB,CS,HBP)
  pitchdf <- read_excel("~/Downloads/pitching2022team.xls") %>%
    mutate(yearID = 2022) %>%
    mutate(IPouts = round(IP *3)) %>%
    select("teamID"=Tm, yearID, W, "RA"="R", ER,ERA, CG, SHO= "cSho", SV,IPouts, "HA"="H", "HRA"="HR", "BBA"="BB", "SOA"="SO", DP, "FP"="Fld%")
  df <- left_join(hitdf, pitchdf, by = c("teamID", "yearID"))
  for ( i in c(2:28)){
    df[,i]= as.numeric(df[,i])
  }
  df<- df  %>%
    mutate(Hitting = (4*HR+3*X3B+2*X2B+(H-HR-X2B-X3B)-2*SO+BB+SB-CS)/IPouts)%>%
    mutate(Pitching = -1*(4*HRA+1.5*(HA-HRA)-2*SOA+BBA)/IPouts) %>%
    mutate(Runs = (R-RA)/G) %>%
    group_by(yearID) %>%
    mutate(Hitting = (Hitting - mean(Hitting))/sd(Hitting)) %>%
    mutate(Pitching = (Pitching - mean(Pitching))/sd(Pitching)) %>%
    mutate(Fielding = (FP - mean(FP))/sd(FP)) %>%
    ungroup() %>%
    mutate(WSWin = ifelse(teamID=="Houston Astros",1,0))%>%
    mutate(LgWin = ifelse(teamID=="Houston Astros"|teamID== "Philadelphia Phillies",1,0)) %>%
    mutate(Wpct = W /G) %>%
    mutate(Era = as.factor(7))
Teams <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Teams.csv") %>%
  filter(yearID > 2010) %>%
  group_by(name) %>%
  slice(1) %>% 
  ungroup() %>%
  select("teamID"=name, franchID,teamIDBR) %>%
  mutate(teamID =ifelse(teamID == "Los Angeles Angels of Anaheim","Los Angeles Angels", teamID))%>%
  mutate(teamID =ifelse(teamID == "Cleveland Indians","Cleveland Guardians", teamID))
df22 <- left_join(Teams, df, by = "teamID") %>% 
  na.omit() %>%
  select(-teamID)
df<-rbind(df23,df22)
#get all teams
Teams <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Teams.csv") %>%
  filter(yearID>1902) %>%
  filter(yearID != 1911&yearID !=1912) %>%
  mutate(Wpct = W/G) %>%
  mutate(Era = ifelse(yearID<1920,1,NA))%>%
  mutate(Era = ifelse((yearID<1942&is.na(Era)==TRUE),2,Era))%>%
  mutate(Era = ifelse((yearID<1961&is.na(Era)==TRUE),3,Era))%>%
  mutate(Era = ifelse((yearID<1977&is.na(Era)==TRUE),4,Era))%>%
  mutate(Era = ifelse((yearID<1994&is.na(Era)==TRUE),5,Era))%>%
  mutate(Era = ifelse((yearID<2006&is.na(Era)==TRUE),6,Era))%>%
  mutate(Era = ifelse((is.na(Era)==TRUE),7,Era)) %>%
  mutate(Era = as.factor(Era)) %>%
  select(-teamID,-Rank,-Ghome,-teamID,-name,-park,-teamIDlahman45,-teamIDretro,-BPF,-PPF,-SF,-L, -divID,-lgID,-DivWin,-WCWin,-attendance,-E) %>%
  mutate(CS = ifelse(is.na(CS)==TRUE,0,CS)) %>%
  mutate(Hitting = (4*HR+3*X3B+2*X2B+(H-HR-X2B-X3B)-2*SO+BB+SB-CS)/IPouts)%>%
  mutate(Pitching = -1*(4*HRA+1.5*(HA-HRA)-2*SOA+BBA)/IPouts) %>%
  mutate(Runs = (R-RA)/G) %>%
  group_by(yearID) %>%
  mutate(Hitting = (Hitting - mean(Hitting))/sd(Hitting)) %>%
  mutate(Pitching = (Pitching - mean(Pitching))/sd(Pitching)) %>%
  mutate(Fielding = (FP - mean(FP))/sd(FP)) %>%
  ungroup() %>%
  mutate(WSWin = ifelse(WSWin=="Y",1,0))%>%
  mutate(LgWin = ifelse(LgWin=="Y",1,0))
#combine
Teams<- rbind(Teams, df)
#find FS
linmod <- lm(Wpct~(Hitting+Pitching+Fielding)*Era, data = Teams)
summary(linmod)
Teams<- Teams %>%
  mutate(FloStrength = predict(linmod)) %>%
  rename("FSTeam"= "FloStrength","teamID"=teamIDBR)
##############################################################################################################################
########Players
##############################################################################################################################

#get 2023
batting2023 <- read_excel("~/Downloads/batting2023.xlsx") %>%
  select(Name,"playerID"="Name-additional","teamID"="Tm", "lgID"="Lg",G,AB,R,H,"X2B"="2B","X3B"="3B",HR,RBI,SB,CS,BB,SO,IBB,SH,SF,HBP,"GIDP"="GDP",PA)%>%
  mutate(Name = str_remove(Name, "\\*")) %>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  mutate(Name = str_remove(Name, "\\#")) %>%
  mutate(Power =((4* HR+ 3 * X3B + 2 * X2B + (H - X2B - X3B - HR)-1.2*SO+BB+IBB)/(PA))) %>%
  group_by(Name)%>%
  arrange(-G)%>%
  slice(1)%>%
  ungroup()%>%
  na.omit()%>%
  mutate(PAPG = (PA)/G) %>%
  filter(PAPG >2 & G>30)%>%
  mutate(yearID = 2023) %>%
  group_by(teamID,yearID) %>%
  mutate(RPG = R / PA) %>%
  mutate(RBIPG = RBI / PA) %>%
  mutate(normRun = (RPG -mean(RPG))/ sd(RPG)) %>%
  mutate(normRBI = (RBIPG -mean(RBIPG))/ sd(RBIPG)) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(SBV = (SB-2*CS+50)^.125) %>%
  mutate(SBV = ((SBV - mean(SBV))/sd(SBV))/2) %>%
  mutate(Power = (Power - mean(Power))/sd(Power)) %>%
  mutate(Other = normRun +normRBI +.3*SBV) %>%
  mutate(Other = (Other - mean(Other))/sd(Other)) %>%
  ungroup() %>%
  mutate(OffFS = (.354*Other +.646*Power)) %>%
  mutate(OffValue = OffFS * (PA/164))

fielding2023 <- read_excel("~/Downloads/fielding2023.xlsx")%>%
  mutate(yearID = 2023) %>%
  mutate(InnOuts = round(Inn * 3)) %>%
  select(-Inn) %>%
  mutate(POS=substr(POS,1,2))%>%
  filter(POS != "P"& InnOuts>=max(InnOuts)*.3) %>%
  mutate(POS = ifelse(POS %in% c("2B","SS","3B"), "IF", POS)) %>%
  mutate(Fielding = ifelse(POS =="OF",((PO+3*A-4*E)/(InnOuts/27)),NA))%>%
  mutate(Fielding = ifelse(POS =="IF"&is.na(Fielding)==TRUE,((PO+2*A-2*E)/(InnOuts/27)),Fielding))%>%
  mutate(Fielding = ifelse(POS =="1B"&is.na(Fielding)==TRUE,((PO+3*A-3*E)/(InnOuts/27)),Fielding))%>%
  mutate(Fielding = ifelse(POS =="C",((PO+15*(A)-3*E)/(InnOuts/27)),Fielding)) %>%
  group_by(yearID, POS) %>%
  mutate(Fielding = (Fielding-mean(Fielding))/sd(Fielding)) %>%
  ungroup() %>%
  group_by(Name, yearID) %>%
  mutate(Fielding = sum(Fielding*(InnOuts)/sum(InnOuts))) %>%
  mutate(PO = sum(PO))%>%
  mutate(A = sum(A))%>%
  mutate(E = sum(E))%>%
  mutate(Fpct = (PO+A)/(PO+A+E)) %>%
  arrange(-InnOuts) %>%
  mutate(Innings = sum(InnOuts)/3) %>%
  slice(1)%>%
  ungroup() %>%
  select(Name, "teamID"="Tm",yearID,POS, Fielding, Fpct,Innings) %>%
  mutate(FValue = Fielding * (Innings/4000))
bat23 <- left_join(batting2023, fielding2023, by = c("Name")) %>%
  rename("teamID" = "teamID.x", "yearID"= "yearID.x") %>%
  mutate(POS = ifelse(is.na(POS)==TRUE,"P/DH",POS)) %>%
  select(-yearID.y,-teamID.y)%>%
  mutate(FValue = ifelse(is.na(FValue)==TRUE,0,FValue)) %>%
  mutate(Fielding = ifelse(is.na(Fielding)==TRUE,0,Fielding)) %>%
  mutate(Innings = ifelse(is.na(Innings)==TRUE,0.01,Innings)) %>%
  mutate(Fpct = ifelse(is.na(Fpct)==TRUE,0,Fpct))%>%
  mutate(DefFS = Fielding) %>%
  mutate(DefValue = FValue) %>%
  mutate(AVG = H/AB) %>%
  mutate(SLG = (4* HR+ 3 * X3B + 2 * X2B + (H - X2B - X3B - HR))/AB) %>%
  mutate(OBP = (BB+IBB+H)/(AB+BB+IBB)) %>%
  mutate(FloValue = OffValue +DefValue) %>%
  mutate(AVG = round(AVG, 3)) %>%
  mutate(SLG = round(SLG, 3))  %>%
  mutate(OBP = round(OBP, 3))  %>%
  mutate(Fpct = round(Fpct, 3))  %>%
  mutate(OffFS = round(OffFS, 3))  %>%
  mutate(DefFS = round(DefFS, 3))  %>%
  mutate(OffValue = round(OffValue, 3))  %>%
  mutate(DefValue = round(DefValue, 3)) %>%
  mutate(FloValue = round(FloValue, 3)) %>%
  mutate(birthYear = NA)
pitching2023 <- read_excel("~/Downloads/pitching2023.xlsx") %>%
  mutate(IPouts = round(IP*3,1))%>%
  mutate(yearID = 2023)%>%
  filter(teamID!="TOT")%>%
  group_by(playerID,yearID)%>%
  mutate(teamID = last(teamID))%>%
  mutate(W = sum(W))%>%
  mutate(L = sum(L))%>%
  mutate(G = sum(G))%>%
  mutate(GS = sum(GS))%>%
  mutate(CG= sum(CG))%>%
  mutate(SHO= sum(SHO))%>%
  mutate(SV = sum(SV))%>%
  mutate(IPouts = sum(IPouts))%>%
  mutate(H = sum(H))%>%
  mutate(ER = sum(ER))%>%
  mutate(HR = sum(HR))%>%
  mutate(BB = sum(BB))%>%
  mutate(SO = sum(SO))%>%
  mutate(IBB = sum(IBB))%>%
  mutate(WP = sum(WP))%>%
  mutate(HBP = sum(HBP))%>%
  mutate(IP = round(IPouts/3,2))%>%
  mutate(ERA = ER*9/IP)%>%
  slice(1)%>%
  ungroup()%>%
  filter(yearID>1902&IP>20) %>%
  mutate(Pos =ifelse(GS>.6*G,"SP","RP"))%>%
  mutate(FloStrength =-1*(4*HR+1.5*(H-HR)-2*SO+BB)/IPouts) %>%
  group_by(yearID,Pos) %>%
  mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength)) %>%
  ungroup() %>%
  mutate(FloValue = FloStrength * (IP/87)) %>%
  select(Name,teamID,"lgID"="Lg",W,L,ERA,G,GS,CG,SHO,SV,IP,H,R,ER,HR,BB,IBB,SO,HBP,WP,playerID,IPouts,yearID,Pos,FloStrength,FloValue)
#get 2022
batting2022 <- read_excel("~/Downloads/batting2022.xls") %>%
  select(Name,"playerID"="Name-additional","teamID"="Tm", "lgID"="Lg",G,AB,R,H,"X2B"="2B","X3B"="3B",HR,RBI,SB,CS,BB,SO,IBB,SH,SF,HBP,"GIDP"="GDP",PA)%>%
  mutate(Name = str_remove(Name, "\\*")) %>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  mutate(Name = str_remove(Name, "\\#")) %>%
  mutate(Power =((4* HR+ 3 * X3B + 2 * X2B + (H - X2B - X3B - HR)-1.2*SO+BB+IBB)/(PA))) %>%
  na.omit()%>%
  mutate(PAPG = (PA)/G) %>%
  filter(PAPG >2 & G>30)%>%
  mutate(yearID = 2022) %>%
  group_by(teamID,yearID) %>%
  mutate(RPG = R / PA) %>%
  mutate(RBIPG = RBI / PA) %>%
  mutate(normRun = (RPG -mean(RPG))/ sd(RPG)) %>%
  mutate(normRBI = (RBIPG -mean(RBIPG))/ sd(RBIPG)) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(SBV = (SB-2*CS+50)^.125) %>%
  mutate(SBV = ((SBV - mean(SBV))/sd(SBV))/2) %>%
  mutate(Power = (Power - mean(Power))/sd(Power)) %>%
  mutate(Other = normRun +normRBI +.3*SBV) %>%
  mutate(Other = (Other - mean(Other))/sd(Other)) %>%
  ungroup() %>%
  mutate(OffFS = (.354*Other +.646*Power)) %>%
  mutate(OffValue = OffFS * (PA/164))

fielding2022 <- read_excel("~/Downloads/fielding2022.xls")%>%
  mutate(yearID = 2022) %>%
  mutate(InnOuts = round(Inn * 3)) %>%
  select(-Inn) %>%
  filter(POS != "P"& InnOuts>270) %>%
  mutate(POS = ifelse(POS %in% c("2B","SS","3B"), "IF", POS)) %>%
  mutate(SB = ifelse(is.na(SB)==TRUE,0,SB)) %>%
  mutate(CS = ifelse(is.na(CS)==TRUE,0,CS)) %>%
  mutate(Fielding = ifelse(POS =="OF",((PO+3*A-4*E)/(InnOuts/27)),NA))%>%
  mutate(Fielding = ifelse(POS =="IF"&is.na(Fielding)==TRUE,((PO+2*A-2*E)/(InnOuts/27)),Fielding))%>%
  mutate(Fielding = ifelse(POS =="1B"&is.na(Fielding)==TRUE,((PO+3*A-3*E)/(InnOuts/27)),Fielding))%>%
  mutate(Fielding = ifelse(POS =="C",((PO+15*(A)-3*E)/(InnOuts/27)),Fielding)) %>%
  group_by(yearID, POS) %>%
  mutate(Fielding = (Fielding-mean(Fielding))/sd(Fielding)) %>%
  ungroup() %>%
  group_by(Name, yearID) %>%
  mutate(Fielding = sum(Fielding*(InnOuts)/sum(InnOuts))) %>%
  mutate(PO = sum(PO))%>%
  mutate(A = sum(A))%>%
  mutate(E = sum(E))%>%
  mutate(Fpct = (PO+A)/(PO+A+E)) %>%
  arrange(-InnOuts) %>%
  mutate(Innings = sum(InnOuts)/3) %>%
  slice(1)%>%
  ungroup() %>%
  select(Name, teamID,yearID,POS, Fielding, Fpct,Innings) %>%
  mutate(FValue = Fielding * (Innings/4000))
bat22 <- left_join(batting2022, fielding2022, by = c("Name")) %>%
  rename("teamID" = "teamID.x", "yearID"= "yearID.x") %>%
  mutate(POS = ifelse(is.na(POS)==TRUE,"P/DH",POS)) %>%
  select(-yearID.y,-teamID.y)%>%
  mutate(FValue = ifelse(is.na(FValue)==TRUE,0,FValue)) %>%
  mutate(Fielding = ifelse(is.na(Fielding)==TRUE,0,Fielding)) %>%
  mutate(Innings = ifelse(is.na(Innings)==TRUE,0.01,Innings)) %>%
  mutate(Fpct = ifelse(is.na(Fpct)==TRUE,0,Fpct))%>%
  mutate(DefFS = Fielding) %>%
  mutate(DefValue = FValue) %>%
  mutate(AVG = H/AB) %>%
  mutate(SLG = (4* HR+ 3 * X3B + 2 * X2B + (H - X2B - X3B - HR))/AB) %>%
  mutate(OBP = (BB+IBB+H)/(AB+BB+IBB)) %>%
  mutate(FloValue = OffValue +DefValue) %>%
  mutate(AVG = round(AVG, 3)) %>%
  mutate(SLG = round(SLG, 3))  %>%
  mutate(OBP = round(OBP, 3))  %>%
  mutate(Fpct = round(Fpct, 3))  %>%
  mutate(OffFS = round(OffFS, 3))  %>%
  mutate(DefFS = round(DefFS, 3))  %>%
  mutate(OffValue = round(OffValue, 3))  %>%
  mutate(DefValue = round(DefValue, 3)) %>%
  mutate(FloValue = round(FloValue, 3)) %>%
  mutate(birthYear = NA)
pitching2022 <- read_excel("~/Downloads/pitching2022.xls") %>%
  mutate(IPouts = round(IP*3,1))%>%
  mutate(yearID = 2022)%>%
  group_by(playerID,yearID)%>%
  mutate(teamID = last(teamID))%>%
  mutate(W = sum(W))%>%
  mutate(L = sum(L))%>%
  mutate(G = sum(G))%>%
  mutate(GS = sum(GS))%>%
  mutate(CG= sum(CG))%>%
  mutate(SHO= sum(SHO))%>%
  mutate(SV = sum(SV))%>%
  mutate(IPouts = sum(IPouts))%>%
  mutate(H = sum(H))%>%
  mutate(ER = sum(ER))%>%
  mutate(HR = sum(HR))%>%
  mutate(BB = sum(BB))%>%
  mutate(SO = sum(SO))%>%
  mutate(IBB = sum(IBB))%>%
  mutate(WP = sum(WP))%>%
  mutate(HBP = sum(HBP))%>%
  mutate(IP = round(IPouts/3,2))%>%
  mutate(ERA = ER*9/IP)%>%
  slice(1)%>%
  ungroup()%>%
  filter(yearID>1902&IP>20) %>%
  mutate(Pos =ifelse(GS>.6*G,"SP","RP"))%>%
  mutate(FloStrength =-1*(4*HR+1.5*(H-HR)-2*SO+BB)/IPouts) %>%
  group_by(yearID,Pos) %>%
  mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength)) %>%
  ungroup() %>%
  mutate(FloValue = FloStrength * (IP/87)) %>%
  na.omit()
  

#get all time
Fielding <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Fielding.csv") %>%
  mutate(InnOuts = ifelse(is.na(InnOuts)==TRUE, G*28,InnOuts))
Batting <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Batting.csv")
Pitching <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Pitching.csv")
People <- read.csv("~/Downloads/baseballdatabank-2022.2/core/People.csv")
HallOfFame <- read.csv("~/Downloads/baseballdatabank-2022.2/contrib/HallOfFame.csv") %>%
  filter(inducted == "Y"&category=="Player") %>%
  select(playerID, "HOF"=inducted)
People<-left_join(People,HallOfFame, by = "playerID") %>%
  mutate(Name = str_c(nameFirst,nameLast, sep = " ")) %>%
  select(-playerID) %>%
  select("playerID"= "bbrefID", Name,HOF)
Positions <- Fielding %>%
  select(yearID, playerID, POS)
# get playerID as bbrefID
changeID <- read.csv("~/Downloads/baseballdatabank-2022.2/core/People.csv") %>%
  select(playerID,bbrefID,birthYear)
changeteamID <- read.csv("~/Downloads/baseballdatabank-2022.2/core/Teams.csv") %>%
  select(teamID, teamIDBR) %>%
  unique()

#Fielding
FieldSzn<- left_join(Fielding, changeID,by = "playerID") %>%
  select(-playerID) %>%
  rename("playerID"= "bbrefID") 
FieldSzn <- left_join(FieldSzn, changeteamID, by = "teamID")%>%
  select(-teamID) %>%
  rename("teamID"= "teamIDBR")
FieldSzn <- FieldSzn %>%
  filter((teamID == "WHS"& yearID <2005)| teamID != "WHS")%>% 
  filter((teamID == "TBD"& yearID <2008)| teamID != "TBD")%>% 
  filter((teamID == "TBR"& yearID >2007)| teamID != "TBR")%>%
  filter(yearID>1902) %>%
  filter(POS != "P"& InnOuts>270) %>%
  mutate(POS = ifelse(POS %in% c("2B","SS","3B"), "IF", POS)) %>%
  mutate(SB = ifelse(is.na(SB)==TRUE,0,SB)) %>%
  mutate(CS = ifelse(is.na(CS)==TRUE,0,CS)) %>%  
  mutate(Fielding = ifelse(POS =="OF",((PO+3*A-4*E)/(InnOuts/27)),NA))%>%
  mutate(Fielding = ifelse(POS =="IF"&is.na(Fielding)==TRUE,((PO+2*A-2*E)/(InnOuts/27)),Fielding))%>%
  mutate(Fielding = ifelse(POS =="1B"&is.na(Fielding)==TRUE,((PO+3*A-3*E)/(InnOuts/27)),Fielding))%>%
  mutate(Fielding = ifelse(POS =="C",((PO+15*(A)-3*E)/(InnOuts/27)),Fielding)) %>%
  group_by(yearID, POS) %>%
  mutate(Fielding = (Fielding-mean(Fielding))/sd(Fielding)) %>%
  ungroup() %>%
  group_by(playerID, yearID) %>%
  mutate(Fielding = sum(Fielding*(InnOuts)/sum(InnOuts))) %>%
  mutate(PO = sum(PO))%>%
  mutate(A = sum(A))%>%
  mutate(E = sum(E))%>%
  mutate(Fpct = (PO+A)/(PO+A+E)) %>%
  arrange(-InnOuts) %>%
  mutate(Innings = sum(InnOuts)/3) %>%
  slice(1)%>%
  ungroup() %>%
  select(playerID, teamID,yearID,POS, Fielding, Fpct,Innings) %>%
  mutate(FValue = Fielding * (Innings/4000))

#Hitting
BatSzn<- left_join(Batting, changeID,by = "playerID") %>%
  select(-playerID) %>%
  rename("playerID"= "bbrefID")
BatSzn <- left_join(BatSzn, changeteamID, by = "teamID")%>%
  select(-teamID) %>%
  rename("teamID"= "teamIDBR")
BatSzn <- BatSzn %>%
  filter((teamID == "WHS"& yearID <2005)| teamID != "WHS")%>% 
  filter((teamID == "TBD"& yearID <2008)| teamID != "TBD")%>% 
  filter((teamID == "TBR"& yearID >2007)| teamID != "TBR")
BatSzn <- left_join(BatSzn,FieldSzn, by = c("yearID","playerID")) %>%
  mutate(POS = ifelse(is.na(POS)==TRUE,"P/DH",POS)) %>%
  mutate(teamID.y = ifelse(is.na(teamID.y)==TRUE,teamID.x,teamID.y)) %>%
  mutate(FValue = ifelse(is.na(FValue)==TRUE,0,FValue)) %>%
  mutate(Fielding = ifelse(is.na(Fielding)==TRUE,0,Fielding)) %>%
  mutate(Innings = ifelse(is.na(Innings)==TRUE,0.01,Innings)) %>%
  mutate(Fpct = ifelse(is.na(Fpct)==TRUE,0,Fpct)) %>%
  filter(yearID >1902) %>%
  group_by(playerID, yearID) %>%
  mutate(teamID = last(teamID.x))%>%
  mutate(G = sum(G))%>%
  mutate(AB = sum(AB))%>%
  mutate(R = sum(R))%>%
  mutate(H = sum(H))%>%
  mutate(X2B= sum(X2B))%>%
  mutate(X3B= sum(X3B))%>%
  mutate(HR = sum(HR))%>%
  mutate(RBI = sum(RBI))%>%
  mutate(SB = sum(SB))%>%
  mutate(CS = sum(CS))%>%
  mutate(BB = sum(BB))%>%
  mutate(SO = sum(SO))%>%
  mutate(IBB = sum(IBB))%>%
  mutate(HBP = sum(HBP))%>%
  mutate(SH = sum(SH))%>%
  mutate(SF = sum(SF))%>%
  mutate(GIDP = sum(GIDP))%>%
  arrange(-G) %>%
  slice(1) %>%
  ungroup()%>%
  filter((POS == "P/DH" &(AB>140))|(POS != "P/DH" &(AB>90&G>30)))%>%
  mutate(IBB = ifelse(is.na(IBB)==TRUE,0,IBB))%>%
  mutate(CS = ifelse(is.na(CS)==TRUE,0,CS))%>%
  mutate(HBP = ifelse(is.na(HBP)==TRUE,0,HBP))%>%
  mutate(SO = ifelse(is.na(SO)==TRUE,0,SO))%>%
  mutate(PA = AB+BB+IBB+HBP) %>%
  mutate(Power =((4* HR+ 3 * X3B + 2 * X2B + (H - X2B - X3B - HR)-1.2*SO+BB+IBB)/(PA))) %>%
  mutate(PAPG = (PA)/G) %>%
  filter(PAPG >2)%>%
  group_by(teamID.x,yearID) %>%
  mutate(RPG = R / PA) %>%
  mutate(RBIPG = RBI / PA) %>%
  mutate(normRun = (RPG -mean(RPG))/ sd(RPG)) %>%
  mutate(normRBI = (RBIPG -mean(RBIPG))/ sd(RBIPG)) %>%
  ungroup %>%
  group_by(yearID) %>%
  mutate(SBV = (SB-2*CS+50)^.125) %>%
  mutate(SBV = ((SBV - mean(SBV))/sd(SBV))/2) %>%
  mutate(Power = (Power - mean(Power))/sd(Power)) %>%
  mutate(Other = normRun +normRBI +.3*SBV) %>%
  mutate(Other = (Other - mean(Other))/sd(Other)) %>%
  ungroup() %>%
  mutate(OffFS = (.354*Other +.646*Power)) %>%
  mutate(OffValue = OffFS * (PA/164)) %>%
  mutate(DefFS = Fielding) %>%
  mutate(DefValue = FValue) %>%
  mutate(AVG = H/AB) %>%
  mutate(SLG = (4* HR+ 3 * X3B + 2 * X2B + (H - X2B - X3B - HR))/AB) %>%
  mutate(OBP = (BB+IBB+H)/(AB+BB+IBB)) %>%
  mutate(FloValue = OffValue +DefValue) %>%
  mutate(AVG = round(AVG, 3)) %>%
  mutate(SLG = round(SLG, 3))  %>%
  mutate(OBP = round(OBP, 3))  %>%
  mutate(Fpct = round(Fpct, 3))  %>%
  mutate(OffFS = round(OffFS, 3))  %>%
  mutate(DefFS = round(DefFS, 3))  %>%
  mutate(OffValue = round(OffValue, 3))  %>%
  mutate(DefValue = round(DefValue, 3)) %>%
  mutate(FloValue = round(FloValue, 3)) %>%
  select(-teamID.y, -stint,-teamID.x)
# add 2022 to complete DF
BatSzn <- left_join(BatSzn,People, by ="playerID")
bat22<-rbind(bat22,bat23)
bat22 <- bat22 %>%
  mutate(HOF =NA)
BatSzn <- rbind(BatSzn, bat22)

#Pitching
PitchSzn<- left_join(Pitching, changeID,by = "playerID") %>%
  select(-playerID) %>%
  rename("playerID"= "bbrefID")
PitchSzn <- left_join(PitchSzn, changeteamID, by = "teamID")%>%
  select(-teamID) %>%
  rename("teamID"= "teamIDBR")
PitchSzn <- PitchSzn %>%
  filter((teamID == "WHS"& yearID <2005)| teamID != "WHS")%>% 
  filter((teamID == "TBD"& yearID <2008)| teamID != "TBD")%>% 
  filter((teamID == "TBR"& yearID >2007)| teamID != "TBR")
PitchSzn<-PitchSzn %>%
  group_by(playerID,yearID)%>%
  mutate(teamID = last(teamID))%>%
  mutate(W = sum(W))%>%
  mutate(L = sum(L))%>%
  mutate(G = sum(G))%>%
  mutate(GS = sum(GS))%>%
  mutate(CG= sum(CG))%>%
  mutate(SHO= sum(SHO))%>%
  mutate(SV = sum(SV))%>%
  mutate(IPouts = sum(IPouts))%>%
  mutate(H = sum(H))%>%
  mutate(ER = sum(ER))%>%
  mutate(HR = sum(HR))%>%
  mutate(BB = sum(BB))%>%
  mutate(SO = sum(SO))%>%
  mutate(IBB = sum(IBB))%>%
  mutate(WP = sum(WP))%>%
  mutate(HBP = sum(HBP))%>%
  mutate(IP = round(IPouts/3,2))%>%
  mutate(ERA = ER*9/IP)%>%
  slice(1)%>%
  ungroup()%>%
  filter(yearID>1902&IP>20) %>%
  mutate(FloStrength =-1*(4*HR+1.5*(H-HR)-2*SO+BB)/IPouts) %>%
  mutate(Pos=ifelse(GS>.6*G,"SP","RP"))%>%
  group_by(yearID,Pos) %>%
  mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength)) %>%
  ungroup() %>%
  mutate(FloValue = FloStrength * (IP/87)) %>%
  select(-BFP,-BK,-GF,-SH,-SF,-GIDP,-BAOpp,-stint)
PitchSzn <- left_join(PitchSzn,People, by ="playerID")
pitching2022<-rbind(pitching2022,pitching2023)
pitching2022<-pitching2022%>%
  mutate(HOF = NA)%>%
  mutate(birthYear=NA)
PitchSzn <- rbind(PitchSzn, pitching2022)



findval <- BatSzn %>%
  filter(teamID != "TOT") %>%
  group_by(teamID, yearID) %>%
  mutate(Value = sum(OffValue)) %>%
  slice(1) %>%
  ungroup()%>%
  select(teamID,yearID,Value)

findval1 <- PitchSzn %>%
  filter(teamID != "TOT") %>%
  group_by(teamID, yearID) %>%
  mutate(PValue = sum(FloValue)) %>%
  slice(1) %>%
  ungroup()%>%
  select(teamID,yearID, PValue)

findval2 <- BatSzn %>%
  filter(teamID != "TOT") %>%
  group_by(teamID, yearID) %>%
  mutate(FValue = sum(DefValue)) %>%
  slice(1) %>%
  ungroup()%>%
  select(teamID,yearID, FValue)

findval<- left_join(findval,findval1, by =c("teamID","yearID") )
findval<- left_join(findval,findval2, by =c("teamID","yearID") )
Teams<- left_join(Teams, findval, by = c("teamID","yearID")) %>%
  na.omit() %>%
  mutate(ComVal = PValue+Value+FValue -(G/16.2)) %>%
  mutate(FSPlay = (ComVal +(G/2))/G)
linmod <- lm(W~FValue+Value+PValue, data = Teams)
mean(Teams$FSPlay)
BatSzn<- BatSzn %>%
  group_by(playerID)%>%
  mutate(Name = first(Name))%>%
  ungroup()
PitchSzn<- PitchSzn %>%
  group_by(playerID)%>%
  mutate(Name = first(Name))%>%
  ungroup()
BatCar <- BatSzn %>% 
  group_by(playerID) %>%
  mutate(FloValue = sum(FloValue)) %>%
  mutate(OffValue = sum(OffValue)) %>%
  mutate(DefValue = sum(DefValue)) %>%
  mutate(OffFS = sum(OffFS*PA/sum(PA)))%>%
  mutate(DefFS = sum(DefFS*Innings/sum(Innings)))%>%
  mutate(G = sum(G)) %>%
  mutate(AB = sum(AB)) %>%
  mutate(H = sum(H)) %>%
  mutate(HR = sum(HR)) %>%
  mutate(R = sum(R)) %>%
  mutate(RBI = sum(RBI)) %>%
  mutate(BB = sum(BB)) %>%
  mutate(SB = sum(SB)) %>%
  mutate(IBB = sum(IBB)) %>%
  mutate(X2B = sum(X2B)) %>%
  mutate(X3B = sum(X3B)) %>%
  mutate(AVG = H /AB) %>%
  mutate(OBP = (BB+IBB+H)/(BB+IBB+AB)) %>%
  mutate(SLG = (2*X2B+3*X3B+4*HR+(H-HR-X2B-X3B))/(AB)) %>%
  mutate(Seasons = length(yearID)) %>%
  mutate(Current= ifelse(max(yearID)==2023,1,0))%>%
  slice(1) %>%
  ungroup() %>%
  filter(G>80) %>%
  mutate(AVG = round(AVG,3)) %>%
  mutate(OBP = round(OBP,3)) %>%
  mutate(SLG = round(SLG,3)) %>%
  mutate(OffFS = round(OffFS,3)) %>%
  mutate(DefFS = round(DefFS,3)) %>%
  select(playerID,yearID,Seasons,G,AB,H,HR,RBI,R,SB,AVG,OBP,SLG,OffFS,DefFS,OffValue, DefValue,FloValue,Current)
BatCar <- left_join(BatCar,People, by ="playerID")


PitchCar <- PitchSzn %>% 
  group_by(playerID) %>%
  mutate(FloStrength = sum(FloStrength*IP/sum(IP)))%>%
  mutate(FloValue = sum(FloValue)) %>%
  mutate(G = sum(G)) %>%
  mutate(GS = sum(GS)) %>%
  mutate(W = sum(W)) %>%
  mutate(L = sum(L)) %>%
  mutate(SV = sum(SV)) %>%
  mutate(H = sum(H)) %>%
  mutate(SO = sum(SO)) %>%
  mutate(BB = sum(BB)) %>%
  mutate(ER = sum(ER)) %>%
  mutate(IP = sum(IP)) %>%
  mutate(ERA = ER*9 /IP) %>%
  mutate(WHIP = (H+BB)/(IP)) %>%
  mutate(Seasons = length(yearID)) %>%
  mutate(Current= ifelse(max(yearID)==2023,1,0))%>%
  slice(1) %>%
  ungroup() %>%
  filter(IP>120 &G>30) %>%
  mutate(WHIP = round(WHIP,2)) %>%
  mutate(ERA = round(ERA,2)) %>%
  mutate(IP = round(IP,1)) %>%
  mutate(FloStrength = round(FloStrength,3)) %>%
  mutate(FloValue = round(FloValue,2)) %>%
  select(playerID,yearID,Seasons,G,GS,W,L,SV,SO,IP,WHIP,ERA,FloValue,FloStrength,Current)
PitchCar <- left_join(PitchCar,People, by ="playerID")

TopPlayers <- full_join(BatCar, PitchCar, by = "playerID") %>%
  mutate(FloValue.x = ifelse(is.na(FloValue.x)==TRUE,0,FloValue.x ))%>%
  mutate(FloValue.y = ifelse(is.na(FloValue.y)==TRUE,0,FloValue.y ))%>%
  mutate(HOF = ifelse(is.na(HOF.x)==TRUE,HOF.y,HOF.x ))%>%
  mutate(Name.x = ifelse(is.na(Name.x)==TRUE,Name.y,Name.x ))%>%
  mutate(Name.y = ifelse(is.na(Name.y)==TRUE,Name.x,Name.y ))%>%
  mutate(FloValue = FloValue.x+FloValue.y) %>%
  mutate(Current = ifelse(is.na(Current.x)==TRUE,Current.y,Current.x )) %>%
  select(Name = "Name.x", FloValue,HOF, Current)  

PitchSzn <- PitchSzn %>%
  mutate(FloStrength = round(FloStrength,3))%>%
  mutate(FloValue = round(FloValue,3))

Teams <- Teams %>%
  mutate(resid1 = FSTeam-Wpct)%>%
  mutate(resid2 = FSPlay-Wpct)
logmod<- glm(WSWin~(Value+PValue+FValue+Hitting+Pitching+Fielding+resid1+resid2)*Era, data = Teams, family = binomial)
Teams <- Teams %>%
  mutate(FloStrength =predict(logmod)) %>%
  group_by(yearID) %>%
  mutate(FloStrength = (FloStrength-mean(FloStrength))/ sd(FloStrength)) %>%
  ungroup() %>%
  mutate(FloStrength = (5+FloStrength)/10)

write_csv(Teams,"/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLB/MLBTeamsAT.csv")
write_csv(BatSzn,"/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLB/MLBBattersAT.csv")
write_csv(PitchSzn,"/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLB/MLBPitchersAT.csv")
write_csv(BatCar,"/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLB/MLBBattersCar.csv")
write_csv(PitchCar,"/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLB/MLBPitchersCar.csv")
write_csv(TopPlayers,"/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/MLB/MLBPlayersAT.csv")

