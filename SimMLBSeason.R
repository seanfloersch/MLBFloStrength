df1<- read.csv("/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")%>%
  mutate(UpdPred=PredWpct)%>%
  mutate(ub=UpdPred+.28)%>%
  mutate(lb =UpdPred-.28)
write_csv(df1,"/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")

### update df

df1<- read.csv("/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")%>%
  select(-G,-Wpct)
df2<-read.csv("/Users/seanfloersch/FloStrength/BaseballApp/TeamMaster.csv")%>%
  mutate(TeamStrength=(Hitting+Pitching))%>%
  mutate(TeamStrength=(TeamStrength-mean(TeamStrength))/sd(TeamStrength))%>%
  mutate(TeamStrength=(5+TeamStrength)/10)%>%
  mutate(FSSched=(4+FSSched)/8)%>%
  mutate(TeamStrength=(TeamStrength+FSSched)/2)%>%
  select(Team,"FSSep30"="TeamStrength","G"=GP,"Wpct"=WinPercent)%>%
  mutate(Team=ifelse(Team=="KC","KCR",Team))%>%
  mutate(Team=ifelse(Team=="SD","SDP",Team))%>%
  mutate(Team=ifelse(Team=="SF","SFG",Team))%>%
  mutate(Team=ifelse(Team=="TB","TBR",Team))%>%
  mutate(Team=ifelse(Team=="WSH","WSN",Team))
midway=ifelse(max(df2$G)>110,"Yes","No")
df<-left_join(df1,df2,by="Team")%>%
  group_by(Team)%>%
  mutate(UpdPred=ifelse(midway=="No",(((2*G)/162)*(FSSep30)+(((162-2*G))/162)*PredWpct),FSSep30))%>%
  mutate(UpdPred=((1-G/162)*UpdPred)+(G/162)*Wpct)%>%
  mutate(lb=UpdPred-0)%>%
  mutate(ub=UpdPred+0)
write_csv(df,"/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")

###Create Simulations
simMLB<- function(numit){
  wswinners=c()
  league <- c(1,1,2,2,1,2,1,2,1,2,2,2,2,1,1,1,2,1,2,2,1,1,1,2,1,1,2,2,2,1)
  division<-c(1,3,3,3,2,2,2,2,1,2,1,2,1,1,3,2,2,3,3,1,3,2,1,1,1,2,3,1,3,3)
  df1<- read.csv("/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")%>%
    mutate(teamID=Team)%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    mutate(leagueID=factor(league))%>%
    mutate(divisionID=division)%>%
    mutate(divisionID=ifelse(leagueID==2,divisionID+3,divisionID))%>%
    mutate(divisionID=factor(divisionID))%>%
    arrange(-FloPrediction)
  for (i in c(1:numit)){
    res<-runif(30, 0.1, .9)
    if (mean(res)>.6){
      res<-runif(30, 0.15, .85)
    }
    res = res- (mean(res)-.5)
    df<- df1 %>%
      mutate(per = res-.5)%>%
      mutate(se = ub-lb)%>%
      mutate(FloPrediction = FloPrediction + se*per)%>%
      arrange(-FloPrediction)
    divwin <- df %>%
      group_by(divisionID)%>%
      slice(1)%>%
      ungroup()%>%
      group_by(leagueID)%>%
      arrange(-FloPrediction)%>%
      mutate(Seed= 1:3)%>%
      ungroup()
    divwinners<- divwin$teamID
    wc<- df %>%
      filter(!teamID %in% divwinners)%>%
      arrange(-FloPrediction)%>%
      group_by(leagueID)%>%
      slice(1:3)%>%
      mutate(Seed=4:6)%>%
      ungroup()
      playoffs<- rbind(divwin,wc)  
      #wc round
      winners<- playoffs[which(playoffs$Seed==1 | playoffs$Seed==2),c(1,9,5)]
      ##6@3
      a63<- playoffs%>%
        filter(leagueID==2&Seed %in%c(3,6))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a63$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,a63$home,a63$away)
      winner<- data.frame(teamID=winner, Seed=36, leagueID=2)
      winners<- rbind(winner,winners)
      n63<- playoffs%>%
        filter(leagueID==1&Seed %in%c(3,6))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n63$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,n63$home,n63$away)
      winner<- data.frame(teamID=winner, Seed=36, leagueID=1)
      winners<- rbind(winner,winners)
      ##5@4
      a54<- playoffs%>%
        filter(leagueID==2&Seed %in%c(4,5))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a54$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,a54$home,a54$away)
      winner<- data.frame(teamID=winner, Seed=45, leagueID=2)
      winners<- rbind(winner,winners)
      n54<- playoffs%>%
        filter(leagueID==1&Seed %in%c(4,5))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n54$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,n54$home,n54$away)
      winner<- data.frame(teamID=winner, Seed=45, leagueID=1)
      winners<- rbind(winner,winners)
      playoffs<- playoffs %>%
        select(teamID,FloPrediction)
      playoffs <- left_join(winners,playoffs, by = "teamID")
      #division round
      ## 1 seeds
      a1<- playoffs%>%
        filter(leagueID==2&Seed %in%c(1,45))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,a1$home,a1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
      winners<- winner
      n1<- playoffs%>%
        filter(leagueID==1&Seed %in%c(1,45))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,n1$home,n1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
      winners<- rbind(winner,winners)
      ## 2 seeds
      a1<- playoffs%>%
        filter(leagueID==2&Seed %in%c(2,36))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,a1$home,a1$away)
      winner<- data.frame(teamID=winner, Seed=2, leagueID=2)
      winners<- rbind(winner,winners)
      n1<- playoffs%>%
        filter(leagueID==1&Seed %in%c(2,36))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,n1$home,n1$away)
      winner<- data.frame(teamID=winner, Seed=2, leagueID=1)
      winners<- rbind(winner,winners)
      playoffs<- playoffs %>%
        select(teamID,FloPrediction)
      playoffs <- left_join(winners,playoffs, by = "teamID")
      # league champ
      a1<- playoffs%>%
        filter(leagueID==2)%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(7, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>3.5,a1$home,a1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
      winners<- winner
      n1<- playoffs%>%
        filter(leagueID==1)%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n1$Prob
      res<-runif(7, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>3.5,n1$home,n1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
      winners<- rbind(winner,winners)
      playoffs<- playoffs %>%
        select(teamID,FloPrediction)
      playoffs <- left_join(winners,playoffs, by = "teamID")
      ### WORLD SERIES
      a1<- playoffs%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(7, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>3.5,a1$home,a1$away)
      wswinners[i]<-winner
  }
  wswinners<- data.frame(wswinners)%>%
    group_by(wswinners)%>%
    mutate(Probability = length(wswinners)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)
  return(wswinners)
}
simMLBendofseason<- function(numit){
  wswinners=c()
  league <- c(1,1,2,2,1,2,1,2,1,2,2,2,2,1,1,1,2,1,2,2,1,1,1,2,1,1,2,2,2,1)
  division<-c(1,3,3,3,2,2,2,2,1,2,1,2,1,1,3,2,2,3,3,1,3,2,1,1,1,2,3,1,3,3)
  df1<- read.csv("/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")%>%
    mutate(teamID=Team)%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    mutate(leagueID=factor(league))%>%
    mutate(divisionID=division)%>%
    mutate(divisionID=ifelse(leagueID==2,divisionID+3,divisionID))%>%
    mutate(divisionID=factor(divisionID))%>%
    arrange(-FloPrediction)
  for (i in c(1:numit)){
    df<- df1 %>%
      mutate(per = .5-.5)%>%
      mutate(se = ub-lb)%>%
      mutate(FloPrediction = FloPrediction)%>%
      arrange(-FloPrediction)
    divwin <- df %>%
      group_by(divisionID)%>%
      slice(1)%>%
      ungroup()%>%
      group_by(leagueID)%>%
      arrange(-FloPrediction)%>%
      mutate(Seed= 1:3)%>%
      ungroup()
    divwinners<- divwin$teamID
    wc<- df %>%
      filter(!teamID %in% divwinners)%>%
      arrange(-FloPrediction)%>%
      group_by(leagueID)%>%
      filter(teamID %in% c("SDP","ATL","NYM","BAL","KCR","DET"))%>%
      mutate(Seed=4:6)%>%
      ungroup()%>%
      mutate(Seed=ifelse(teamID=="DET",6,Seed))%>%
      mutate(Seed=ifelse(teamID=="KCR",5,Seed))
    playoffs<- rbind(divwin,wc)  
    #wc round
    winners<- playoffs[which(playoffs$Seed==1 | playoffs$Seed==2),c(1,9,5)]
    ##6@3
    a63<- playoffs%>%
      filter(leagueID==2&Seed %in%c(3,6))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a63$Prob
    res<-runif(3, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>1.5,a63$home,a63$away)
    winner<- data.frame(teamID=winner, Seed=36, leagueID=2)
    winners<- rbind(winner,winners)
    n63<- playoffs%>%
      filter(leagueID==1&Seed %in%c(3,6))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n63$Prob
    res<-runif(3, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>1.5,n63$home,n63$away)
    winner<- data.frame(teamID=winner, Seed=36, leagueID=1)
    winners<- rbind(winner,winners)
    ##5@4
    a54<- playoffs%>%
      filter(leagueID==2&Seed %in%c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a54$Prob
    res<-runif(3, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>1.5,a54$home,a54$away)
    winner<- data.frame(teamID=winner, Seed=45, leagueID=2)
    winners<- rbind(winner,winners)
    n54<- playoffs%>%
      filter(leagueID==1&Seed %in%c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n54$Prob
    res<-runif(3, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>1.5,n54$home,n54$away)
    winner<- data.frame(teamID=winner, Seed=45, leagueID=1)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction)
    playoffs <- left_join(winners,playoffs, by = "teamID")
    #division round
    ## 1 seeds
    a1<- playoffs%>%
      filter(leagueID==2&Seed %in%c(1,45))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
    winners<- winner
    n1<- playoffs%>%
      filter(leagueID==1&Seed %in%c(1,45))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
    winners<- rbind(winner,winners)
    ## 2 seeds
    a1<- playoffs%>%
      filter(leagueID==2&Seed %in%c(2,36))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner, Seed=2, leagueID=2)
    winners<- rbind(winner,winners)
    n1<- playoffs%>%
      filter(leagueID==1&Seed %in%c(2,36))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner, Seed=2, leagueID=1)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction)
    playoffs <- left_join(winners,playoffs, by = "teamID")
    # league champ
    a1<- playoffs%>%
      filter(leagueID==2)%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
    winners<- winner
    n1<- playoffs%>%
      filter(leagueID==1)%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction)
    playoffs <- left_join(winners,playoffs, by = "teamID")
    ### WORLD SERIES
    a1<- playoffs%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,a1$home,a1$away)
    wswinners[i]<-winner
  }
  wswinners<- data.frame(wswinners)%>%
    group_by(wswinners)%>%
    mutate(Probability = length(wswinners)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)
  return(wswinners)
}

wswinners<-simMLBendofseason(5000)
wswinners<- wswinners%>%
  mutate(Date=as.character("2024-09-30"))%>%
  select("teamID"=wswinners,Date,Probability)
df1<- read.csv("/Users/seanfloersch/FloStrength/BaseballApp/TeamPred24.csv")%>%
  select("teamID"=Team,Lg,Div)
wswin<-left_join(df1,wswinners,by="teamID")%>%
  mutate(Probability=ifelse(is.na(Probability)==TRUE,0,Probability))
old=read.csv("/Users/seanfloersch/FloStrength/BaseballApp/WSOdds2024.csv")
wswin<-rbind(old,wswin)

write_csv(wswin, "/Users/seanfloersch/FloStrength/BaseballApp/WSOdds2024.csv")

oldwswinners<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/MLB/WSOdds.csv")%>%
  select("wswinners"="Team",Probability,Date)
wswinners<-rbind(oldwswinners,wswinners)
wswinners<-wswinners%>%
  mutate(wswinners=ifelse(wswinners=="SDP","SD",wswinners))%>%
  mutate(wswinners=ifelse(wswinners=="TBR","TB",wswinners))%>%
  mutate(wswinners=ifelse(wswinners=="KCR","KC",wswinners))%>%
  mutate(wswinners=ifelse(wswinners=="SFG","SF",wswinners))
df2<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/MLB/TeamMaster.csv")%>%
  select("wswinners"=Team,Div,Lg)
test<-left_join(wswinners,df2,by ="wswinners")%>%
  rename("Team"="wswinners")
write_csv(test, "/Users/seanfloersch/FloStrength/FloStrengthFuture/MLB/WSOdds.csv")
