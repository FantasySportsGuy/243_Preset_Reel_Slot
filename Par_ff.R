load("Winning_Combos.Rda")

#Reel.Function takes in 14 integers that correspond to the number of each
#symbol on a reel
Reel.Generator<-function(Illidan,Naga,Gnome,Dwarf,Draenei,NightElf,
                         Human,Troll,Undead,Tauren,Orc,BloodElf,
                         Goblin,Naaru){
  reel.Illidan<-Symbol.Generator(1,Illidan)
  reel.Naga<-Symbol.Generator(2,Naga)
  reel.Gnome<-Symbol.Generator(3,Gnome)
  reel.Dwarf<-Symbol.Generator(4,Dwarf)
  reel.Draenei<-Symbol.Generator(5,Draenei)
  reel.NightElf<-Symbol.Generator(6,NightElf)
  reel.Human<-Symbol.Generator(7,Human)
  reel.Troll<-Symbol.Generator(8,Troll)
  reel.Undead<-Symbol.Generator(9,Undead)
  reel.Tauren<-Symbol.Generator(10,Tauren)
  reel.Orc<-Symbol.Generator(11,Orc)
  reel.BloodElf<-Symbol.Generator(12,BloodElf)
  reel.Goblin<-Symbol.Generator(13,Goblin)
  reel.Naaru<-Symbol.Generator(14,Naaru)
  
  reel<-rbind(reel.Illidan,reel.Naga)
  reel<-rbind(reel,reel.Gnome)
  reel<-rbind(reel,reel.Dwarf)
  reel<-rbind(reel,reel.Draenei)
  reel<-rbind(reel,reel.NightElf)
  reel<-rbind(reel,reel.Human)
  reel<-rbind(reel,reel.Troll)
  reel<-rbind(reel,reel.Undead)
  reel<-rbind(reel,reel.Tauren)
  reel<-rbind(reel,reel.Orc)
  reel<-rbind(reel,reel.BloodElf)
  reel<-rbind(reel,reel.Goblin)
  reel<-rbind(reel,reel.Naaru)
  reel<-na.omit(reel)
  return(reel)
}

#Symbol.Generator takes in a char that corresponds to a symbol and a int that 
#corresponds to how many of that symbol is on a specific reel
Symbol.Generator<-function(symbol,size){
  if(size==0){
    reel.symbol<-matrix(NA,1)
  }else{
    reel.symbol<-matrix(symbol,size)
  }
  return(reel.symbol)
}

#Slot.Generator creates a matrix that corresponds to the reels of the slot machine
Slot.Generator<-function(){
  
  reel.1<-Reel.Generator(Illidan=10,Naga=9,Gnome=7,Dwarf=5,Draenei=4,NightElf=3,
                         Human=2,Troll=2,Undead=2,Tauren=1,Orc=1,BloodElf=1,
                         Goblin=1,Naaru=0)
  reel.2<-Reel.Generator(Illidan=13,Naga=6,Gnome=5,Dwarf=3,Draenei=2,NightElf=2,
                         Human=2,Troll=2,Undead=2,Tauren=1,Orc=1,BloodElf=1,
                         Goblin=1,Naaru=7)
  reel.3<-Reel.Generator(Illidan=1,Naga=8,Gnome=9,Dwarf=4,Draenei=4,NightElf=4,
                         Human=2,Troll=2,Undead=2,Tauren=2,Orc=2,BloodElf=1,
                         Goblin=1,Naaru=6)
  reel.4<-Reel.Generator(Illidan=7,Naga=5,Gnome=7,Dwarf=4,Draenei=3,NightElf=3,
                         Human=3,Troll=3,Undead=2,Tauren=2,Orc=2,BloodElf=2,
                         Goblin=1,Naaru=4)
  reel.5<-Reel.Generator(Illidan=13,Naga=9,Gnome=6,Dwarf=2,Draenei=2,NightElf=3,
                         Human=2,Troll=2,Undead=2,Tauren=2,Orc=2,BloodElf=1,
                         Goblin=1,Naaru=1)
  
  reel.1<-sample(reel.1,size=length(reel.1),replace = FALSE)
  reel.2<-sample(reel.2,size=length(reel.2),replace = FALSE)
  reel.3<-sample(reel.3,size=length(reel.3),replace = FALSE)
  reel.4<-sample(reel.4,size=length(reel.4),replace = FALSE)
  reel.5<-sample(reel.5,size=length(reel.5),replace = FALSE)
  
  slot.reels<-cbind(reel.1,reel.2)
  slot.reels<-cbind(slot.reels,reel.3)
  slot.reels<-cbind(slot.reels,reel.4)
  slot.reels<-cbind(slot.reels,reel.5)
  colnames(slot.reels)<-c("Reel_1","Reel_2","Reel_3","Reel_4","Reel_5")
  return(slot.reels)
}

#Codex is a function that takes in a char which corresponds to a specific symbol
#and converts it into its abriviation for the par sheet
Codex<-function(symbol){
  if(symbol=="I"){
    return(1)
  }else if(symbol=="N"){
    return(2)
  }else if(symbol=="G"){
    return(3)
  }else if(symbol=="D"){
    return(4)
  }else if(symbol=="DR"){
    return(5)
  }else if(symbol=="NE"){
    return(6)
  }else if(symbol=="H"){
    return(7)
  }else if(symbol=="T"){
    return(8)
  }else if(symbol=="U"){
    return(9)
  }else if(symbol=="TN"){
    return(10)
  }else if(symbol=="O"){
    return(11)
  }else if(symbol=="B"){
    return(12)
  }else if(symbol=="GN"){
    return(13)
  }else if(symbol=="NU"){
    return(14)
  }
}

#InvCodex takes in a integer that corresponds to a symbol and returns that symbol
InvCodex<-function(integer){
  if(integer==1){
    return("Illidan")
  }else if(integer==2){
    return("Naga")
  }else if(integer==3){
    return("Gnome")
  }else if(integer==4){
    return("Dwarf")
  }else if(integer==5){
    return("Draenei")
  }else if(integer==6){
    return("Night Elf")
  }else if(integer==7){
    return("Human")
  }else if(integer==8){
    return("Troll")
  }else if(integer==9){
    return("Undead")
  }else if(integer==10){
    return("Tauren")
  }else if(integer==11){
    return("Orc")
  }else if(integer==12){
    return("Blood Elf")
  }else if(integer==13){
    return("Goblin")
  }else if(integer==14){
    return("Naaru")
  }
}

#Window counter is a function that takes in 3 varialbes being
#slot which is a matrix that represent the slot machines reels
#symbol is the symbol that we are checking the window count on the reels, and
#col is which reel we are checking the window count on. This function
#calculates how many 3 index windows on a specific reels do not contain 
#a specific symbol
WindowCounter<-function(slot,symbol,col){
  counter<-0
  for(i in 1:nrow(slot)){
    i_plus_1<-i+1
    i_plus_2<-i+2
    
    if(i_plus_1>nrow(slot)){
      i_plus_1<-i_plus_1-nrow(slot)
    }
    if(i_plus_2>nrow(slot)){
      i_plus_2<-i_plus_2-nrow(slot)
    }
    if((slot[i,col]==symbol)||(slot[i_plus_1,col] ==symbol)||(slot[i_plus_2,col]==symbol)){
      counter<-counter+1
    }
  }
  return(nrow(slot)-counter)
}


#TableGenerator is a function that takes in a variable slot which is a matrix
#that corresponds to the reels of the slot machine, it returns the factor table
TableGenerator<-function(slot){

  z1<-as.data.frame(table(slot[,1]))#frequency table of Reel 1
  names(z1)<-c("Symbol","Freq_1")
  z2<-as.data.frame(table(slot[,2]))#frequency table of Reel 2
  names(z2)<-c("Symbol","Freq_2")
  z3<-as.data.frame(table(slot[,3]))#frequency table of Reel 3
  names(z3)<-c("Symbol","Freq_3")
  z4<-as.data.frame(table(slot[,4]))#frequency table of Reel 4
  names(z4)<-c("Symbol","Freq_4")
  z5<-as.data.frame(table(slot[,5]))#frequency table of Reel 5
  names(z5)<-c("Symbol","Freq_5")
  
  #Creating a frequency matrix that has symbols and corresponding frequencies 
  #for each reel
  zcomb<-merge(z1,z2,by="Symbol",all.y = "TRUE")
  zcomb<-merge(zcomb,z3,by="Symbol",all.y = "TRUE")
  zcomb<-merge(zcomb,z4,by="Symbol",all.y = "TRUE")
  zcomb<-merge(zcomb,z5,by="Symbol",all.y = "TRUE")
  
  zcomb[is.na(zcomb)]<-0
  
  abv<-as.data.frame(c("I","N","G","D","DR","NE","H","T","U","TN","O","B","GN","NU"))
  names(abv)<-c("Abv")
  zcomb<-zcomb[order(zcomb$Symbol),]
  zcomb<-cbind(abv,zcomb)
  tempmatrix<-matrix(NA,nrow=nrow(Wins),ncol=5)#Creating a matrix which will
  #become the factor matrix
  
  #creating a factor matrix
  for(x in 1:36){#We are creating a factor table from the dataframe 
    #Winning_Combos which has 36 rows following a patern where if 
    # row# %% 3==1 it follows S(symbol), S, S,S, XS(not symbol),R(any symbol)
    #row# %% 3==2 it follows S(symbol), S, S, S,S, XS
    #row# %% 3==0 it follows S(symbol), S, S, S,S, S
    if(x %% 3 ==1){
      for (i in 1:3){#for reels 1,2, and 3 the factors correspond to how many
        #of that corresponding symbol is on that reel
        temp<-zcomb[which(zcomb$Symbol==Codex(Wins[x,i])),i+2]+zcomb[14,i+2]
        tempmatrix[x,i]<-temp
      }
      reel_total<-sum(zcomb$Freq_4)#length of Reel 4
      not<-reel_total-(zcomb[which(zcomb$Symbol==Codex(Wins[x,3])),4+2]+zcomb[14,4+2])
        
         #n(s-r)=number of times symbol s or a wild appears on reel r
         # Let t(r) = Total length of reel r
         #x(s,r)=t(r)-n(s,r)
        
         tempmatrix[x,4]<-WindowCounter(slot,symbol=Codex(Wins[x,3]),col=4)
         tempmatrix[x,5]<-length(slot[,5])
    }
    if(x %% 3 ==2){
      for (i in 1:4){#for reels 1,2,3, and 4 the factors correspond to how many
        #of that corresponding symbol is on that reel
        temp<-zcomb[which(zcomb$Symbol==Codex(Wins[x,i])),i+2]+zcomb[14,i+2]
        tempmatrix[x,i]<-temp
      }
      reel_total<-sum(zcomb$Freq_5)
      #n(s-r)=number of times symbol s or a wild appears on reel r
      # Let t(r) = Total length of reel r
      #x(s,r)=t(r)-n(s,r)
      #x(s,4)*( (x(s,4)-1)_P_2/ (t(4)-1)_P_2) is what tempmatrix[x,5] is
     
      
      tempmatrix[x,5]<-WindowCounter(slot,symbol=Codex(Wins[x,4]),col=5)
    }
    
    if(x %% 3 == 0){
      for (i in 1:5){#the factors correspond to how many of the corresponding
        #symbols are on that reel
        temp<-zcomb[which(zcomb$Symbol==Codex(Wins[x,i])),i+2]+zcomb[14,i+2]
        tempmatrix[x,i]<-temp
      }
    }
  }
  return(tempmatrix)
}

#this function calculates the 90% confidence factor and the upper and lower 
#limits of a 90% confidence interval for a a certain amount of pulls
#the function returns a data frame containing the amount of pulls, 
#the confidence factor, and the lower and upper limit of the confidence interval
confidence_90<-function(xbar,vi,pulls){
  #the formula for the 90% confidence interval is 
  #xbar +or- vi/sqrt(pulls)
  #xbar is the payback percentage, vi is the Volatility Index, and pulls 
  #is how many pulls 
  lower<-(xbar-(vi/sqrt(pulls)))
  upper<-(xbar +(vi/sqrt(pulls)))
  confidence_factor<-((upper-lower)/2)*100
  conf.table<-c(pulls,confidence_factor,lower,upper)
  conf.table<-as.data.frame(conf.table)
  conf.table<-t(conf.table)#transpose to turn conf.table from a list into a colm
  colnames(conf.table)=c("Pulls","confidence_factor","lower","upper")
  return(conf.table)
}

#this function returns a dataframe whose rows contain the number of pulls,
#confidence factor, lower limit, and upper limit of a confidence interval. 
#This function creates a dataframe for 1000,10000, 100000,1000000, and 10000000
#pulls from a given xbar(percent payback) and vi(volatility index)
Con_table<-function(xbar,vi){
  confidence_table_1000<-confidence_90(xbar=xbar,vi=vi,pulls=1000)
  confidence_table_10000<-confidence_90(xbar=xbar,vi=vi,pulls=10000)
  confidence_table_100000<-confidence_90(xbar=xbar,vi=vi,pulls=100000)
  confidence_table_1000000<-confidence_90(xbar=xbar,vi=vi,pulls=1000000)
  confidence_table_10000000<-confidence_90(xbar=xbar,vi=vi,pulls=10000000)
  
  contable<-rbind(confidence_table_1000,confidence_table_10000)
  contable<-rbind(contable,confidence_table_100000)
  contable<-rbind(contable,confidence_table_1000000)
  contable<-rbind(contable,confidence_table_10000000)
  contable<-as.data.frame(contable)
  contable$Pulls<-as.integer(contable$Pulls)
  return(contable)
}

#this function takes in the variables calc which is a dataframe containing two 
#columns which are hits which is the number of hits of a specific pay outcome, 
#and the corresponding payout, total.in which is the product of the number 
#of stops of each reel, and the amount of coins the slot takes. The output is 
#a dataframe containing the 90% confidence intervals and factor for  1000,10000, 
#100000,1000000, and 10000000 pulls.
confidence_table<-function(calc,total.in,coins){
  #if 0 is not the pay column of calc, we add in this row with the number of 
  #hits being total.in-sum(hits). This is the number of hits for 0 payout
  if(!(0 %in% calc$pay)){
    p_0<-data.frame(
      hits=total.in -sum(calc$hits),
      pay=0
    )
    calc<-rbind(calc,p_0)
  }
  hit.total<-sum(calc$hits)#calculating the number of total hits
  hit.percent=as.data.frame(calc$hits/(hit.total))#calculating the hit % of
  #each payout
  ev=hit.percent*calc$pay#ev is the expected value for each payout
  xbar<-sum(ev)/coins#xbar is the expected value of the machine per coin
  e.v.disadv=1-(sum(ev)/coins)#e.v.disadv is the expected disadvantage of the 
  #player per coin, this is also 1-xbar
  
  netpay<-as.data.frame((coins-calc$pay)/coins)#netpay is the amount of coins
  #minus the payout divided by the amount of coins. This is also equal to 
  #the amount of each payout divided by coins minus 1.
  
  #the games standard deviation is sqrt(sum((netpay-e.v.disadv)^2 * hit.percent))
  e<-netpay-e.v.disadv#netpay-e.v.disadv
  f<-e^2 #(netpay-e.v.disadv)^2
  cf<-f*hit.percent#(netpay-e.v.disadv)^2 * hit.percent)
  sd<-sqrt(sum(cf))#sqrt(sum((netpay-e.v.disadv)^2 * hit.percent))
  #vi is the games volatility index(V.I) which is equal to k*sigma, where 
  #k is the z score for the required confidence interval, and sigma is the 
  #standard deviation
  vi=sd*1.645
  contable<-Con_table(xbar=xbar,vi=vi)
  return(contable)
}

#ReelFreqGenerator returns a frequency table of the symbols of each reel
ReelFreqGenerator<-function(){
  Reels<-Slot.Generator()
  R_1<-as.data.frame(table(Reels[,1]))
  colnames(R_1)<-c("Var1","Freq1")
  R_2<-as.data.frame(table(Reels[,2]))
  colnames(R_2)<-c("Var1","Freq2")
  R_3<-as.data.frame(table(Reels[,3]))
  colnames(R_3)<-c("Var1","Freq3")
  R_4<-as.data.frame(table(Reels[,4]))
  colnames(R_4)<-c("Var1","Freq4")
  R_5<-as.data.frame(table(Reels[,5]))
  colnames(R_5)<-c("Var1","Freq5")
  
  r_comb<-merge(R_1,R_2,by="Var1",all = TRUE)
  colnames(r_comb)<-c("Var1","Reel 1","Reel 2")
  r_comb<-merge(r_comb,R_3,by="Var1",all = TRUE)
  colnames(r_comb)<-c("Var1","Reel 1","Reel 2","Reel 3")
  r_comb<-merge(r_comb,R_4,by="Var1",all = TRUE)
  colnames(r_comb)<-c("Var1","Reel 1","Reel 2","Reel 3","Reel 4")
  r_comb<-merge(r_comb,R_5,by="Var1",all = TRUE)
  colnames(r_comb)<-c("Var1","Reel 1","Reel 2","Reel 3","Reel 4","Reel 5")
  r_comb[is.na(r_comb)]<-0
  return(r_comb)
}


