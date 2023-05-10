source("Par_ff.R")
library("readxl")
Wins<-read_excel("Wins.xlsx")


#ReelFaceFinder is a function that takes in 3 variables being position which
#is the index of the top symbol of the reel face. The variable is is a matrix
#which represents the reels of a slot. The variable col represents which column.
#the function returns a matrix of indicies that are the face of the slot machine.
#ie (1,2,3), (7,8,9), or (47,48,1) 
ReelFaceFinder<-function(position,slot,col){
  reel_face<-matrix(NA,nrow=3,ncol=1)
  pos_p1<-position+1
  pos_p2<-position+2
  print(position)
  if(pos_p1>nrow(slot)){
    pos_p1<-pos_p1-nrow(slot)
    pos_p2<-pos_p2-nrow(slot)
  }
  if(pos_p2>nrow(slot)){
    pos_p2<-pos_p2-nrow(slot)
  }
  reel_face[1,1]<-slot[position,col]
  reel_face[2,1]<-slot[pos_p1,col]
  reel_face[3,1]<-slot[pos_p2,col]
  return(reel_face)
}

#Spin is a function that takes in the variable slot which represent the reels 
#of the slot machine. Spin "spins" the slot machine by sampling with replacement
#and outputting the reel face.
spin<-function(slot){
  position<-matrix(NA,nrow=1,ncol=5)
  
  position[1,1]<-sample(1:nrow(slot),1)
  position[1,2]<-sample(1:nrow(slot),1)
  position[1,3]<-sample(1:nrow(slot),1)
  position[1,4]<-sample(1:nrow(slot),1)
  position[1,5]<-sample(1:nrow(slot),1)
  
  
  reel_1<-ReelFaceFinder(position[1,1],slot,1)
  reel_2<-ReelFaceFinder(position[1,2],slot,2)
  reel_3<-ReelFaceFinder(position[1,3],slot,3)
  reel_4<-ReelFaceFinder(position[1,4],slot,4)
  reel_5<-ReelFaceFinder(position[1,5],slot,5)
  
 
  reel_face<-cbind(reel_1,reel_2)

  reel_face<-cbind(reel_face,reel_3)
  reel_face<-cbind(reel_face,reel_4)
  reel_face<-cbind(reel_face,reel_5)
  
  return(reel_face)
}

#symbol.check is a function that takes in 2 variables being symbol and 
#reel_face which represent a symbol and the face of the slot machine,
# this function calculates how many of a specific symbol are on that reel
# and returns a matrix with 1 row 5 columns whose indices represent
# how many of a symbol are on each reel
Symbol.Checker<-function(symbol,reel_face){
  reel_check1<-length(which(reel_face[,1]==symbol))+length(which(reel_face[,1]==14))
  
  reel_check2<-length(which(reel_face[,2]==symbol))+length(which(reel_face[,2]==14))
  
  reel_check3<-length(which(reel_face[,3]==symbol))+length(which(reel_face[,3]==14))
  
  reel_check4<-length(which(reel_face[,4]==symbol))+length(which(reel_face[,4]==14))
  
  reel_check5<-length(which(reel_face[,5]==symbol))+length(which(reel_face[,5]==14))
  
  check<-cbind(reel_check1,reel_check2)
  check<-cbind(check,reel_check3)
  check<-cbind(check,reel_check4)
  check<-cbind(check,reel_check5)
  return(check)
}

#combo.check is a function which takes in 2 variables being symbol.check which
#is a matrix with 5 columns and 1 row representing how many of a specific 
#symbol are on that reel and symbol which is the corresponding symbol to the 
#symbol.check. This function creates a matrix symbol wins whose values represent
#the symbol, length of win, number of lines for 5 length win, number of lines
#for 4 length win, and number of lines for 3 length win
Combo.Check<-function(symbol.check,symbol){
  symbol_wins<-matrix(0,nrow=1,ncol=5)
  colnames(symbol_wins)<-c("Symbol","Length_M","lines_5l","lines_4l","Lines_3l")
  if(symbol.check[1] & symbol.check[2] &symbol.check[3] &symbol.check[4] &symbol.check[5]>0){
    symbol_wins[1]<-symbol
    symbol_wins[2]<-5
    symbol_wins[3]<-symbol.check[1]*symbol.check[2]*
      symbol.check[3]*symbol.check[4]*symbol.check[5]
  }else if(symbol.check[1] & symbol.check[2] &symbol.check[3] &symbol.check[4]>0){
    symbol_wins[1]<-symbol
    symbol_wins[2]<-4
    symbol_wins[3]<-0
    symbol_wins[4]<-symbol.check[1]*symbol.check[2]*
      symbol.check[3]*symbol.check[4]
  }else if(symbol.check[1] & symbol.check[2] &symbol.check[3]>0){
    symbol_wins[1]<-symbol
    symbol_wins[2]<-3
    symbol_wins[5]<-symbol.check[1]*symbol.check[2]*symbol.check[3]
  }else{
    symbol_wins[1]<-symbol
    symbol_wins[2]<-0
    symbol_wins[3]<-0
  }
  return(symbol_wins)
}

#PayCalc is a function that takes in a variable combocheck that
#is a matrix with 1 row and 5 columns that  correspond to 
#symbol,length, lines length 5, lines length 4, and lines length 3.The function
#calculates the pay based off which symbol, how long is winning line, and number
#of lines.
PayCalc<-function(ComboCheck){
  if(ComboCheck[2]==0){
    return(0)
  }else{
    if(ComboCheck[1]==2){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[1,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[2,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[3,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==3){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[4,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[5,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[6,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==4){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[7,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[8,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[9,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==5){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[10,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[11,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[12,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==6){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[13,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[14,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[15,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==7){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[16,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[17,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[18,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==8){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[19,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[20,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[21,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==9){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[22,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[23,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[24,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==10){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[25,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[26,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[27,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==11){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[28,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[29,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[30,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==12){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[31,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[32,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[33,6]*ComboCheck[3]
        return(pay)
      }
    }
    if(ComboCheck[1]==13){
      if(ComboCheck[2]==0){
        return(0)
      }else if(ComboCheck[2]==3){
        pay<-Wins[34,6]*ComboCheck[5]
        return(pay)
      }else if(ComboCheck[2]==4){
        pay<-Wins[35,6]*ComboCheck[4]
        return(pay)
      }else{
        pay<-Wins[36,6]*ComboCheck[3]
        return(pay)
      }
    }
  }
}

#winning.Combos is a function that takes in a variable reel_face which is
#a matrix that represents the face of the slot machine and calculates the 
#pay of each spin
Winning.Combos<-function(reel_face){
  pay<-0

  s_2<-Symbol.Checker(2,reel_face)
  combo_2<-Combo.Check(s_2,2)

  pay<-PayCalc(combo_2)+pay
  
  s_3<-Symbol.Checker(3,reel_face)
  combo_3<-Combo.Check(s_3,3)

  pay<-PayCalc(combo_3)+pay
  
  s_4<-Symbol.Checker(4,reel_face)
  combo_4<-Combo.Check(s_4,4)

  pay<-PayCalc(combo_4)+pay
  
  s_5<-Symbol.Checker(5,reel_face)
  combo_5<-Combo.Check(s_5,5)

  pay<-PayCalc(combo_5)+pay
  
  s_6<-Symbol.Checker(6,reel_face)
  combo_6<-Combo.Check(s_6,6)
  
  pay<-PayCalc(combo_6)+pay
  
  s_7<-Symbol.Checker(7,reel_face)
  combo_7<-Combo.Check(s_7,7)

  pay<-PayCalc(combo_7)+pay
  
  s_8<-Symbol.Checker(8,reel_face)
  combo_8<-Combo.Check(s_8,8)
  pay<-PayCalc(combo_8)+pay
  
  s_9<-Symbol.Checker(9,reel_face)
  combo_9<-Combo.Check(s_9,9)
  
  pay<-PayCalc(combo_9)+pay
  
  s_10<-Symbol.Checker(10,reel_face)
  combo_10<-Combo.Check(s_10,10)
  
  pay<-PayCalc(combo_10)+pay
  
  s_11<-Symbol.Checker(11,reel_face)
  combo_11<-Combo.Check(s_11,11)
 
  pay<-PayCalc(combo_11)+pay
  
  s_12<-Symbol.Checker(12,reel_face)
  combo_12<-Combo.Check(s_12,12)
  
  pay<-PayCalc(combo_12)+pay
  
  s_13<-Symbol.Checker(13,reel_face)
  combo_13<-Combo.Check(s_13,13)
  
  pay<-PayCalc(combo_13)+pay
  return(as.double(pay))
}

#SlotDisplay takes in a variable reel_face which is a matrix that represents
#the face of the slot machine, ie reels showing. The function converts the 
#integer matrix to a char matrix converting integers that represent symbols
#to the corresponding symbols
SlotDisplay<-function(reel_face){
  reel_display<-matrix(NA,ncol=5,nrow = 3)
  for(i in 1:5){
    for(y in 1:3 ){
      reel_display[y,i]<-InvCodex(reel_face[y,i])
    }
  }
  return(reel_display)
}





#the function PromptFunc takes in no variables. It prints a list of options
#for the user to choose from and takes a integer as the input. The function 
#outputs the users choice
PromptFunc<-function(){
  writeLines(#displaying prompt
    "Option 1: Output your credit balance
Option 2: Deposit more Credits
Option 3: Spin slot machine for 1 Credit
Option 4: Cash out")
  
  option<-as.integer(readline(prompt = "Enter option number: "))#asking 
  #user for input
  return(option)
}

#this function takes in the variables option which is a integer representing
#what the user wants to program to do.Output which is a list of length 3, the
#first variable being end_bool which is a integer, this variable is used to 
#break the while loop to stop the slot machine.The second variable is balance 
#which represents how many credits the user has. The third variable is 
#window.matrix whose rows represent the idexes corresponding to which section
#of that reel is being displayed
PromptOption<-function(option, output, slot){
  if(option==1){#if your option is to Output your credit balance
    cat("Your current credit balance is: ", output[[2]])
    cat("\n")
    return(output)
  }else if(option==2){#if your option is to deposit more credits
    deposit<-as.integer(#asking user for how many credits they want to deposit
      readline(prompt="Enter the amount(integers) of credits to deposit: "))
    output[[2]]<-output[[2]]+deposit
    cat("Your current credit balance is: ", output[[2]])
    cat("\n")
    return(output)
  }else if(option==3){#if your option is to spin the slot
    if(output[[2]]>=1){#this if statement is for if you have atleast 1 credit
      
      reel_face<-spin(slot)#WindowSpin randomly spins the reels
      
      
      display<-SlotDisplay(reel_face)#slot_display returns 
      #a matrix of the window display of the slot machine
      print("The slot window display is: ")
      print(display)
      pay<-Winning.Combos(reel_face)#calculating how much you won
      output[[2]]<-output[[2]]+pay-1#account balance = accounnt balance+pay-1
      cat("You won: ",pay)
      cat("\n")
      cat("Your current credit balance is: ",output[[2]])
      cat("\n")
      return(output)
    }else{#if you dont have sufficient credits
      print("Insufficient credits, deposit more")
      return(output)
    }
  }else if(option==4){#cashing out of the slot
    cat("Your cashing out with: ", output[[2]])
    cat("\n")
    output[[1]]<-1
    return(output)
  }else{#improper option entered
    print("Improper option entered")
    cat("\n")
    return(output)
  }
}

#the function SlotManager takes in the variables slot which is a matrix 
#where the columns hold the stops of each reel. window.matrix is the matrix 
#where Each row of window.matrix corresponds with a reel, and the columns 
#correspond  to top, middle, and bottom of the display stops.
SlotManager<-function(slot){
  
  position<-matrix(1,nrow = 1,ncol=5)

  account_balance<-0 # setting account balance to 0

  reel_face<-spin(slot)#WindowSpin randomly spins the reels
 
  
  
  #display of the slot machine
  display<-SlotDisplay(reel_face)
  print(display)#printing window display of the slot machine
  
  output<-list(0, account_balance,position)#creating a list where the first 
  #element is used as a variable to stop the slot machine. The second variable
  #is the users account balance. The third variable is window matrix
  names(output)<-c("End_bool", "balance")
  
  
  
  while(output[[1]]==0){#this while loop will keep going until output[[1]], which
    #is the variable used to stop this loop, is equal to zero
    option<-PromptFunc()#asking the user for which option he wants
    output<-PromptOption(option, output, slot)#executing which option
    #the user wants
  }
}
