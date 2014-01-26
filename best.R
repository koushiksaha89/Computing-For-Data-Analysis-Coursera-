
best<-function(state,outcome)
{
out<-read.csv("outcome-of-care-measures.csv",colClasses="character")
s<-out[,7]
  if(!(state %in% s))
  {
    stop("invalid state")
	break
  }
 outcome_list<-c("heart attack","heart failure","pneumonia")
 if(!(outcome %in% outcome_list))
 {
   stop("invalid outcome")
   break
 }
 
selected_state<-subset(out,out$State==state)
selected_state[,11]<-as.numeric(selected_state[,11])
selected_state[,17]<-as.numeric(selected_state[,17])
selected_state[,23]<-as.numeric(selected_state[,23])

selected_state<-selected_state[order(selected_state$Hospital.Name),]

 if(outcome==outcome_list[1])
 {
   
   
   
   
   mini<-which.min(selected_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
   return(selected_state$Hospital.Name[mini])
 }
 
  if(outcome==outcome_list[2])
  {
    
    
    mini<-which.min(selected_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    return(selected_state$Hospital.Name[mini])
    
  }
  if(outcome==outcome_list[3])
  {
    
    
    mini<-which.min(selected_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    return(selected_state$Hospital.Name[mini])
    
  }

}
