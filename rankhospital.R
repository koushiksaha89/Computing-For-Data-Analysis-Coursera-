rankhospital<-function(state,outcome,num="best")
{
  data1<<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  if(!(state %in% data1$State))
  {
	  stop("invalid state")
  }
  
  outcome_list<-c("heart attack","heart failure","pneumonia")
  if(!(outcome %in% outcome_list))
	  {
	  stop("invalid outcome")
  }
  
  
  sub<-subset(data1,data1$State==state)
  sub[,11]<-as.numeric(sub[,11])
  sub[,17]<-as.numeric(sub[,17])
  sub[,23]<-as.numeric(sub[,23])
  
  if(!(is.character(num)))
  {
	  if(num > (length(sub$Hospital.Name)))
	  {
		  return(NA)
		  break
	  }
  }
  
  
  if(outcome==outcome_list[1])
  {
	  
		e<<-sub[with(sub,order(sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,sub$Hospital.Name)),]
		e<<-na.omit(e)
		if(is.numeric(num))
		{
			return(e[num,2])
		}
		else
		{
			if(num=="best")
			{
				return(e[1,2])
			}
			if(num=="worst")
			{
				t<-nrow(e)
				return(e[t,2])
			}
		}
  }
  
  if(outcome==outcome_list[2])
  {
	  
	  sub[with(sub,order(sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,sub$Hospital.Name)),]->e
	  e<<-na.omit(e)
	  if(is.numeric(num))
	  {
		  return(e[num,2])
	  }
	  else
	  {
		  if(num=="best")
		  {
			  return(e[1,2])
		  }
		  if(num=="worst")
		  {
			  t<-nrow(e)
			  return(e[t,2])
		  }
	  }
  }
  
  if(outcome==outcome_list[1])
  {
	  
	  sub[with(sub,order(sub$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,sub$Hospital.Name)),]->e
	  e<<-na.omit(e)
	  if(is.numeric(num))
	  {
		  return(e[num,2])
	  }
	  else
	  {
		  if(num=="best")
		  {
			  return(e[1,2])
		  }
		  if(num=="worst")
		  {
			  t<-nrow(e)
			  return(e[t,2])
		  }
	  }
  }
}
