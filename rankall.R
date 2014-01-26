rankall<-function(outcome,num="best")
{
	d<-data.frame()
	outcome_list<-c("heart attack","heart failure","pneumonia")
	if(!(outcome %in% outcome_list))
	{
		stop("invalid outcome")
		break
	}
	data1<-read.csv("outcome-of-care-measures.csv",colClasses="character")
	data1[,11]<-as.numeric(data1[,11])
	data1[,17]<-as.numeric(data1[,17])
	data1[,23]<-as.numeric(data1[,23])
	
	states<-sort(unique(data1$State))
	{
	
		
		for(i in 1:length(states))
		{
			sub<-subset(data1,data1$State==states[i])
			
			if(outcome==outcome_list[1])
			{
				sub<-subset(sub,!is.na(sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
				e<-sub[with(sub,order(sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,sub$Hospital.Name)),]
				
				if(is.character(num))
				{
					if(num=="best")
					{
						d<-rbind(d,data.frame(e[1,2],states[i]))
					}
					if(num=="worst")
					{
						d<-rbind(d,data.frame(e[nrow(e),2],states[i]))
					}
					
				}
				
				if(is.numeric(num))
				{
					d<-rbind(d,data.frame(e[num,2],states[i]))
				}
			}
			
			if(outcome==outcome_list[2])
			{
				sub<-subset(sub,!is.na(sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
				e<-sub[with(sub,order(sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,sub$Hospital.Name)),]
				if(is.character(num))
				{
					if(num=="best")
					{
						d<-rbind(d,data.frame(e[1,2],states[i]))
					}
					if(num=="worst")
					{
						d<-rbind(d,data.frame(e[nrow(e),2],states[i]))
					}
					
				}
				
				if(is.numeric(num))
				{
					d<-rbind(d,data.frame(e[num,2],states[i]))
				}
			}
			
			if(outcome==outcome_list[3])
			{
				
				sub<-subset(sub,!(is.na(sub$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
				e<-sub[with(sub,order(sub$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,sub$Hospital.Name)),]
				if(is.character(num))
				{
					if(num=="best")
					{
						d<-rbind(d,data.frame(e[1,2],states[i]))
					}
					if(num=="worst")
					{
						d<-rbind(d,data.frame(e[nrow(e),2],states[i]))
					}
					
				}
				
				if(is.numeric(num))
				{
					d<-rbind(d,data.frame(e[num,2],states[i]))
				}
			}
			
			
			
		}
	}
	names(d)<-c("hospital","state")
	
	return(d)
}
