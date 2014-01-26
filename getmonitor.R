# Usage: getmonitor(12,"specdata",TRUE)
getmonitor <- function(id, directory, summarize = FALSE) {

l<-nchar(id)
if(l==1)
{
op<-paste(directory,"/","00",paste(id,".csv",sep=""),sep="")
data<-read.csv(op)
#print(class(data))
if(summarize == TRUE)
{
print(summary(data))
return(data)
}
else 
return(data)

}
if (l==2)
{
op<-paste(directory,"/","0",paste(id,".csv",sep=""),sep="")
data<-read.csv(op)
if(summarize == TRUE)
{
print(summary(data))
return(data)
}
else 
return(data)

}
if(l==3)
{
op<-paste(directory,"/",id,".csv",sep="")
data<-read.csv(op)
if(summarize == TRUE)
{
print(summary(data))
return(data)
}
else 
return(data)
}
}
