complete <- function(directory,id=1:332) {
  d <- data.frame( id=rep(0, length(id)), nobs=rep(0,length(id))) # I think here it cann't be used
  #print(paste("ID :",id,sep=" "))
  
  for(i in 1:length(id))
{
  l<-nchar(id[i])
  if(l==1)
  {
    op<-paste(directory,"/","00",paste0(id[i],".csv",sep=""),sep="")
  }
  if (l==2)
  {
    op<-paste(directory,"/","0",paste0(id[i],".csv",sep=""),sep="")
  }
  if(l==3)
  {
    op<-paste(directory,"/",id[i],".csv",sep="")
  }
  
  c<-complete.cases(read.csv(op))
  d1<-length(c[c[TRUE]])
  d[i,]=c(id[i],d1)
  
}
 return(d)
}
