corr <- function(directory, threshold = 0) {
  n<-list.files(path=directory)
  #print(n)
  
  q<-c()
  for(i in 1:length(n))
  {
     op<-paste0(directory,"/",n[i],sep="")
     #print(op) 
    fi<-read.csv(op)
    c<-complete.cases(fi)
    d1<-length(c[c[TRUE]])
    if(d1>threshold)
    {
      t<-na.omit(fi)
      cr<-cor(x=t$sulfate,y=t$nitrate)
      q<-append(q,cr)
    }
  }
  return(q)
}
