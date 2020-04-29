rankall<-function(outcome,num='best'){
  mdata<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  o<-c('heart attack','heart failure','pneumonia')
  if(!(outcome %in% o)){stop("invalid outcome")}
  if(num=='best'){num=1}
  statewise<-split(mdata,mdata$State)
  dtf<-c()
  for (i in seq_along(statewise)){
    sw<-data.frame(statewise[i])
    if(outcome=='heart attack'){
      l<-sw[,11]
      suppressWarnings(l<-as.numeric(l))
      l<-sort(l,decreasing = FALSE,na.last = NA)
      if(num=='worst'){num=length(l)}
      result = which(sw[,11] == l[num])
    }
    if(outcome=='heart failure'){
      l<-sw[,17]
      suppressWarnings(l<-as.numeric(l))
      l<-sort(l,decreasing = FALSE,na.last = NA)
      if(num=='worst'){num=length(l)}
      result = which(sw[,17] == l[num])
    }
    if(outcome=='pneumonia'){
      l<-sw[,23]
      suppressWarnings(l<-as.numeric(l))
      l<-sort(l,decreasing = FALSE,na.last = NA)
      if(num=='worst'){num=length(l)}
      result = which(sw[,23] == l[num])
    }
    dtf<-c(dtf,sw[result,2][1],sw[result,7][1])
  }
  dtf<-data.frame(dtf)
  dtf
}