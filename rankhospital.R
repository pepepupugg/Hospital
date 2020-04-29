rankhospital<-function(state,outcome,num='best'){
  mdata<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  u<-unique(mdata$State)
  o<-c('heart attack','heart failure','pneumonia')
  if(!(state %in% u)){stop("invalid state")}
  if(!(outcome %in% o)){stop("invalid outcome")}
  statewise<-split(mdata,mdata$State)
  statedata<-statewise[state]
  statedata<-data.frame(statedata)
  i<-NULL
  x<-NULL
  if(num=='best'){num=1}
  result<-NULL
  if(outcome=='heart attack'){
    l<-statedata[,11]
    suppressWarnings(l<-as.numeric(l))
    l<-sort(l,decreasing = FALSE,na.last = NA)
    if(num=='worst'){num=length(l)}
    result = which(statedata[,11] == l[num])
  }
  if(outcome=='heart failure'){
    l<-statedata[,17]
    suppressWarnings(l<-as.numeric(l))
    l<-sort(l,decreasing = FALSE,na.last = NA)
    if(num=='worst'){num=length(l)}
    result = which(statedata[,17] == l[num])
  }
  if(outcome=='pneumonia'){
    l<-statedata[,23]
    suppressWarnings(l<-as.numeric(l))
    l<-sort(l,decreasing = FALSE,na.last = NA)
    if(num=='worst'){num=length(l)}
    result = which(statedata[,23] == l[num])
  }
  statedata[result,2][1]
}