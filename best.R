best<-function(state,condition){
  b<-NULL
  mdata<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  u<-unique(mdata$State)
  o<-c('heart attack','heart failure','pneumonia')
  if(!(state %in% u)){stop("invalid state")}
  if(!(condition %in% o)){stop("invalid outcome")}
  statewise<-split(mdata,mdata$State)
  statedata<-statewise[state]
  statedata<-data.frame(statedata)
  result<-NULL
  if(condition=='heart attack'){
    result = which(statedata[,11] == min(statedata[,11],na.rm = TRUE))
  }
  if(condition=='heart failure'){
    result = which(statedata[,17] == min(statedata[,17],na.rm = TRUE))
  }
  if(condition=='pneumonia'){
    result = which(statedata[,23] == min(statedata[,23],na.rm = TRUE))
  }
  statedata[result,2][1]
  
}