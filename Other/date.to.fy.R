#convert to fiscal year

date.convert.WY <- function(date,fmt="dash"){
  WYall=NULL
  if(fmt=="dash"){
    for(i in 1:length(date)){
      if(nchar(date[i])==4) WY=date[i] else  
        if(nchar(date[i])==7){
          WY<-as.numeric(format(as.Date(paste(date[i],"01",sep="-")),"%Y"))
          WY[as.numeric(format(as.Date(paste(date[i],"01",sep="-")),"%m"))>=10] <- 
            as.numeric(WY[as.numeric(format(as.Date(paste(date[i],"01",sep="-")),"%m"))>=10])+1
        } else{
          WY<-as.numeric(format(as.Date(date[i],"%m/%d/%Y"),"%Y"))
          WY[as.numeric(format(as.Date(date[i],"%m/%d/%Y"),"%m"))>=10] <- 
            as.numeric(WY[as.numeric(format(as.Date(date[i],"%m/%d/%Y"),"%m"))>=10])+1
        }
      WYall=c(WYall,WY)
    }
  }else if(fmt=="nodlm"){
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    date <- trim(date)
    for(i in 1:length(date)){
      if(nchar(date[i])==4) WY=date[i] else  
        if(nchar(date[i])==6){
          WY<-as.numeric(format(as.Date(paste(date[i],"01",sep=""),"%Y%m%d"),"%Y"))
          WY[as.numeric(format(as.Date(paste(date[i],"01",sep=""),"%Y%m%d"),"%m"))>=10] <- 
            as.numeric(WY[as.numeric(format(as.Date(paste(date[i],"01",sep=""),"%Y%m%d"),"%m"))>=10])+1
        } else{
          WY<-as.numeric(format(as.Date(date[i],"%Y%m%d"),"%Y"))
          WY[as.numeric(format(as.Date(date[i],"%Y%m%d"),"%m"))>=10] <- 
            as.numeric(WY[as.numeric(format(as.Date(date[i],"%Y%m%d"),"%m"))>=10])+1
        }
      WYall=c(WYall,WY)
    }
  }
  return(WYall)
}


date.convert <- function(date,fmt="dash"){
  allnewdate=NULL
  if(fmt=="dash"){
    for(i in 1:length(date)){
      if(nchar(date[i])==4) newdate <- format(as.Date(paste(date[i],"01","01",sep="-")),"%Y-%m-%d") else  
        if(nchar(date[i])==7) newdate <- format(as.Date(paste(date[i],"01",sep="-")),"%Y-%m-%d") else
          newdate <-format(as.Date(date[i],"%m/%d/%Y"),"%Y-%m-%d")
        allnewdate=c(allnewdate,newdate)
    }
  } else if(fmt=="nodlm"){
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    date <- trim(date)
    for(i in 1:length(date)){
      if(nchar(date[i])==4) newdate <- format(as.Date(paste(date[i],"01","01",sep=""),"%Y%m%d"),"%Y-%m-%d")else  
        if(nchar(date[i])==6) newdate <- format(as.Date(paste(date[i],"01",sep=""),"%Y%m%d"),"%Y-%m-%d") else
          newdate <-format(as.Date(date[i],"%Y%m%d"),"%Y-%m-%d")
        allnewdate=c(allnewdate,newdate)
    }
  }
  return(allnewdate)
}



