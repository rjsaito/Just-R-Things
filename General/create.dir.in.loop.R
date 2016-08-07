#create directories in loop

if(!file.exists(paste(senssubout,subset,sep=""))) dir.create(paste(senssubout,subset,sep=""))

dir.create(paste(outpath,"/","by.station/adj.jpeg/fe&sw",sep=""),showWarnings=F)