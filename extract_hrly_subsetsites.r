#Extract subset of sites from dataset created by "readin_SimCity_CO2.r"
#V2(210406): deal with situation when selected sites not found in dataset
#April 22nd, 2019 by John C. Lin (John.Lin@utah.edu)

#############
#sites<-c("RPK","WBB"); YEARs <- c(2015,2016,2017,2018)
#sites<-c("LGN","LG2"); YEARs <- 2018:2020
#sites<-c("FRU"); YEARs <- 2019:2021
#sites<-c("FRU"); YEARs <- 2019:2021
#sites <- c("CSP","DBK","FRU","HDP","HEB","HPL","IMC","LGN","LG2","ROO","RPK","SUG","SUN","WBB")
sites <- c("WBB","FRU","HPL","CSP","HDP")
#sites <- c("WBB","HDP","SUG","IMC","SUN")[1:3]
YEARs <- 2021
#YEARs <- 2018:2019
#tracer<-"co2"
tracer<-"ch4"
#############

for(yy in 1:length(YEARs)){
  YEAR <- YEARs[yy]
  tracer <- casefold(tracer,upper=TRUE)
  resultname <- paste("SimCity_",tracer,"_allsites_hrly_",YEAR,sep="")

  #dat.all <- getr(resultname)
  dat.all <- readRDS(paste0(resultname,".rds"))
  colnms.sel <- c("Time",paste0(tracer,"_",sites))
  sel <- colnames(dat.all)%in%colnms.sel
  if(sum(sel)==1)print(paste("sites not found:",YEAR))
  dat<-dat.all[,sel]
  YYYYMMDDHH<-format(dat[,"Time"],format="%Y%m%d%H")
  Year<-substring(YYYYMMDDHH,1,4)
  Month<-substring(YYYYMMDDHH,5,6)
  Day<-substring(YYYYMMDDHH,7,8)
  Hour<-substring(YYYYMMDDHH,9,10)
  dat<-data.frame(Year,Month,Day,Hour,dat)
  colnames(dat)[colnames(dat)=="Time"]<-"Time_UTC"

  outputname<-paste("UUCON_",tracer,"_",paste0(sites,collapse="_"),"_hrly_",YEAR,".csv",sep="")
  write.csv(dat,file=outputname,row.names=FALSE)
  print(paste(outputname,"written out"))
} #for(yy in 1:length(YEARs)){
