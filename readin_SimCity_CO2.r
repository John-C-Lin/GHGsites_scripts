#Reads in SimCity CO2 time series datasets and saves them as .RData files
#May 19th, 2015 by John C. Lin (John.Lin@utah.edu)

#From Ben Fasoli (151006):
#I've also made changes to the timestamps for all sites in the Uinta Basin network. The time in the .dat files is now UTC to eliminate confusion between MST and MDT. This issue does not impact the POSIXct data in the .rds files, since the POSIXct class associates time offsets with each point. The POSIXct data can be formatted to whatever timezone is preferred by editing the tzone attribute of the time column. If this is the route you choose (it's usually what I prefer), you can change the data to UTC or MST using
#
#From Megan Ostlie (220922):
#Added logic for sites that need to read in data from more than one instrument
#
#data <- readRDS(rdsfile)
#attributes(data$Time)$tzone <- 'UTC

##########################
obsdir <- "/uufs/chpc.utah.edu/common/home/lin-group20/measurements/data/"
sites.all <- c("CSP","DBK","FRU","HDP","HEB","HPL","IMC","LGN","LG2","ROO","RPK","SUG","SUN","WBB")
sites <- sites.all
calraws <- c("cal","raw")[1]  #can be either "cal" (calibrated) or "raw"
YEARs <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
tracer <- "co2"  
#tracer <- "ch4"
readinTF <- TRUE  #reads in dataset and save as RDS files?
aveTF <- TRUE     #create HOURLY datasets by averaging 10-sec data?
##########################

tracer <- casefold(tracer,upper=TRUE)

#look up table of different instruments used at various sites
tmp <- c("CSP","lgr_ugga","DBK","licor_6262","DBK","licor_7000","FRU","lgr_ugga","HDP","lgr_ugga","HEB","licor_6262","HPL","lgr_ugga",
         "IMC","licor_6262","LGN","licor_6262","LG2","licor_6262","ROO","lgr_ugga","RPK","licor_6262","RPK","licor_7000","SUG","licor_6262","SUG","licor_7000","SUN","licor_6262","WBB","lgr_ugga")
tmp2 <- matrix(tmp,byrow=T,ncol=2)
instrm.matrix <- tmp2
instrm.sites <- instrm.matrix[instrm.matrix[,1]%in%sites,2]
names(instrm.sites) <- instrm.matrix[instrm.matrix[,1]%in%sites,1]
prevSite <- ""

for(yy in 1:length(YEARs)){
  YEAR <- YEARs[yy]
  print(paste("==================== YEAR:",YEAR," ===================="))

#Reads in entire dataset and saves as .rds output files
if(readinTF){
#for(i in 1:nrow(instrm.matrix)){
for(i in 1:length(instrm.sites)){
  site <- names(instrm.sites)[i]
  sitenm <- site;substring(sitenm,1,1) <- toupper(substring(sitenm,1,1))
  print(paste("*********** Site:",sitenm," *************"))

  #obsdir2 <- paste(obsdir,"/",tolower(site),"/",instrm.sites[i],"/calibrated/",sep="")
  obsdir2 <- paste(obsdir,"/",tolower(site),"/",instrm.sites[i],"/final/",sep="")  # 240520: new data pipeline has "final" directory that simplifies application of QAQC flag
  filenms <- list.files(path=obsdir2,pattern="dat")
  filenms <- filenms[grep(YEAR,filenms)]
  if(length(filenms)==0){print(paste("No data for",site,";  skipping!"));next}
  result <- NULL
for(ff in 1:length(filenms)){
  print(paste(".....Reading in",filenms[ff],"....."))
  tmp <- read.csv(file=paste(obsdir2,filenms[ff],sep=""))
  result <- rbind(result,tmp)
  gc()
} #for(ff in 1:length(filenms)){
  if(prevSite == site) result <- rbind(prevResult, result) # if same site measured by multiple instruments
  result <- result[order(result$Time_UTC),]
  prevResult <- result  
  attributes(result$Time_UTC)$tzone <- "UTC"  #change to UTC (if not already)
  resultname <- paste(sitenm,"_GHG_",YEAR,sep="")
  saveRDS(result,paste0(resultname,".rds"));print(paste0(resultname,".rds written out"))
  prevSite <- site
} #for(i in 1:nrow(instrm.matrix)){
} #if(readinTF){

#Create HOURLY datasets by averaging 10-sec data
if(aveTF){
for(j in 1:length(calraws)){
  calraw <- calraws[j]
  print(paste("----------- Calibrated or Raw:",calraw," ---------------"))
  dat.all <- NULL
for(i in 1:length(sites)){
  site <- sites[i]; sitenm <- site;substring(sitenm,1,1) <- toupper(substring(sitenm,1,1))
  print(paste("*********** Averaging Data from Site to Hourly Timescale:",sitenm," *************"))
  resultname <- paste(sitenm,"_GHG_",YEAR,sep="")
  #if(!existsr(resultname)){print(paste("No data for",site,";  skipping!"));next}
  #tmp <- getr(resultname)
  if(!file.exists(paste0(resultname,".rds"))){print(paste("No data for",site,";  skipping!"));next}
  tmp <- readRDS(paste0(resultname,".rds"))

  if(length(grep(tracer,colnames(tmp)))==0){print(paste("No",tracer,"data for",site,";  skipping!"));next}

  #240520:  No need to implement QAQC flag for new data pipeline if read from "final" folder above!
  # assign NA for data under calibration sequence or those that don't pass QA/QC metrics
  # just remove all data with QAQC_Flag < 0; includes problematic data and those measuring reference tanks (QAQC_Flag = -9)
  #sel <- tmp$QAQC_Flag < 0
  #tmp[sel,paste0(tracer,c("d_ppm_cal","d_ppm_raw"))] <- NA

  Time.all <- as.POSIXct(tmp[,"Time_UTC"],tz="GMT")
  YYYYMMDDHH <- format(Time.all,tz="GMT",format="%Y%m%d%H")
  #average 10-sec data to HOURLY
  #if(calraw=="cal")GHG <- tapply(tmp[,paste0(tracer,"d_ppm_cal")],YYYYMMDDHH,mean)  #NA if even 1 timepoint is NA
  #if(calraw=="raw")GHG <- tapply(tmp[,paste0(tracer,"d_ppm_raw")],YYYYMMDDHH,mean)  #NA if even 1 timepoint is NA
  sumNA<-function(x)return(sum(is.na(x)))
  if(calraw=="cal"){
    GHG <- tapply(tmp[,paste0(tracer,"d_ppm_cal")],YYYYMMDDHH,mean,na.rm=TRUE)  
    NN <- tapply(tmp[,paste0(tracer,"d_ppm_cal")],YYYYMMDDHH,length)
    N.NA <- tapply(tmp[,paste0(tracer,"d_ppm_cal")],YYYYMMDDHH,sumNA)
    sel <- (N.NA/NN)>0.5    #assign NA if more than half of time points are NAs...
    GHG[sel] <- NA
  } #if(calraw=="cal"){
  if(calraw=="raw")GHG <- tapply(tmp[,paste0(tracer,"d_ppm_raw")],YYYYMMDDHH,mean,na.rm=TRUE)  
  tt <- names(GHG)
  Time <- as.POSIXct(tt,tz="GMT",format="%Y%m%d%H")
  dat <- data.frame(Time,GHG)
  colnames(dat)[2] <- paste(tracer,"_",site,sep="")
  if(is.null(dat.all)){
    dat.all <- dat
  }else{
    dat.all <- merge(x=dat.all,y=dat,by="Time",all=TRUE)
  } #if(i>1){
  gc()
} #for(i in 1:length(sites)){
  #remove initial time period when too many observations are missing 
  numNA <- apply(dat.all[,-1],1,sumNA)
  xrle <- rle(numNA==2*length(sites))
  if(xrle$values[1]){
    dat.all <- dat.all[-(1:xrle$lengths[1]),]  #remove initial time period with ALL NAs
  } #if(xrle$values[1]){
  if(calraw=="cal")resultname <- paste("SimCity_",tracer,"_allsites_hrly_",YEAR,sep="")
  if(calraw=="raw")resultname <- paste("SimCity_",tracer,"_allsites_hrly_raw_",YEAR,sep="")
  # assignr(resultname,dat.all,printTF=TRUE)
  saveRDS(dat.all,paste0(resultname,".rds"));print(paste0(resultname,".rds written out"))

} #for(j in 1:length(calraws)){
} #if(aveTF){

} #for(yy in 1:length(YEARs)){
