#Reads in SimCity CO2 time series datasets and saves them as .RData files
#(151006): Implement reading in of RDS data files, and use UTC timezone as default
#V2(160319): Also read in UofU site (archived under "LGR_network")
#V3(160517): New data path, and 3 letter codes (consistent among all U-ATAQ sites)
#V4(170419): Based new version on "readin_SimCity_CO2V4.r"
#V6(210407): Added LG2 site, use new QAQC_Flag filtering method--i.e., remove all data with *negative* flags
#V7(220906): Save data as RDS files, instead of .RData* files
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
obsdir <- "/uufs/chpc.utah.edu/common/home/lin-group9/measurements/data/"
sites.all <- c("CSP","DBK","FRU","HDP","HEB","HPL","IMC","LGN","LG2","ROO","RPK","SUG","SUN","WBB")
sites <- sites.all
calraws <- c("cal","raw")[1]  #can be either "cal" (calibrated) or "raw"
YEARs <- c(2015,2016,2017,2018,2019,2020,2021,2022)[8]
#tracer <- "co2"  
tracer <- "ch4"
readinTF <- TRUE  #reads in dataset and save as RDS files?
aveTF <- TRUE     #create HOURLY datasets by averaging 10-sec data?
##########################

tracer <- casefold(tracer,upper=TRUE)

#look up table of different instruments used at various sites
tmp <- c("CSP","lgr_ugga","DBK","licor_6262","DBK","licor_7000","FRU","lgr_ugga","HDP","lgr_ugga","HEB","licor_6262","HPL","lgr_ugga","IMC","licor_6262","LGN","licor_6262","LG2","licor_6262","ROO","lgr_ugga","RPK","licor_6262","SUG","licor_6262","SUG","licor_7000","SUN","licor_6262","WBB","lgr_ugga")
tmp2 <- matrix(tmp,byrow=T,ncol=2)
instrm.matrix <- tmp2
instrm.sites <- tmp2[,2];names(instrm.sites) <- tmp2[,1]
prevSite <- ""

for(yy in 1:length(YEARs)){
  YEAR <- YEARs[yy]
  print(paste("==================== YEAR:",YEAR," ===================="))

#Reads in entire dataset and saves as .rds output files
if(readinTF){
for(i in 1:nrow(instrm.matrix)){
  site <- instrm.matrix[i]
  sitenm <- site;substring(sitenm,1,1)<-toupper(substring(sitenm,1,1))
  print(paste("*********** Site:",sitenm," *************"))

  obsdir2 <- paste(obsdir,"/",tolower(site),"/",instrm.sites[i],"/calibrated/",sep="")
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
  if(prevSite == site) {
	  result <- rbind(prevResult, result)}
  prevResult <- result  
  attributes(result$Time)$tzone <- "UTC"  #change to UTC (if not already)
  resultname <- paste(sitenm,"_GHG_",YEAR,sep="")
  # assignr(resultname,result,printTF=TRUE)
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

  # assign NA for data under calibration sequence or those that don't pass QA/QC metrics
  isNA <- is.na(tmp[,paste0("ID_",tracer)])|is.na(tmp$QAQC_Flag)
  sel <- tmp[,paste0("ID_",tracer)]!=(-10)   # ID_CO2 == -10 means that measuring ambient atmosphere; so remove if not indicating ambient atmosphere
  #V5 data processing not perfect, so also need to include QAQC_Flag = (-6), which is "Time elapsed between reference tank measurements out of range”
  # sel <- sel | ((tmp$QAQC_Flag!=0) & tmp$QAQC_Flag!=(-6))
  #V6 just remove all data with QAQC_Flag < 0
  sel <- sel | (tmp$QAQC_Flag < 0)
  tmp[sel&!isNA,paste0(tracer,c("d_ppm_cal","d_ppm_raw"))] <- NA

  # > colnames(getr("University_CO2_2016"))
  # [1] "Time"         "CO2d_ppm_raw" "CO2d_ppm_cal" "CH4d_ppm_raw" "CH4d_ppm_cal"
  # [6] "n_co2"        "n_ch4"       
  #if(sitenm=="University"){
    #follow same column names as the SimCity CO2 sites (instead of the columns in "LGR_network")
  #  colnms<-colnames(tmp)
  #  colnms[colnms=="CO2d_ppm_raw"]<-"raw"
  #  colnms[colnms=="CO2d_ppm_cal"]<-"corrected"
  #  colnames(tmp)<-colnms
  #} #if(sitenm=="University"){
  Time.all <- as.POSIXct(tmp[,"Time"],tz="GMT")
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
