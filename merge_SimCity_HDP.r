#Merges SimCity CO2 time series with Britt Stephens's RACCOON observations from HDP
#June 13th, 2017 by John C. Lin (John.Lin@utah.edu)

#####################
dir0<-"/uufs/chpc.utah.edu/common/home/lin-group4/jcl/SimCity/"

#datdir<-"/uufs/chpc.utah.edu/common/home/lin-group4/jcl/SimCity/Lin_SimCity_BAMS_paper/"
datdir<-"/uufs/chpc.utah.edu/common/home/lin-group4/jcl/SimCity/"
YEARs<-c(2015,2016,2017)    #specify multiple years (for long time series)
tracer<-"CO2"
plotTF<-TRUE
calraw<-"cal"  #can be either "cal" (calibrated) or "raw"
#####################

#read in RACOON HDP observations
rdat<-getr("HDP_RACCOON.hrly",path="Lin_SimCity_BAMS_paper/")[,c("time","CO2")]   #dataset created by Lin_SimCity_BAMS_paper/readin_RACCOON.r
colnames(rdat)<-c("time","CO2_HDP.RACCOON")

for(yy in 1:length(YEARs)){
  YEAR<-YEARs[yy]
  if(calraw=="cal")resultname<-paste("SimCity_",tracer,"_allsites_hrly_",YEAR,sep="")
  if(calraw=="raw")resultname<-paste("SimCity_",tracer,"_allsites_hrly_raw_",YEAR,sep="")
  sdat<-getr(resultname,path=dir0)
  gc()

  dat<-merge(x=sdat,y=rdat,by.x="Time",by.y="time",all.x=TRUE)

if(plotTF){
   X11();par(cex.axis=1.3,cex.lab=1.3,cex.main=1.3)
   plot(dat[,c("Time","CO2_DBK")],pch=16,cex=0.5,main=YEAR,xlab="Time",ylab="CO2")
   points(dat[,c("Time","CO2_HDP.RACCOON")],pch=16,cex=0.3,col="blue")
} #if(plotTF){
  
  #save results
  outfilename<-paste0(datdir,"/",resultname,".rds")
  saveRDS(dat,file=outfilename)
  print(paste(outfilename,"generated"))
} #for(yy in 1:length(YEARs)){
