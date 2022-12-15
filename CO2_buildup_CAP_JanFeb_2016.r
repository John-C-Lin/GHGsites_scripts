#Analyze CO2 buildup in Salt Lake Valley during cold air pool events in Jan & Feb 2016
#V2(160214): introduce runaveCO2TF to carry out running average for CO2 (not just for PM2.5)
#V3(160216): cleaned up figures over V2
#V4(160319): added UofU site 
#Feb. 12th, 2016 by John C. Lin (John.Lin@utah.edu)

#######################
sites<-c("daybreak","draper","heber","logan","rosepark","sugarhouse","university")[c(1,2,5,6,7)]
sitenames<-sites;substring(sitenames,1,1)<-toupper(substring(sitenames,1,1))
names(sitenames)<-sites
sitenames[sitenames=="Draper"]<-"Suncrest"
YEARs<-c(2015,2016)
CO2dir<-"./"
VHDdir<-"/uufs/chpc.utah.edu/common/home/lin-group1/jcl/DAQ_Air_Quality/DAQ_atmchem_winter_2015_to_2016/Valley_heat_deficit/"   #where valley heat deficit can be found
DAQdir<-"/uufs/chpc.utah.edu/common/home/lin-group2/group_data/DAQ_qcklook_data/"
resultname<-"co2heatdefDAQ.merged"
runave.DT<-24  #window used for running average [hr]; match to Valley Heat Deficit timestep (determined by radiosonde launches)
runaveCO2TF<-TRUE
#######################

runave<-function(x, int,na.rm=F){
  outvec <- matrix(NA, nrow = length(x), ncol = 1)
  for(i in int:length(outvec)) {
   outvec[i, 1] <- mean(x[(i + 1 - int):i],na.rm=na.rm)
  } 
  return(outvec)
} #runave<-function(x, int,na.rm=F){

#running average, where window is CENTERED at current point
runave.ctr<-function(x, int,na.rm=FALSE){
  if(int%%1!=0|int<0)stop("int has to be a POSITIVE INTEGER interval!")
  outvec <- matrix(NA, nrow = length(x), ncol = 1)
  if(int%%2==1){
    #a) if interval is ODD, then center of window is simply the datapoint under consideration (index=i)
    inthalf<-(int-1)/2  #half of window
    inthalf1<-inthalf;inthalf2<-inthalf
  }else{
    #b) if interval is EVEN, then window is uneven--use 1 larger BACKWARD in time
    inthalf<-int/2  #half of window
    inthalf1<-inthalf;inthalf2<-inthalf-1
  } #if(int%%2==1){
    for(i in 1:length(outvec)) {
     i1<-i - inthalf1  
     i2<-i + inthalf2  
     #only generate average if entire window has data
     if(i1>0&i1<=length(x)&i2>0&i2<=length(x)){
       outvec[i, 1] <- mean(x[i1:i2],na.rm=na.rm)
     } #if(i1>0&i1<=length(x)&i2>0&i2<=length(x)){
    } #for(i in int:length(outvec)) {
  return(outvec)
} #runave.ctr<-function(x, int,na.rm=F){


#From Whiteman et al. [2014] ("Relationship between particulate air pollution and meteorological variables in Utah's Salt Lake Valley"):
#  "multi-day particulate episodes are triggered when the heat deficit rises above a certain threshold. Once this threshold is exceeded, PM2.5 concentrations tend to increase monotonically from day to day, often exceeding the NAAQS.  For the purposes of this paper, a reasonable threshold based on PM2.5 concentrations and set sufficiently above the diurnal inversion signal, is the mean heat deficit (4.04 MJ m-2) corresponding to all daily PM2.5 values in the wintertime POR that exceed half of the PM2.5 standard (i.e., 17.5 ug m-3). . .Hereafter, A SERIES OF 3 OR MORE TWICE-DAILY SOUNDINGS EACH HAVING H22 > 4.04 MJ m-2 WILL BE CALLED A PERSISTENT COLD-AIR POOL or PCAP."
VHDthresh<-4.04
VHD<-getr("VHD",path=VHDdir)
tmp<-getr("Hawthorne.qcklook",path=DAQdir)
#  average data to hourly timescales (NOTE:  floored--e.g., 02UT timestamp denotes obs averaged between 02UT and 03UT)
Time.hr<-format(tmp[,"Time"],"%Y-%m-%d %H")  #cut off minutes and sec
dum<-NULL
for(cc in 3:ncol(tmp)){
  xx<-tapply(tmp[,cc],Time.hr,mean,na.rm=TRUE) 
  dum<-cbind(dum,xx)
} #for(cc in 1:ncol(dat)){
colnames(dum)<-colnames(tmp)[-c(1,2)]
dum[is.nan(dum)]<-NA
Time<-strptime(rownames(dum),format="%Y-%m-%d %H",tz="GMT")
DAQdat<-data.frame(Time,dum)

dt.DAQ<-unique(diff(as.numeric(DAQdat[,"Time"]))/3600)  #time interval of DAQ dataset [hours]
#use running average--centered
PM2.5runave<-runave.ctr(DAQdat[,"PM2.5"],int=runave.DT/dt.DAQ,na.rm=TRUE)  
if(FALSE){
X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5))
sel<-1:nrow(tmp)
plot(tmp[sel,c("Time","PM2.5")],pch=16,lwd=2,type="o")  #original sub-hourly data
lines(DAQdat[,"Time"],DAQdat[,"PM2.5"],pch=16,col="blue",lwd=2,type="o")  #hourly averages
points(tmp[sel,c("Time","PM2.5")],pch=16)  #original sub-hourly data
lines(DAQdat[,"Time"],PM2.5runave,pch=16,col="orange",lwd=2,type="o")  #running average
} #if(FALSE){
DAQdat<-data.frame(DAQdat,PM2.5runave)

#read in CO2 observational data
co2.dat<-NULL
for(i in 1:length(YEARs))co2.dat<-rbind(co2.dat,getr(paste("SimCity_CO2_allsites_hrly_",YEARs[i],sep="")))
if(runaveCO2TF){
  dum<-co2.dat
  dt.co2<-unique(diff(as.numeric(co2.dat[,"Time"]))/3600) #time interval of CO2 dataset [hours]
  for(cc in 2:ncol(co2.dat)){
    rr<-runave.ctr(co2.dat[,cc],int=runave.DT/dt.co2,na.rm=TRUE)  
    rr[is.nan(rr)]<-NA
    dum[,cc]<-rr
  } #for(cc in 2:ncol(co2.dat)){
  co2.dat<-dum
} #if(runaveCO2TF){

#merge valley heat deficit [MJ m-2] with CO2 time series
tmp<-merge(x=VHD,y=co2.dat,by="Time")
#merge PM2.5 time series into dataset
dat<-merge(x=tmp,y=DAQdat,by="Time")

DT<-unique(diff(as.numeric(dat[,"Time"]))/3600)  #time interval [hours]
if(length(DT)>1)stop(paste("time interval NOT constant:",DT))

#Pairwise scatterplot between PM2.5 <=> CO2 at different sites
X11();par(cex.main=1.0,cex.axis=1.3,cex.lab=1.3)
pairs(dat[,c("PM2.5runave","CO2_daybreak","CO2_draper","CO2_sugarhouse","CO2_rosepark","CO2_university")],cex.labels=1.1)
cor(dat[,"PM2.5runave"],dat[,"CO2_daybreak"],use="complete.obs")^2
cor(dat[,"PM2.5runave"],dat[,"CO2_university"],use="complete.obs")^2
figfilenm<-paste("pairwise.png",sep="")
dev.copy(png,res=100,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))

#Relationship between heat deficit and PM2.5 or CO2
X11(width=11,height=6);par(cex.main=1.0,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5),mfrow=c(1,2))
#1) Time series
plot(dat[,c("Time.MST","PM2.5runave")],pch=16,type="l",lwd=2,axes=F)
title(main=paste("PM2.5 (Hawthorne)& HeatDef time series in Salt Lake Valley\nrunaveCO2TF=",runaveCO2TF," (",runave.DT,"hr running ave)"))
axis.POSIXct(side=1,x=dat[,"Time.MST"],format="%m/%y",las=0)
axis(2);box()
par(new=TRUE)
plot(dat[,c("Time.MST","HeatDef")],pch=16,xlab="",ylab="",axes=F,col="orange",type="l",lwd=2)
axis(4,col="orange",col.axis="orange")
mtext("HeatDef [MJ m-2]",side=4,line=2,col="orange",cex=1.3)
abline(h=VHDthresh,lty=2,lwd=1,col="orange")
#2) Correlation
R<-cor(dat[,"HeatDef"],dat[,"PM2.5runave"],use="na.or.complete")
#  X11();par(cex.main=1.3,cex.axis=1.0,cex.lab=1.3)
plot(dat[,c("HeatDef","PM2.5runave")],pch=16,main=paste("PM2.5 (Hawthorne):HeatDef correlation in Salt Lake Valley\nR^2=",signif(R^2,5)),
     xlab="Heat Deficit [MJ m-2]",ylab=paste("PM2.5 [ug/m3] (",runave.DT,"hr running ave)"))
abline(v=VHDthresh,lty=2,lwd=2)
figfilenm<-paste("PM2.5_HeatDef_Hawthorne.png",sep="")
dev.copy(png,width=11,height=6,units="in",res=100,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))
#3) Time series + correlation between CO2 and VHD, looped through each site
for(i in 1:length(sitenames)){
  X11(width=11,height=6);par(cex.main=1.0,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5),mfrow=c(1,2))
  site<-names(sitenames)[i]
  label<-paste("CO2_",site,sep="")
  plot(dat[,c("Time.MST",label)],pch=16,main=sitenames[i],type="l",lwd=2,axes=F)
  axis.POSIXct(side=1,x=dat[,"Time.MST"],format="%m/%Y")
  axis(2);box()
  par(new=TRUE)
  plot(dat[,c("Time.MST","HeatDef")],pch=16,xlab="",ylab="",axes=F,col="orange",type="l",lwd=2)
  axis(4,col="orange",col.axis="orange")
  mtext("HeatDef [MJ m-2]",side=4,line=2,col="orange",cex=1.3)
  abline(h=VHDthresh,lty=2,lwd=1,col="orange")

  R<-cor(dat[,"HeatDef"],dat[,label],use="na.or.complete")
  #X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3)
  plot(dat[,c("HeatDef",label)],pch=16,main=paste(sitenames[i],"\nR^2=",signif(R^2,5),"; runaveCO2TF=",runaveCO2TF," (",runave.DT,"hr running ave)"))
  abline(v=VHDthresh,lty=2,lwd=2)

  figfilenm<-paste("CO2_HeatDef_",sitenames[i],".png",sep="")
  dev.copy(png,width=11,height=6,units="in",res=100,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))
} #for(i in 1:length(sitenames)){

#Relationship between heat deficit and PM2.5 or CO2
#1) (Daybreak - Suncrest)
X11(width=11,height=6);par(cex.main=1.0,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5),mfrow=c(1,2))
#a) Time series
dCO2<-dat[,"CO2_daybreak"]-dat[,"CO2_draper"]
plot(dat[,"Time.MST"],dCO2,pch=16,type="l",lwd=2,axes=F,xlab="Time.MST",ylab="CO2 difference (Daybreak - Suncrest) [ppm]")
title(main=paste("CO2 difference (Daybreak - Suncrest)\nrunaveCO2TF=",runaveCO2TF," (",runave.DT,"hr running ave)"))
axis.POSIXct(side=1,x=dat[,"Time.MST"],format="%m/%y",las=0)
axis(2);box()
par(new=TRUE)
plot(dat[,c("Time.MST","HeatDef")],pch=16,xlab="",ylab="",axes=F,col="orange",type="l",lwd=2)
axis(4,col="orange",col.axis="orange")
mtext("HeatDef [MJ m-2]",side=4,line=2,col="orange",cex=1.3)
abline(h=VHDthresh,lty=2,lwd=1,col="orange")
#b) Correlation
R<-cor(dat[,"HeatDef"],dCO2,use="na.or.complete")
plot(dat[,"HeatDef"],dCO2,pch=16,main=paste("dCO2 (Daybreak - Suncrest):HeatDef correlation in Salt Lake Valley\nR^2=",signif(R^2,5)),
     xlab="Heat Deficit [MJ m-2]",ylab="CO2 difference (Daybreak - Suncrest) [ppm]")
abline(v=VHDthresh,lty=2,lwd=2)
figfilenm<-paste("dCO2_university_suncrest_HeatDef.png",sep="")
dev.copy(png,width=11,height=6,units="in",res=100,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))
#2) (University - Suncrest)
X11(width=11,height=6);par(cex.main=1.0,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5),mfrow=c(1,2))
#a) Time series
dCO2<-dat[,"CO2_university"]-dat[,"CO2_draper"]
plot(dat[,"Time.MST"],dCO2,pch=16,type="l",lwd=2,axes=F,xlab="Time.MST",ylab="CO2 difference (University - Suncrest) [ppm]")
title(main=paste("CO2 difference (University - Suncrest)\nrunaveCO2TF=",runaveCO2TF," (",runave.DT,"hr running ave)"))
axis.POSIXct(side=1,x=dat[,"Time.MST"],format="%m/%y",las=0)
axis(2);box()
par(new=TRUE)
plot(dat[,c("Time.MST","HeatDef")],pch=16,xlab="",ylab="",axes=F,col="orange",type="l",lwd=2)
axis(4,col="orange",col.axis="orange")
mtext("HeatDef [MJ m-2]",side=4,line=2,col="orange",cex=1.3)
abline(h=VHDthresh,lty=2,lwd=1,col="orange")
#b) Correlation
R<-cor(dat[,"HeatDef"],dCO2,use="na.or.complete")
plot(dat[,"HeatDef"],dCO2,pch=16,main=paste("dCO2 (University - Suncrest):HeatDef correlation in Salt Lake Valley\nR^2=",signif(R^2,5)),
     xlab="Heat Deficit [MJ m-2]",ylab="CO2 difference (University - Suncrest) [ppm]")
abline(v=VHDthresh,lty=2,lwd=2)
figfilenm<-paste("dCO2_university_suncrest_HeatDef.png",sep="")
dev.copy(png,width=11,height=6,units="in",res=100,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))



#look for PERSISTENT cold air pool--i.e., "a series of 3 or more TWICE-daily soundings each having H22 > 4.04 MJ m-2"
xrle<-rle(dat[,"HeatDef"]>4.04)
sel<-xrle$lengths>=(3*DT/12)&xrle$values
isPCAP<-rep(sel,xrle$lengths)

#create "PCAP index"--i.e., the amount of timesteps since PCAP started--and "PCAP start"
counter<-0
inds<-rep(NA,length(isPCAP))
PCAPstartTime.MST<-dat[,"Time.MST"];PCAPstartTime.MST[1:nrow(dat)]<-NA
startTime<-NA
for(i in 1:length(isPCAP)){
  if(isPCAP[i])counter<-counter+1
  if(!isPCAP[i]){counter<-0;startTime<-NA}
  inds[i]<-counter
  if(counter==1)startTime<-dat[i,"Time.MST"]
  PCAPstartTime.MST[i]<-startTime
} #for(i in 1:length(isPCAP)){

dat<-cbind(inds,dat);colnames(dat)[1]<-"inds.PCAP"
dat<-cbind(PCAPstartTime.MST,dat)
#save merged dataset
assignr(resultname,dat,printTF=TRUE)

dat<-getr(resultname)
DT<-unique(diff(as.numeric(dat[,"Time"]))/3600)  #time interval [hours]
#Plot PM2.5 as a function of timesteps since PCAP started ("inds.PCAP")
sub<-dat[dat[,"inds.PCAP"]!=0,]
#  loop through each PCAP event
events<-unique(sub[,"PCAPstartTime.MST"])
X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5))
plot(sub[,"inds.PCAP"]*DT/24,sub[,"PM2.5"],pch=16,xlab="Days since start of PCAP",ylab=paste("PM2.5 [ug/m3] (",runave.DT,"hr running ave)"),type="n")
title(main="PM2.5 buildup at Hawthorne since start of PCAP")
cols<-rev(1:length(events))
for(j in 1:length(events)){
  sel<-sub[,"PCAPstartTime.MST"]==events[j]
  points(sub[sel,"inds.PCAP"]*DT/24,sub[sel,"PM2.5runave"],type="o",pch=16,lwd=2.0,col=cols[j])
} #for(j in 1:length(events)){
legend("bottomright",legend=events,col=cols,lwd=2.5,text.col=cols)
figfilenm<-paste("PCAPbuildup_PM2.5_Hawthorne.png",sep="")
dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))

#Relationship between heat deficit and PM2.5 during PCAP (defined as "a series of 3 or more TWICE-daily soundings each having H22 > 4.04 MJ m-2")
R<-cor(sub[,"HeatDef"],sub[,"PM2.5runave"],use="na.or.complete")
X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3)
plot(sub[,c("HeatDef","PM2.5")],pch=16,main=paste("PM2.5 (Hawthorne):HeatDef correlation in Salt Lake Valley\nduring PCAP events;  R^2=",signif(R^2,5)),
     xlab="Heat Deficit [MJ m-2]",ylab=paste("PM2.5 [ug/m3] (",runave.DT,"hr running ave)"))

#Plot CO2 as a function of timesteps since PCAP started ("inds.PCAP")
for(i in 1:length(sitenames)){
  site<-names(sitenames)[i]
  label<-paste("CO2_",site,sep="")
  X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5))
  plot(sub[,"inds.PCAP"]*DT/24,sub[,label],pch=16,main=sitenames[i],xlab="Days since start of PCAP",ylab="CO2 [ppm]",type="n")
  cols<-rev(1:length(events))
for(j in 1:length(events)){
  sel<-sub[,"PCAPstartTime.MST"]==events[j]
  points(sub[sel,"inds.PCAP"]*DT/24,sub[sel,label],type="o",pch=16,lwd=2.0,col=cols[j])
} #for(j in 1:length(events)){
  #legend("topleft",legend=events,col=cols,lwd=2.5,text.col=cols)
  legend("bottomright",legend=events,col=cols,lwd=2.5,text.col=cols)
  figfilenm<-paste("PCAPbuildup_CO2_",sitenames[i],".png",sep="")
  dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))
} #for(i in 1:length(sitenames)){


#Relationship between heat deficit and CO2 during PCAP (defined as "a series of 3 or more TWICE-daily soundings each having H22 > 4.04 MJ m-2")
if(FALSE){
for(i in 1:length(sitenames)){
  site<-names(sitenames)[i]
  label<-paste("CO2_",site,sep="")
  R<-cor(sub[,"HeatDef"],sub[,label],use="na.or.complete")
  X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3)
  plot(sub[,c("HeatDef",label)],pch=16,main=paste(sitenames[i],"\nR^2=",signif(R^2,5),"; runaveCO2TF=",runaveCO2TF," (",runave.DT,"hr running ave)"))
} #for(i in 1:length(sitenames)){
} #if(FALSE){

#Can DIFFERENCE between (Daybreak - Suncrest) serve as proxy for PCAP event?
X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5))
plot(sub[,"inds.PCAP"]*DT/24,sub[,"CO2_daybreak"]-sub[,"CO2_draper"],pch=16,main="CO2 difference (Daybreak - Suncrest) since start of PCAP",
      xlab="Days since start of PCAP",ylab="CO2 difference (Daybreak - Suncrest) [ppm]",type="n")
  cols<-rev(1:length(events))
for(j in 1:length(events)){
  sel<-sub[,"PCAPstartTime.MST"]==events[j]
  points(sub[sel,"inds.PCAP"]*DT/24,sub[sel,"CO2_daybreak"]-sub[sel,"CO2_draper"],type="o",pch=16,lwd=2.0,col=cols[j])
} #for(j in 1:length(events)){
  #legend("topleft",legend=events,col=cols,lwd=2.5,text.col=cols)
  legend("bottomright",legend=events,col=cols,lwd=2.5,text.col=cols)
  figfilenm<-paste("PCAPbuildup_dCO2_daybreak_suncrest_diff_.png",sep="")
  dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))

#Can DIFFERENCE between (University - Suncrest) serve as proxy for PCAP event?
X11();par(cex.main=1.3,cex.axis=1.3,cex.lab=1.3,mar=c(5,4,4,5))
plot(sub[,"inds.PCAP"]*DT/24,sub[,"CO2_university"]-sub[,"CO2_draper"],pch=16,main="CO2 difference (University - Suncrest) since start of PCAP",
      xlab="Days since start of PCAP",ylab="CO2 difference (University - Suncrest) [ppm]",type="n")
  cols<-rev(1:length(events))
for(j in 1:length(events)){
  sel<-sub[,"PCAPstartTime.MST"]==events[j]
  points(sub[sel,"inds.PCAP"]*DT/24,sub[sel,"CO2_university"]-sub[sel,"CO2_draper"],type="o",pch=16,lwd=2.0,col=cols[j])
} #for(j in 1:length(events)){
  #legend("topleft",legend=events,col=cols,lwd=2.5,text.col=cols)
  legend("bottomright",legend=events,col=cols,lwd=2.5,text.col=cols)
  figfilenm<-paste("PCAPbuildup_dCO2_university_suncrest_diff_.png",sep="")
  dev.copy(png,filename=figfilenm);dev.off();print(paste(figfilenm,"output"))


