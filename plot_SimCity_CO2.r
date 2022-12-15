#Plots time series of the Uinta Basin GHG observations
#V1(151006): now that the data file is stored in GMT, convert it to MST
#V2(151204): a) add new sites (Rosepark, Sugarhouse)
#            b) ability to choose specific months
#V3(160319): add new site (University of Utah)
#V4(170322): additional sites (all U-ATAQ sites)
#V5(170419): can combine multiple years into single plot
#Reads in the .RData* files created by "readin_Uinta_GHG.r"
#May 19th, 2015 by John C. Lin (John.Lin@utah.edu)

require(MASS)
################
sites.all<-c("CSP","DBK","FRU","HDP","HEB","HPL","IMC","LGN","ROO","RPK","SUG","SUN","WBB")
#sites.SEL<-c("CSP","FRU","HPL","ROO")  #Uintah Basin
sites.SEL<-c("DBK","IMC","RPK","SUG","SUN","WBB")   #Salt Lake Valley
SEL<-sites.all%in%sites.SEL
cols<-c("orange","black","black","cyan","purple","red","blue",
        "blue","blue","pink","green","orange","red")[SEL]
sites<-sites.all[SEL]
sitenames<-sites;substring(sitenames,1,1)<-toupper(substring(sitenames,1,1))
names(sitenames)<-sites
calraw<-"cal"  #can be either "cal" (calibrated) or "raw"

YEARs<-NULL
#YEARs<-c(2015,2016,2017)    #specify multiple years (for long time series)
YEAR<-2017;MONs<-1:6 #MON(s) to select
#YEAR<-2016;MONs<-11:12 #MON(s) to select
DAYs<-NULL  #if NULL, then select whole month; otherwise selects subset of days
#DAYs<-26:30  #if NULL, then select whole month; otherwise selects subset of days
if(!is.null(YEARs)){MONs<-NULL;DAYs<-NULL;YEAR<-paste(YEARs,collapse="_")}

tracer<-"CO2"
################

#read in hourly dataset with ALL sites (created by "readin_Uinta_GHG.r")
if(!is.null(YEARs)){
  dat.all<-NULL
  for(yy in 1:length(YEARs)){
    if(calraw=="cal")resultname<-paste("SimCity_",tracer,"_allsites_hrly_",YEARs[yy],sep="")
    if(calraw=="raw")resultname<-paste("SimCity_",tracer,"_allsites_hrly_raw_",YEARs[yy],sep="")
    tmp<-getr(resultname)
    SEL<-!sites%in%unlist(strsplit(colnames(tmp),split="_"))
    if(sum(SEL)>0){
      #there's missing column--need to create column with all NAs
      addcols<-matrix(NA,nrow=nrow(tmp),ncol=sum(SEL))
      colnames(addcols)<-paste0(tracer,"_",sites[SEL])
      tmp<-data.frame(tmp,addcols)
    } #if(sum(SEL)>0){
    #make sure column order is the same from year to year
    tmp<-tmp[,c("Time",paste0(tracer,"_",sites))] 
    dat.all<-rbind(dat.all,tmp)
    gc()
  } #for(yy in 1:length(YEARs)){
}else{
  if(calraw=="cal")resultname<-paste("SimCity_",tracer,"_allsites_hrly_",YEAR,sep="")
  if(calraw=="raw")resultname<-paste("SimCity_",tracer,"_allsites_hrly_raw_",YEAR,sep="")
  dat.all<-getr(resultname)
} #if(!is.null(YEARs)){

if(!is.null(MONs)){
  MM<-format(dat.all[,"Time"],format="%m")
  sel<-as.numeric(MM)%in%MONs
  if(!is.null(DAYs)){
    DD<-format(dat.all[,"Time"],format="%d")
    sel<-sel&(as.numeric(DD)%in%DAYs)
  } #if(!is.null(DAYS)){
  dat.all<-dat.all[sel,]
} #if(!is.null(MONs)){

#need to subset sites since not all sites may be available during specified time
SEL<-sites%in%unlist(strsplit(colnames(dat.all),split="_"))
sites<-sites[SEL];cols<-cols[SEL];sitenames<-sitenames[SEL]

colnms<-colnames(dat.all)
if(tracer=="CO2"){
  ylims<-c(400,600)          #ylims for time series plot
  ylims.diurnal<-c(400,500)  #ylims for diurnal plot
} #if(tracer=="co2"){
if(tracer=="CH4"){
  ylims<-c(1.9,10.0)          #ylims for time series plot
  ylims.diurnal<-c(1.9,4.0)  #ylims for diurnal plot
} #if(tracer=="ch4"){
  dat<-dat.all

  calraw2<-calraw;if(calraw=="cal")calraw2<-""
  #Plot time series
  sites.leg<-substring(colnms,5,nchar(colnms))
  SEL<-substring(colnms,1,3)==tracer
  SEL<-SEL&(sites.leg%in%sites)
  sites.leg<-sites.leg[SEL]
  sumNA<-function(x)return(sum(is.na(x)))
  SEL.x<-apply(dat[,SEL],1,sumNA)<sum(SEL)
  if(length(cols)!=length(sites))stop("length of cols not equal to length of sites")
  X11(width=8,height=6);par(cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  Time.LT<-dat[,"Time"]   #original time is in UTC
  attributes(Time.LT)$tzone<-"MST"
  matplot(x=Time.LT[SEL.x],y=dat[SEL.x,paste(tracer,"_",sites,sep="")],type="l",axes=FALSE,xlab="Date [MST]",ylab=paste(tracer,"[ppm]"),lty=1,pch=16,col=cols,lwd=1.5,ylim=ylims)
  xmain<-"Univ of Utah Obs Network Time Series"
  if(calraw=="raw")xmain<-paste(xmain,"\nraw(uncalibrated)")
  title(main=xmain)
  axis.POSIXct(side=1,Time.LT[SEL.x],format="%m/%d/%Y")
  axis(side=2);box()
  legend("topleft",sitenames[sites],col=cols,lty=1,lwd=2,cex=1.3,text.col=cols)
  figfilenm<-paste(tracer,calraw2,"_allsites_",YEAR,"_",paste(MONs,collapse="_"),".png",sep="")
  dev.copy(png,figfilenm,width=8,height=6,units="in",res=200);dev.off()
  print(paste(figfilenm,"generated"))

  #Plot average diurnal cycles in MST
  Time.LT<-dat[,"Time"]   #time is in UTC
  attributes(Time.LT)$tzone<-"MST"
  HH.LT<-format(Time.LT,format="%H")
  MM<-format(dat[,"Time"],format="%m")

  diurnal<-NULL
  for(ss in 1:length(sites)){
    site<-sites[ss]
    diurnal<-cbind(diurnal,tapply(dat[,paste(tracer,"_",site,sep="")],HH.LT,mean,na.rm=T))
  } #for(ss in 1:length(sites)){
  colnames(diurnal)<-colnms[SEL]
  Hour.LT<-as.numeric(rownames(diurnal))
  X11();par(cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  matplot(x=Hour.LT,y=diurnal,type="l",ylab=paste(tracer,"[ppm]"),xlab="Hour [MST]",
          lty=1,pch=16,col=cols,lwd=1.5,ylim=ylims.diurnal)
  xmain<-paste(tracer,"Average Diurnal Cycle\nYEAR=",YEAR,"MONs:",paste(MONs,collapse=" "))
  if(calraw=="raw")xmain<-paste(xmain,"\nraw(uncalibrated)")
  title(main=xmain)
  legend("topright",sitenames[sites],col=cols,lty=1,lwd=2,cex=1.3,text.col=cols)
  figfilenm<-paste(tracer,calraw2,"_allsites_diurnal",YEAR,"_",paste(MONs,collapse="_"),".png",sep="")
  dev.copy(png,figfilenm);dev.off()
  print(paste(figfilenm,"generated"))

  #Plot average seasonal cycle
if(length(MONs)>1){
  seasonal<-NULL
  for(ss in 1:length(sites)){
    site<-sites[ss]
    seasonal<-cbind(seasonal,tapply(dat[,paste(tracer,"_",site,sep="")],MM,mean,na.rm=T))
  } #for(ss in 1:length(sites)){
  colnames(seasonal)<-colnms[SEL]
  Mon<-as.numeric(rownames(seasonal))
  X11();par(cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  matplot(x=Mon,y=seasonal,type="l",ylab=paste(tracer,"[ppm]"),xlab="Hour [MST]",
          lty=1,pch=16,col=cols,lwd=1.5,ylim=ylims.diurnal)
  xmain<-paste(tracer,"Average Seasonal Cycle\nYEAR=",YEAR,"MONs:",paste(MONs,collapse=" "))
  if(calraw=="raw")xmain<-paste(xmain,"\nraw(uncalibrated)")
  title(main=xmain)
  legend("topright",sitenames[sites],col=cols,lty=1,lwd=2,cex=1.3,text.col=cols)
  figfilenm<-paste(tracer,calraw2,"_allsites_seasonal",YEAR,"_",paste(MONs,collapse="_"),".png",sep="")
  dev.copy(png,figfilenm);dev.off()
  print(paste(figfilenm,"generated"))
} #if(length(MONs)>1){


