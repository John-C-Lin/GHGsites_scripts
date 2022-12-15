#Merges SimCity-era CO2 data with historic data from "DOE era"
#July 16th, 2017 by John C. Lin (John.Lin@utah.edu)

#################
DOEdatdir<-"DOE_historic_Utah_CO2_record_to_Obspack_2017/"   #generated with "readin_DOE_historic_CO2.r"
YEAR<-2015
sites<-c("DBK","MSA","RPK","SUG","UOU")   #sites to merge with
plotTF<-TRUE
#################

origresultname<-paste("SimCity_CO2_allsites_hrly_",YEAR,".rds",sep="")
newresultname<-paste0("SimCity_DOE_merge_CO2_allsites_hrly_",YEAR,".rds",sep="")
DAT<-readRDS(origresultname)

for(ss in 1:length(sites)){
  site<-sites[ss]
  dat<-DAT
  dat2<-readRDS(paste0(DOEdatdir,"/",site,"_DOE_historicCO2.rds"))

  print(paste("-----",site,"before: -----"))
  print(dat[1:5,])
  #merge the datasets
  tmp<-merge(dat,dat2[,c("Time","CO2")],by="Time",all.x=TRUE)

  #overwrite original blank dataset (NAs)
if(site%in%substring(colnames(dat),5,7)){
  isNA<-is.na(dat[,paste0("CO2_",site)])
  dat[isNA,paste0("CO2_",site)]<-tmp[isNA,"CO2"]
}else{
  #if historic DOE site NOT in SimCity dataset (e.g., UOU), then just add data as new column
  dat<-data.frame(dat,tmp[,"CO2"]);colnames(dat)[ncol(dat)]<-paste0("CO2_",site)
  DAT<-data.frame(DAT,NA);colnames(DAT)[ncol(DAT)]<-paste0("CO2_",site)
} # if(site%in%substring(colnames(dat),5,7)){
  print(paste("-----",site,"after : -----"))
  print(dat[1:5,])
if(plotTF){
  ylims<-c(390,500)
  X11();par(cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
  plot(dat[,c("Time",paste0("CO2_",site))],main=paste(site,"merged with DOE record"),
           pch=16,cex=0.5,ylim=ylims,type="o")
  points(dat[,c("Time","CO2_WBB")],pch=16,cex=0.5,col="red")
} #if(plotTF){
  DAT[,paste0("CO2_",site)]<-dat[,paste0("CO2_",site)]
} #for(ss in 1:length(sites)){

#save results
saveRDS(DAT,newresultname)
print(paste(origresultname,"merged with DOE historic data and regenerated as NEW result, called:",newresultname))

