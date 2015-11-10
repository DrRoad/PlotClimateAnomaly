#######################################################################################
#       faceit
# This script retrieves the csv filename that contains the information
#   of country, site, scenarios, etc.
#
#   Author: Wei Xiong
#   Created:       09/23/2014
#   usage:  faceit_getfilename(fileDir,ClimVar,SiteName)
#   fileDir: the directory of the csv files
#   ClimVar: Variables for generation
#   SiteName: SiteName for showing as the plot title
#######################################################################################

GetCliSceName<-function(csvfilename) {
    fifthchar<-substr(csvfilename,1,1)
    sixthchar<-substr(csvfilename,2,2)
    seventhchar<-substr(csvfilename,3,3)
    eigthchar<-substr(csvfilename,4,4)
    # the fifthchar
    df1<-data.frame(charname=c(0:6,'S',LETTERS[1:13]),value=c('1980-2009 baseline','A2-2005-2035 (Near-term)','B1-2005-2035 (Near-term)','A2-2040-2069 (Mid-Century)','B1-2040-2069 (Mid-Century)','A2-2070-2099 (End-of-Century)','B1-2070-2099 (End-of-Century)','sensitivity scenarios','observational time period (determined in file)','RCP3PD 2010-2039 (Near-term)','RCP45 2010-2039 (Near-term)','RCP60 2010-2039 (Near-term)','RCP85 2010-2039 (Near-term)','RCP3PD 2040-2069 (Mid-Century)','RCP45 2040-2069 (Mid-Century)','RCP60 2040-2069 (Mid-Century)','RCP85 2040-2069 (Mid-Century)','RCP3PD 2070-2099 (End-of-Century)','RCP45 2070-2099 (End-of-Century)','RCP60 2070-2099 (End-of-Century)','RCP85 2070-2099 (End-of-Century)'))
    df2<-data.frame(charname=c('X','0','Q','T',LETTERS[22:26]),value=c('no GCM used','imposed values (sensitivity tests)','Bias-corrected MERRA','NASA POWER','NARR','ERA-INTERIM','MERRA','NCEP CFSR','NCEP/DoE Reanalysis-2'))
    df3<-data.frame(charname=c('X','0',LETTERS[1:16]),value=c('no GCM used','imposed values (sensitivity tests)','bccr','cccma cgcm3','cnrm','csiro','gfdl 2.0','gfdl 2.1','giss er','inmcm 3.0','ipsl cm4','miroc3 2 medres','miub echo g','mpi echam5','mri cgcm2','ncar ccsm3','ncar pcm1','ukmo hadcm3'))
    df4<-data.frame(charname=c('0',LETTERS[1:20]),value=c('imposed values (sensitivity tests)','ACCESS1-0','bcc-csm1-1','BNU-ESM','CanESM2','CCSM4','CESM1-BGC','CSIRO-Mk3-6-0','GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC','HadGEM2-ES','inmcm4','IPSL-CM5A-LR','IPSL-CM5A-MR','MIROC5','MIROC-ESM','MPI-ESM-LR','MPI-ESM-MR','MRI-CGCM3','NorESM1-M'))
    #df5<-data.frame(charname=c('X',0:7,LETTERS[1:6],LETTERS[23:26]),value=c('no additional downscaling','imposed values (sensitivity tests)','WRF','RegCM3','ecpc','hrm3','crcm','mm5i','RegCM4','GiST','MarkSIM','WM2','1/8 degree BCSD','1/2 degree BCSD','2.5minute WorldClim','TRMM 3B42','CMORPH','PERSIANN','GPCP 1DD'))
    df5<-data.frame(charname=c('X',0:7,LETTERS[1:6]),value=c('no additional downscaling','imposed values (sensitivity tests)','WRF','RegCM3','ecpc','hrm3','crcm','mm5i','RegCM4','GiST','MarkSIM','WM2','1/8 degree BCSD','1/2 degree BCSD','2.5minute WorldClim'))
    df6<-data.frame(charname=c('X',LETTERS[1:11]),value=c('Observations (no scenario)','Mean Change from GCM','Mean Change from RCM','Mean Change from GCM modified by RCM','Mean Temperature Changes Only','Mean Precipitation Changes Only','Mean and daily variability change for Tmax,Tmin,and P','P,Tmax and Tmin daily variability change only','Tmax and Tmin daily variability and mean change only','P daily variability and mean change only','Tmax and Tmin daily variability change only','P daily variability change only'))
    time_emission<-""
    source<-""
    method<-""
    type<-""
    #fifth char
    for (thisid in 1:nrow(df1)){
        if (fifthchar==df1[thisid,1]) {
            time_emission<-df1[thisid,2]
            break
        }
    }
    #sixth char
    if (fifthchar %in% c('0','S','A')){
        for (thisid1 in 1:nrow(df2)){
            if (sixthchar==df2[thisid1,1]) {
                source<-df2[thisid1,2]
                break
            }
        }
    } else if (fifthchar %in% c(1:6)){
        for (thisid1 in 1:nrow(df3)){
            if (sixthchar==df3[thisid1,1]) {
                source<-df3[thisid1,2]
                break
            }
        }
    } else {
        for (thisid1 in 1:nrow(df4)){
            if (sixthchar==df4[thisid1,1]) {
                source<-df4[thisid1,2]
                break
            }
        }
    }
    # Seventh char
    for (thisid in 1:nrow(df5)){
        if (seventhchar==df5[thisid,1]) method<-df5[thisid,2]
    }
    # Eighth char
    for (thisid in 1:nrow(df6)){
        if (eigthchar==df6[thisid,1]) type<-df6[thisid,2]
    }
    
    return(paste(time_emission,source,method,type,sep="_"))
}


faceit_ClimAnomaly<-function(fileDir,ClimVar,plottype,plotformat,output){
    ## Bejin debug
csvfiles<-list.files(fileDir,pattern="\\.csv$")

futureclimatelist<-unique(substring(csvfiles,5,8))
TitleName1<-""

for (thisw in 1:length(futureclimatelist)){
    if (substr(futureclimatelist[thisw],1,1)!="0") TitleName1=paste(TitleName1,GetCliSceName(futureclimatelist[thisw]),sep="\n")
}

#setwd(fileDir)
dd0<-data.frame(yr=NA,mo=NA,se=NA,se_yr=NA,ClimCol=NA)
dd1<-data.frame(yr=NA,mo=NA,se=NA,se_yr=NA,ClimCol=NA)
TitleName<-""
skipline=2
for (thisfile in 1:length(csvfiles)){
    conn=file(paste(fileDir,"/",csvfiles[thisfile],sep=""),open="r")
    for (i in 1:4){
        linn=readLines(conn,1)
        if (i==1) WeatherInfoName<-unlist(strsplit(linn,","))
        if (i==2) WeatherInfo<-unlist(strsplit(linn,","))
        other<-unlist(strsplit(linn,","))
        if (other[1]=="%"){
            skipline=i-1
            break
        }
        }
    for (j in 1:length(WeatherInfoName)){
        if (WeatherInfoName[j] %in% c("wst_notes","WST_NOTES","\"WST_NOTES\"")){
            TitleName<-WeatherInfo[j]
            break
        }
    }
    close(conn)
    OriData<-read.csv(csvfiles[thisfile],skip=skipline)
    x<-as.POSIXct(paste(OriData$W_DATE),format="%Y-%m-%d")
    mo<-as.numeric(strftime(x,"%m"))
    yr<-as.numeric(strftime(x,"%Y"))
    se<-trunc(mo/3.1)+13
    se_yr<-trunc(mo/12.1)+17
    if (ClimVar=="SRAD") {
        ClimCol<-OriData$SRAD
        unit<-"MJ"
        ClimName<-"Solar radiation"
    }
    if (ClimVar=="TMAX"){
        ClimCol<-OriData$TMAX
        unit<-"°C"
        ClimName<-"Maximum temperature"
    }
    if (ClimVar=="TMIN"){
        ClimCol<-OriData$TMIN
        unit<-"°C"
        ClimName<-"Minimum temperature"
    }
    if (ClimVar=="RAIN"){
        ClimCol<-OriData$RAIN
        unit<-"mm"
        ClimName<-"Precipitation"
    }
    if (ClimVar=="TDEW"){
        ClimCol<-OriData$TDEW
        unit<-"°C"
        ClimName<-"Dewpoint temperature"
    }
    if (ClimVar=="VPRSD"){
        ClimCol<-OriData$VPRSD
        unit<-"kPa"
        ClimName<-"Vapor pressure"
    }
    if (ClimVar=="RHUMD"){
        ClimCol<-OriData$RHUMD
        unit<-"%"
        ClimName<-"Relative humidity at TMIN"
    }
    if (ClimVar=="WIND"){
        ClimCol<-OriData$WIND
        unit<-"m/s"
        ClimName<-"Wind speed"
    }
    dd<-data.frame(yr,mo,se,se_yr,ClimCol)
    
    
    if (substr(csvfiles[thisfile],5,5)=="0") {
       dd0<-rbind(dd0,dd)
       if (ClimVar %in% c("SRAD","RAIN")) {
            BaselineVar0<-aggregate(ClimCol~yr+mo,dd0,sum)
            BaselineVar<-aggregate(ClimCol~mo,BaselineVar0,mean) #the monthly value of different years
            BaselineSe0<-aggregate(ClimCol~yr+se,dd0,sum)
            BaselineSe<-aggregate(ClimCol~se,BaselineSe0,mean)
            BaselineYr0<-aggregate(ClimCol~yr+se_yr,dd0,sum)
            BaselineYr<-aggregate(ClimCol~se_yr,BaselineYr0,mean)
            }
        if (ClimVar %in% c("TMAX","TMIN","TDEW","VPRSD","RHUMD","WIND")) {
            BaselineVar<-aggregate(ClimCol~mo,dd0,mean)
            BaselineSe<-aggregate(ClimCol~se,dd0,mean)
            BaselineYr<-aggregate(ClimCol~se_yr,dd0,mean)
            }
        names(BaselineVar)<-c("Group","Value")
        names(BaselineSe)<-c("Group","Value")
        names(BaselineYr)<-c("Group","Value")
        Baseline<-rbind(BaselineVar,BaselineSe,BaselineYr)
    }
    else {
        #csvfilename<-csvfiles[thisfile]
        #TitleName<-paste(TitleName,substr(csvfiles[thisfile],1,4),",",GetCliSceName(csvfiles[thisfile]),"\n",sep='')
       
        dd1<-rbind(dd1,dd)
        if (ClimVar %in% c("SRAD","RAIN")) {
            FutureClimVar<-aggregate(ClimCol~yr+mo,dd1,sum)
            FutureClimSe<-aggregate(ClimCol~yr+se,dd1,sum)
            FutureClimYr<-aggregate(ClimCol~yr+se_yr,dd1,sum)
        }
        if (ClimVar %in% c("TMAX","TMIN","TDEW","VPRSD","RHUMD","WIND")) {
            FutureClimVar<-aggregate(ClimCol~yr+mo,dd1,mean)
            FutureClimSe<-aggregate(ClimCol~yr+se,dd1,mean)
            FutureClimYr<-aggregate(ClimCol~yr+se_yr,dd1,mean)
        }
        names(FutureClimVar)<-c("Year","Group","Value")
        names(FutureClimSe)<-c("Year","Group","Value")
        names(FutureClimYr)<-c("Year","Group","Value")
        FutureClim<-rbind(FutureClimVar,FutureClimSe,FutureClimYr)
        FutureMean<-aggregate(Value~Group,FutureClim,mean)
    }
}

if (plotformat=="png") png(output, height=400+(length(futureclimatelist)-1)*10, width=780)
if (plotformat=="pdf") pdf(output, height=4.5+(length(futureclimatelist)-1)*0.1,width=8.8)
#plot.new()
#attach(mtcars)
#par(mfrow=c(1,2))
#op <- par(oma=c(5,7,1,1))
#png(file="boxplot.png", height=500, width=1000)

#boxplot(ClimCol~mo,data=FutureClimVar,xlab="Month",ylab=ClimVar,main="Maps",whiskcol="red",col="red",outline=TRUE,ylim=c(0,800),xlim=c(1,12),xaxs="i",yaxs="i")
if (plottype=="absolute"){
#TitleName<-paste(TitleName,ClimName, sep=",")
TitleName<-ClimName
#TitleName<-paste(TitleName,TitleName1)
Ymin<-min(Baseline$Value)
Ymax<-max(FutureClim$Value)
par(mar=c((length(futureclimatelist)-1)*1+4,4,2,2))
boxplot(Value~Group,data=FutureClim,xlab="Month",ylab=paste(ClimVar," (",unit,")",sep=""),ylim=c(Ymin,Ymax),main=TitleName,xaxt="n")
axis(1,at=1:17,labels=c("1","2","3","4","5","6","7","8","8","10","11","12","JFM","AMJ","JAS","OND","Ya"),par(cex.axis=0.6))

mtext(TitleName1, line=-1,outer=TRUE, side=1,cex=0.8)


#boxplot(Value~Group,data=FutureClim,xlab="Month",ylab=paste(ClimVar," (",unit,")",sep=""),ylim=c(Ymin,Ymax),main=TitleName,xaxt="n",add=TRUE)

lines(Value~Group,data=subset(FutureMean,Group<=12),xlim=c(1,12),xaxt="n",type="o",pch=22,col="red")
points(Value~Group,data=subset(FutureMean,Group>12),xlim=c(13,17),xaxt="n",type="p",pch=22,col="red")

lines(Value~Group,data=subset(Baseline,Group<=12),xlim=c(1,12),xaxt="n",type="o",pch=8,col="green")
points(Value~Group,data=subset(Baseline,Group>12),xlim=c(13,17),xaxt="n",type="p",pch=8,col="green")
abline(v=12.5,lwd=3.5,col="black")
}

if (plottype=="relative"){
    Future<-merge(FutureClim,Baseline,by.x=c("Group"),by.y=c("Group"))
    Future$Relative<-Future$Value.x-Future$Value.y
    FutureNewMean<-aggregate(Relative~Group,Future,mean)
    #TitleName<-paste(TitleName,paste("Change of ",ClimName,sep=""),sep=",")
    TitleName<-paste("Change of ",ClimName,sep="")
    #TitleName<-paste(TitleName,TitleName1)
    
    Ymin<-min(Future$Relative)
    Ymax<-max(Future$Relative)
    par(mar=c((length(futureclimatelist)-1)*1+4,4,2,2))
    boxplot(Relative~Group,data=Future,xlab="Month",ylab=paste(ClimVar," (",unit,")",sep=""),ylim=c(Ymin,Ymax),xaxt="n",main=TitleName,xaxt="n")
    axis(1,at=1:17,labels=c("1","2","3","4","5","6","7","8","9","10","11","12","JFM","AMJ","JAS","OND","Ya"),par(cex.axis=0.7))
    mtext(TitleName1, line=-1,outer=TRUE, side=1,cex=0.8)
    
    #boxplot(Relative~Group,data=Future,xlab="Month",ylab=paste(ClimVar," (",unit,")",sep=""),ylim=c(Ymin,Ymax),main=TitleName,xaxt="n",add=TRUE)
 lines(Relative~Group,data=subset(FutureNewMean,Group<=12),xlim=c(1,12),xaxt="n",type="o",pch=22,col="red")
 points(Relative~Group,data=subset(FutureNewMean,Group>12),xlim=c(13,17),xaxt="n",type="p",pch=22,col="red")
 abline(v=12.5,lwd=3.5,col="black")
}
dev.off()
}

args<-commandArgs(trailingOnly=TRUE)
fileDir<-args[1] #
ClimVar<-args[2]#
plottype<-args[3]
plotformat<-args[4]
output<-args[5]

faceit_ClimAnomaly(fileDir,ClimVar,plottype,plotformat,output)
