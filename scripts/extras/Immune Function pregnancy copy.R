dropdir<-substr(getwd(),1,regexec("x",getwd()))

#Getstar
getstar<-function(p){
  c("","t","*","**","***")[apply(outer(p, c(0.1049,0.0549,0.0149,0.00149), "<="),1,sum)+1]
}

mydata<-read.csv(paste0(dropdir,"/My Files/Anthropology/Tsimane/Databases Newer/Merged/Biochem-Flow-Medical-Anthropometry-Reproduction-GPS-ADB.16.05.24-19Dec15PIDs.csv"),stringsAsFactors = FALSE)


mydata$agegroup<-NA
mydata$agegroup[mydata$Age>=0 & mydata$Age<6]<-0
mydata$agegroup[mydata$Age>=6 & mydata$Age<12]<-6
mydata$agegroup[mydata$Age>=12 & mydata$Age<18]<-12
mydata$agegroup[mydata$Age>=18 & mydata$Age<50]<-18
mydata$agegroup[mydata$Age>=50]<-50
mydata$LymphCnt<-mydata$WBC*mydata$Lymphocitos/100
mydata$nonNaiveCD4<-mydata$CD4Count-mydata$NaiveCD4Count
mydata$nonNaiveCD8<-mydata$CD8Count-mydata$NaiveCD8Count
mydata<-mydata[-which(mydata$Maturntrophls==0 | mydata$Lymphocitos<=1),]

mydata$Trimester2<-mydata$Trimester
mydata$Trimester2[mydata$Trimester %in% c("1","2","3")]<-"Preg"
mydata$nonNaiveCD4<-mydata$CD4Count-mydata$NaiveCD4Count
mydata$nonNaiveCD8<-mydata$CD8Count-mydata$NaiveCD8Count
# 
# vars<-c("IgE","IgG","iga","crp","WBC","CD4Count","Copro","Present.Hookworm")
# mydata$Year2<-as.POSIXlt(mydata$Date,format="%Y-%m-%d")$year+1900
# aggregate(mydata[,vars],by=list(Year=mydata$Year2),FUN=function(x) sum(!is.na(x)))
# mydata3<-mydata[!duplicated(mydata[,c("Year2","pid")]),]
# aggregate(mydata3[,vars],by=list(Year=mydata3$Year2),FUN=function(x) sum(!is.na(x)))


mydata2<-mydata[which(mydata$male==0 & mydata$Age>=18 & mydata$Age<40 & mydata$Trimester2!="Menopause"),]

mydata2$HasPreg<-ave(mydata2$Trimester2,mydata2$pid,FUN=function(x) "Preg" %in% x)
mydata2$HasCycling<-ave(mydata2$Trimester2,mydata2$pid,FUN=function(x) "Cycling" %in% x)
mydata2$Tcells<-mydata2$CD4Count+mydata2$CD8Count
#mydata3<-mydata2[mydata2$HasPreg=="TRUE" & mydata2$HasCycling=="TRUE",]

#paired analysis of same woman, only going to work for very broad categories...
library(reshape2)

v<-"CD4Count"


library(nlme)

#iga and igm went missing from this biochem file. Have to go back and track down...
vars<-c("IgE","IgG","WBC","LymphCnt","NeutroCnt","BasoCnt","EosinCnt","MonoCnt","crp","VSG","Tcells","CD4Count","NaiveCD4Count","nonNaiveCD4","CD8Count","NaiveCD8Count","nonNaiveCD8","NKCount" ,"BCount","CD48Ratio","SenescentCD4Count","SenescentCD8Count")
for(v in vars){
  #remove NAs from dataset, each variable
  useD<-na.omit(mydata2[,c("pid","Age","Trimester2",v)]) 
  #sample each variable
  depuse<-useD[,v]
  ifelse(min(depuse)==0,depuse2<-depuse+1,depuse2<-depuse)
  #model zero values with raw data
  m<-lme(depuse~Trimester2+Age,random=~1|pid,data=useD)
  #model gaussian models with logged values
  m2<-lme(I(log(depuse2))~Trimester2+Age,random=~1|pid,data=useD)
  fixed<-summary(m)$tTable
  fixed2<-summary(m2)$tTable
  row.names(fixed2)<-paste0(row.names(fixed2),".log")
  sample<-table(useD$Trimester2)
  if(v==vars[1]) out<-data.frame(as.list(c("dep",rownames(fixed),rownames(fixed2),names(sample))),stringsAsFactors = FALSE)
  out<-rbind(out,c(v,paste0(round(fixed[,1],3),getstar(fixed[,5])),paste0(round(fixed2[,1],3),getstar(fixed2[,5])),sample))
}

write.csv(out,"Pregnancy results.csv",row.names=FALSE)

#with Hookworm + Roundworm control
vars<-c("IgE","IgG","WBC","LymphCnt","NeutroCnt","BasoCnt","EosinCnt","MonoCnt","crp","VSG","CD4Count","NaiveCD4Count","nonNaiveCD4","CD8Count","NaiveCD8Count","nonNaiveCD8","NKCount" ,"BCount","CD48Ratio","SenescentCD4Count","SenescentCD8Count")
for(v in vars){
  useD<-na.omit(mydata2[,c("pid","Age","Trimester2","Present.Hookworm","Present.A.lumbricoides",v)])
  depuse<-useD[,v]
  ifelse(min(depuse)==0,depuse2<-depuse+1,depuse2<-depuse)
  m<-lme(depuse~Trimester2+Age+Present.Hookworm+Present.A.lumbricoides,random=~1|pid,data=useD)
  m2<-lme(I(log(depuse2))~Trimester2+Age+Present.Hookworm+Present.A.lumbricoides,random=~1|pid,data=useD)
  fixed<-summary(m)$tTable
  fixed2<-summary(m2)$tTable
  row.names(fixed2)<-paste0(row.names(fixed2),".log")
  sample<-table(useD$Trimester2)
  if(v==vars[1]) out<-data.frame(as.list(c("dep",rownames(fixed),rownames(fixed2),names(sample))),stringsAsFactors = FALSE)
  out<-rbind(out,c(v,paste0(round(fixed[,1],3),getstar(fixed[,5])),paste0(round(fixed2[,1],3),getstar(fixed2[,5])),sample))
}

write.csv(out,"Pregnancy results w worms.csv",row.names=FALSE)



#lm version
vars<-c("iga","igm","IgE","IgG","WBC","LymphCnt","NeutroCnt","BasoCnt","EosinCnt","MonoCnt","crp","VSG","CD4Count","NaiveCD4Count","nonNaiveCD4","CD8Count","NaiveCD8Count","nonNaiveCD8","NKCount" ,"BCount","CD48Ratio","SenescentCD4Count","SenescentCD8Count")
for(v in vars){
  useD<-na.omit(mydata2[,c("pid","Age","Trimester2",v)])
  depuse<-useD[,v]
  ifelse(min(depuse)==0,depuse2<-depuse+1,depuse2<-depuse)
  m<-lm(depuse~Trimester2+Age,data=useD)
  m2<-lm(I(log(depuse2))~Trimester2+Age,data=useD)
  fixed<-summary(m)$coef
  fixed2<-summary(m2)$coef
  row.names(fixed2)<-paste0(row.names(fixed2),".log")
  sample<-table(useD$Trimester2)
  if(v==vars[1]) out<-data.frame(as.list(c("dep",rownames(fixed),rownames(fixed2),names(sample))),stringsAsFactors = FALSE)
  out<-rbind(out,c(v,paste0(round(fixed[,1],3),getstar(fixed[,4])),paste0(round(fixed2[,1],3),getstar(fixed2[,4])),sample))
}

write.csv(out,"Pregnancy results lm.csv",row.names=FALSE)

#with Hookworm + Roundworm control
vars<-c("IgE","IgG","WBC","LymphCnt","NeutroCnt","BasoCnt","EosinCnt","MonoCnt","crp","VSG","CD4Count","NaiveCD4Count","nonNaiveCD4","CD8Count","NaiveCD8Count","nonNaiveCD8","NKCount" ,"BCount","CD48Ratio","SenescentCD4Count","SenescentCD8Count")
for(v in vars){
  useD<-na.omit(mydata2[,c("pid","Age","Trimester2","Present.Hookworm","Present.A.lumbricoides",v)])
  depuse<-useD[,v]
  ifelse(min(depuse)==0,depuse2<-depuse+1,depuse2<-depuse)
  m<-lm(depuse~Trimester2+Age+Present.Hookworm+Present.A.lumbricoides,data=useD)
  m2<-lm(I(log(depuse2))~Trimester2+Age+Present.Hookworm+Present.A.lumbricoides,data=useD)
  fixed<-summary(m)$coef
  fixed2<-summary(m2)$coef
  row.names(fixed2)<-paste0(row.names(fixed2),".log")
  sample<-table(useD$Trimester2)
  if(v==vars[1]) out<-data.frame(as.list(c("dep",rownames(fixed),rownames(fixed2),names(sample))),stringsAsFactors = FALSE)
  out<-rbind(out,c(v,paste0(round(fixed[,1],3),getstar(fixed[,4])),paste0(round(fixed2[,1],3),getstar(fixed2[,4])),sample))
}

write.csv(out,"Pregnancy results w worms lm.csv",row.names=FALSE)

library(beeswarm)
boxplot(NeutroCnt~Trimester2,data=mydata2,main="Neutrophils")
beeswarm(NeutroCnt~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)

boxplot(LymphCnt~Trimester2,data=mydata2,main="Lymphocytes")
beeswarm(LymphCnt~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)

boxplot(EosinCnt~Trimester2,data=mydata2,main="Eosinophils")
beeswarm(EosinCnt~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)

boxplot(NKCount~Trimester2,data=mydata2,main="Natural Killer")
beeswarm(NKCount~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)
boxplot(crp~Trimester2,data=mydata2,log="y",main="CRP")
beeswarm(crp~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)
boxplot(IgG~Trimester2,data=mydata2,main="IgG")
beeswarm(IgG~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)
boxplot(IgE~Trimester2,data=mydata2,main="IgE")
beeswarm(IgE~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)
boxplot(VSG~Trimester2,data=mydata2,main="ESR")
beeswarm(VSG~Trimester2,data=mydata2,method="swarm",corral="none",add=TRUE)


plot(VSG~PregDays,data=mydata2[mydata2$Trimester2=="Preg",])


plot(EosinCnt~PregDays,data=mydata2[mydata2$Trimester2=="Preg",])
plot(NeutroCnt~PregDays,data=mydata2[mydata2$Trimester2=="Preg",])
plot(LymphCnt~PregDays,data=mydata2[mydata2$Trimester2=="Preg",])
plot(MonoCnt~PregDays,data=mydata2[mydata2$Trimester2=="Preg",])

#check eosin interaction
mydata2$Hookworm<-as.numeric(mydata2$Present.Hookworm>0)
m1<-lm(Hb~Trimester2*Hookworm+HTO,data=mydata2)
m1<-gam(Hb~s(PregDays)+EosinCnt*Hookworm+HTO,data=mydata2)
m1<-lm(EosinCnt~Trimester2*Hookworm,data=mydata2)
m1<-lm(NeutroCnt~Trimester2*Hookworm,data=mydata2)

library(mgcv)
mydata2$MCHC<-mydata2$Hb/mydata2$HTO
mydata2<-mydata2[!(mydata2$MCHC>0.5),]
mydata2$EosinHTO<-mydata2$EosinCnt/mydata2$HTO


useD<-mydata2[mydata2$Trimester2=="Preg",]
mE<-lm(EosinCnt~PregDays*Hookworm,data=useD)
mVSG<-gam(VSG~s(PregDays)+Age,data=useD)
pVSG<-predict(mVSG, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

mNeutro<-gam(NeutroCnt~s(PregDays)+Age,data=useD)
pNeutro<-predict(mNeutro, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

mLymph<-gam(LymphCnt~s(PregDays)+Age,data=useD)
pLymph<-predict(mLymph, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

mEosin<-gam(EosinCnt~s(PregDays)+Age,data=useD)
pEosin<-predict(mEosin, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

mHb<-gam(Hb~s(PregDays)+Age,data=useD)
pHb<-predict(mHb, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

mHTO<-gam(HTO~s(PregDays)+Age,data=useD)
pHTO<-predict(mHTO, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)


mMCHC<-gam(MCHC~s(PregDays)+Age,data=useD)
pMCHC<-predict(mMCHC, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

mEoHTO<-gam(EosinHTO~s(PregDays)+Age,data=useD)
pEoHTO<-predict(mEoHTO, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

useDC<-mydata2[mydata2$Trimester2=="Cycling",]
mVSG2<-gam(VSG~Age,data=useDC)
pVSG2<-predict(mVSG2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mNeutro2<-gam(NeutroCnt~Age,data=useDC)
pNeutro2<-predict(mNeutro2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mLymph2<-gam(LymphCnt~Age,data=useDC)
pLymph2<-predict(mLymph2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mEosin2<-gam(EosinCnt~Age,data=useDC)
pEosin2<-predict(mEosin2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mHb2<-gam(Hb~Age,data=useDC)
pHb2<-predict(mHb2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mHTO2<-gam(HTO~Age,data=useDC)
pHTO2<-predict(mHTO2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mMCHC2<-gam(MCHC~Age,data=useDC)
pMCHC2<-predict(mMCHC2, newdata = data.frame(Age=27.3),se.fit=TRUE)

mEoHTO2<-gam(EosinHTO~Age,data=useDC)
pEoHTO2<-predict(mEoHTO2, newdata = data.frame(Age=27.3),se.fit=TRUE)

library(rethinking)
plotPreg<-function(m1,m2,col1,col2,...){
  pd<-seq(0,272,0.1)
  plot(m1$fit~pd,type="n",...)
  lines(rep(m2$fit,2)~c(0,272))
  shade(rbind(rep(m2$fit+1.96*m2$se.fit,2),rep(m2$fit-1.96*m2$se.fit,2)),c(0,272),col=col1)
  lines(m1$fit~pd)
  shade(rbind(m1$fit+1.96*m1$se.fit,m1$fit-1.96*m1$se.fit),pd, col = col2)
}

col1<-col.alpha("black",0.15)
col2<-col.alpha("red",0.15)
plotPreg(pHb,pHb2,col1,col2,ylab="Hb",ylim=c(10,14),xlab="Days Pregnant")
plotPreg(pHTO,pHTO2,col1,col2,ylab="Hb",xlab="Days Pregnant")

plotPreg(pMCHC,pMCHC2,col1,col2,ylab="Hb",xlab="Days Pregnant")
plotPreg(pEoHTO,pEoHTO2,col1,col2,ylab="Hb",ylim=c(0.02,0.06),xlab="Days Pregnant")

pHTO2.1<-predict(mHTO, newdata = data.frame(PregDays=0,Age=27.3), se.fit=TRUE)  #39.84743 , 0.7999422 

pHTO3<-predict(mHTO, newdata = data.frame(PregDays=140,Age=27.3), se.fit=TRUE)  #35.12573, se.fit=0.3481116
pHTO4<-predict(mHTO, newdata = data.frame(PregDays=210,Age=27.3), se.fit=TRUE)  #36.41509, se.fit=0.362189
pHTO5<-predict(mHTO, newdata = data.frame(PregDays=272,Age=27.3), se.fit=TRUE)  #37.19314, se.fit=0.676784

boxplot(Hb~Trimester,data=useD)
boxplot(EosinCnt~Trimester,data=useD)
abline(h=1.6)

tiff("Pregnancy Leukocytes.tif",width=234,height=170,units="mm",res=800,pointsize=18,compression="lzw")
par(mfrow=c(2,2),mar=c(4,4,1,1))
plotPreg(pVSG,pVSG2,col1,col2,ylab="ESR",ylim=c(30,52),xlab="Days Pregnant")
plotPreg(pEosin,pEosin2,col1,col2,ylab="Eosinophils",ylim=c(0.9,2.3),xlab="Days Pregnant")
plotPreg(pNeutro,pNeutro2,col1,col2,ylab="Neutrophils" ,ylim=c(4.5,6.5),xlab="Days Pregnant")
plotPreg(pLymph,pLymph2,col1,col2,ylab="Lymphocytes",ylim=c(2.2,3.8),xlab="Days Pregnant")

dev.off()


#beeswarm/box
library(beeswarm)
tiff("Pregnancy Boxplots.tif",width=234,height=170,units="mm",res=800,pointsize=18,compression="lzw")
par(mfrow=c(2,3),mar=c(4,4,0.5,1))
useD<-mydata2[mydata2$Trimester2 %in% c("Preg","Cycling"),]
boxplot(NKCount~Trimester2,data=useD,ylab="Natural Killer",col=col2)
beeswarm(NKCount~Trimester2,data=useD,method="swarm",corral="none",add=TRUE,pch=19)
boxplot(Tcells~Trimester2,data=useD,ylab="T cells",col=col2)
beeswarm(Tcells~Trimester2,data=useD,method="swarm",corral="none",add=TRUE,pch=19)
boxplot(BCount~Trimester2,data=useD,ylab="B cells",col=col2)
beeswarm(BCount~Trimester2,data=useD,method="swarm",corral="none",add=TRUE,pch=19)

boxplot(crp~Trimester2,data=useD,log="y",ylab="CRP",col=col2)
beeswarm(crp~Trimester2,data=useD,method="swarm",corral="none",add=TRUE,pch=19)
boxplot(IgG~Trimester2,data=useD,ylab="IgG",col=col2)
beeswarm(IgG~Trimester2,data=useD,method="swarm",corral="none",add=TRUE,pch=19)
boxplot(IgE~Trimester2,data=useD,ylab="IgE",col=col2)
beeswarm(IgE~Trimester2,data=useD,method="swarm",corral="none",add=TRUE,pch=19)
dev.off()





plot(orinaproteina~PregDays,data=mydata2[mydata2$Trimester2=="Preg",])
mydata2$PAenHGSys[mydata2$PAenHGSys>300]<-NA
mydata2$PAenHGDias[mydata2$PAenHGDias>200]<-NA
mydata2$PAenHGSys[mydata2$PAenHGSys<50]<-NA
mydata2$PAenHGDias[mydata2$PAenHGDias<25]<-NA
plot(PAenHGSys~PregDays,data=mydata2[mydata2$Trimester2=="Preg" & !grepl("Richard",mydata2$Medico),],main="Systolic")
abline(140,0)
plot(PAenHGDias~PregDays,data=mydata2[mydata2$Trimester2=="Preg" & mydata2$PAenHGDias<200 & !grepl("Richard",mydata2$Medico),],main="Diastolic")
abline(90,0)
boxplot(PAenHGSys~Trimester2,data=mydata2,main="Systolic")
boxplot(PAenHGDias~Trimester2,data=mydata2,main="Diastolic")

plot(orinaproteina~PAenHGSys,data=mydata2[mydata2$Trimester2=="Preg",])

library(mgcv)
m<-gam(PAenHGSys~Trimester*Present.Hookworm+Trimester*Present.A.lumbricoides+s(Age)+HTO+WBC+Trimester*VSG,data=mydata2[mydata2$Trimester2=="Preg" & !grepl("Richard",mydata2$Medico),])

m<-gam(PAenHGDias~Trimester*Present.Hookworm+Trimester*Present.A.lumbricoides+s(Age)+HTO+WBC+Trimester*VSG,data=mydata2[mydata2$Trimester2=="Preg" & !grepl("Richard",mydata2$Medico),])


m<-gam(PAenHGSys~Trimester+I(log(EosinCnt+1))+I(log(NeutroCnt+1))+I(log(LymphCnt+1))+s(Age)+BMI,data=mydata2[mydata2$Trimester2=="Preg" & !grepl("Richard",mydata2$Medico),])

m<-gam(PAenHGDias~Trimester+I(log(EosinCnt+1))+I(log(NeutroCnt+1))+I(log(LymphCnt+1))+s(Age)+BMI,data=mydata2[mydata2$Trimester2=="Preg" & !grepl("Richard",mydata2$Medico),])



m<-gam(PAenHGDias~Trimester*Present.Hookworm+Trimester*Present.A.lumbricoides+s(Age)+HTO,data=mydata2[mydata2$Trimester2=="Preg",])

m<-gam(PAenHGDias~PregDays*Present.Hookworm+PregDays*Present.A.lumbricoides+s(Age)+HTO,data=mydata2[mydata2$Trimester2=="Preg",])

m<-gam(PAenHGSys~Trimester*I(log(EosinCnt+1))+s(Age),data=mydata2[mydata2$Trimester2=="Preg",])

table(mydata2$orinaproteina,mydata2$Trimester)
3 of 71
#add cytokines
cyto<-read.csv(paste0(dropdir, "/My Files/Anthropology/Tsimane/Blood analyses/Melissa/Cytokines/CYTO_AllResultsStimSerum_Longform.1.20.14.19Dec15PIDs.csv"),stringsAsFactors = FALSE)

cytomerge<-mydata2[,c("pid","vuelta","Vuelta","NewVuelta","Date","Trimester2","Age","Present.Hookworm","Present.A.lumbricoides")]
cytomerge$vuelta[cytomerge$vuelta==12.2]<-13
cytomerge$Vuelta[is.na(cytomerge$Vuelta)]<-cytomerge$vuelta[is.na(cytomerge$Vuelta)]
cytomerge<-cytomerge[cytomerge$pid %in% cyto$pid & cytomerge$Vuelta %in% c(11,12,13),]
library(gdata)
cytomerge[duplicated2(cytomerge[,c("pid","Vuelta")]),]
cytomerge<-cytomerge[-which(cytomerge$pid==209085 & is.na(cytomerge$vuelta) | cytomerge$Vuelta==13),]
cytomerge$Vuelta[cytomerge$pid==248221 & cytomerge$NewVuelta==12.2]<-13

cyto<-merge(cyto,cytomerge)

vars<-unique(cyto$Cyto)
#stims<-unique(cyto$StimType)
stims<-"Serum"
for(v in cytokines){
  for(s in stims){
    useD<-na.omit(cyto[cyto$Cyto==v & cyto$StimType==s,c("pid","Result","lnResult","Trimester2","Age")])
    m<-lm(Result~Trimester2+Age,data=useD)
    m2<-lm(lnResult~Trimester2+Age,data=useD)
    fixed<-summary(m)$coef
    fixed2<-summary(m2)$coef
    row.names(fixed2)<-paste0(row.names(fixed2),".log")
    sample<-table(useD$Trimester2)
    if(v==vars[1] & s==stims[1]) out<-data.frame(as.list(c("dep","stim",rownames(fixed),rownames(fixed2))),stringsAsFactors = FALSE)
    out<-rbind(out,c(v,s,paste0(round(fixed[,1],3),getstar(fixed[,4])),paste0(round(fixed2[,1],3),getstar(fixed2[,4]))))
  }
}
#arghh! zero pregnant in the stims! Is that likely or missing data?  



write.csv(out,"Pregnancy results cytokines.csv",row.names=FALSE)
#only effect seems to be on higher IL-6

library(beeswarm)
boxplot(lnResult~Trimester2,data=cyto[cyto$StimType=="Serum" & cyto$Cyto=="IL-8",])
beeswarm(lnResult~Trimester2,data=cyto[cyto$StimType=="Serum" & cyto$Cyto=="IL-8",],add=TRUE)


#arthritis?
mydata2$ADBCat.Arthritis[mydata2$ADBCat.Arthritis>1]<-1
library(MCMCglmm)
useD<-na.omit(mydata2[,c("pid","Age","Trimester2","Present.Hookworm","Present.A.lumbricoides","ADBCat.Arthritis")])

prior1=list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
m<-MCMCglmm(ADBCat.Arthritis~Trimester2+Age+I(Age^2)+Present.Hookworm+Present.A.lumbricoides,random=~pid,data=useD,family="categorical",prior=prior1)
summary(m)
useD<-na.omit(mydata2[,c("pid","Age","Trimester2","ADBCat.Arthritis")])
m2<-MCMCglmm(ADBCat.Arthritis~Trimester2+Age+I(Age^2),random=~pid,data=useD,family="categorical",prior=prior1)

library(lmerTest)
useD<-na.omit(mydata2[,c("pid","Age","Trimester2","Present.Hookworm","Present.A.lumbricoides","ADBCat.Arthritis")])
m<-glmer(ADBCat.Arthritis~Trimester2+Age+I(Age^2)+Present.Hookworm+Present.A.lumbricoides+(1|pid),data=useD,family="binomial")

summary(m)
useD<-na.omit(mydata2[,c("pid","Age","Trimester2","ADBCat.Arthritis")])
m<-glmer(ADBCat.Arthritis~Trimester2+Age+I(Age^2)+(1|pid),data=useD,family="binomial")


useDH <- mydata2[mydata2$Trimester2=="Preg" & mydata2$Present.Hookworm=="1",]
useDO <- mydata2[mydata2$Trimester2=="Preg" & mydata2$Present.Hookworm=="0",]

mHbH<-gam(Hb~s(PregDays)+Age,data=useDH)
pHbH<-predict(mHbH, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)
mHbO<-gam(Hb~s(PregDays)+Age,data=useDO)
pHbO<-predict(mHbO, newdata = data.frame(PregDays=seq(0,272,0.1),Age=27.3),se.fit=TRUE)

plotPreg<-function(m1,m2,col1,col2,...){
  pd<-seq(0,272,0.1)
  plot(m1$fit~pd,type="n",...)
  lines(m2$fit~pd)
  shade(rbind(m2$fit+1.96*m2$se.fit,m2$fit-1.96*m2$se.fit),pd, col = col1)
  lines(m1$fit~pd)
  shade(rbind(m1$fit+1.96*m1$se.fit,m1$fit-1.96*m1$se.fit),pd, col = col2)
}
col1<-col.alpha("black",0.15)
col2<-col.alpha("red",0.15)
plotPreg(pHbH,pHbO,col1,col2,ylab="Hemoglobin",ylim=c(10,14),xlab="Days Pregnant")



anemDat<- na.omit(mydata2[,c("Hb","Trimester","Present.Hookworm")])
anemDat$anemic[which(anemDat$Trimester=="Cycling")] <- as.numeric(anemDat$Hb<12)[anemDat$Trimester=="Cycling"]
anemDat$anemic[which(anemDat$Trimester!="Cycling")] <- as.numeric(anemDat$Hb<11)[anemDat$Trimester!="Cycling"]



anemHPreg<-aggregate(anemic~Trimester, data=anemDat[which(anemDat$Trimester!="Cycling" & anemDat$Trimester!="Lactating" & anemDat$Present.Hookworm>0),], mean)

anemHPreg<-aggregate(anemic~Trimester+Hookworm, data=anemDat, mean)

anemOPreg<-aggregate(anemic~Trimester, data=anemDat[which(anemDat$Trimester=="1" |anemDat$Trimester=="2" | anemDat$Trimester=="3" & anemDat$Trimester!="Lactating"& anemDat$Present.Hookworm<1),], mean)
anemHCycle<-mean((anemDat$Hb<12)[anemDat$Trimester=="Cycling" & anemDat$Present.Hookworm=="1"])
anemOCycle<-mean((anemDat$Hb<12)[anemDat$Trimester=="Cycling" & anemDat$Present.Hookworm=="0"])

table(anemHPreg[,2],anemOPreg[,2])#
