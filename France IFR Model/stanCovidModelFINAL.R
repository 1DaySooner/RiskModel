library(rstan)
options(mc.cores = parallel::detectCores()) #Added to make this run faster
#Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7') #Added for David - Remove for others.
library(ReIns)
setwd("France IFR Model") #Added Note: Change directory to the one containing these files. Will error on rerun.
AgeSexPyramid<-read.csv2("AgeSexPyramidFrance.csv")
dailyHospCounts<-read.csv("dailyHospCounts.csv")
countData<-read.csv("TotalCountData.csv")

## Delay distributions
meanDelaysICUByAge<-rep(1/0.6574718,8)
propDeathPropExp<-c(rep(0.11,6),0.13,0.18)
delayDeathExpPar<-rep(1/0.67,8)
meanDelaysDeathByAge_LNorm<-c(rep(21.19,6),12.57,10.5)
medianDelaysDeathByAge_LNorm<-c(rep(12.4,6),8.52,7.47)
maxDelay=60

delayFunCumExp <- function(x, par){pexp(x, par)}
delayFunCumlnormTruncExp <- function(x, pars){
  ExpPart=pexp(x,pars[4])
  mu <- log(pars[2])
  sigma <- sqrt(2*(log(pars[1]) - mu))
  logNormPart=ptlnorm(x, mu, sigma,endpoint=pars[3])
  overall<-pars[5]*ExpPart+(1-pars[5])*logNormPart
  }
AdjFunc<-function(datIn,delay_fun,pars){
  cumulative_known_t <- 0
  for(ii in 1:length(datIn)){
      known_i <- 0 # number of cases with known outcome at time ii
      for(jj in 0:(ii - 1)){
        known_jj <- datIn[ii - jj]*(delay_fun(jj,pars) - delay_fun(jj - 1,pars))
        known_i <- known_i + known_jj
      }
      cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
    }
  propCasesWithOutcome<-cumulative_known_t/sum(datIn)
    return(propCasesWithOutcome)
}

## Age groupings
agesLower<-c(0,seq(20,80,10))
agesUpper<-c(19,seq(29,79,10),100)
NGroups=length(agesUpper)

# EPHADdata (retirement communities)
Tot<-727930
pctF<-0.736
Females<-rev(c(11.3+28.3+28.5+16.4,7.5+3.4,4.6))/100 #<70, 70-80,80+
Males<-rev(c(5.5+18.4+23.2+18.2,11.8+8.0,14.9))/100
MalesEHPAD<-FemalesEHPAD<-rep(0,NGroups)
FemalesEHPAD[6:8]<-Females*pctF*Tot
MalesEHPAD[6:8]<-Males*(1-pctF)*Tot

## Population pyramid
agePopGroup<-cumsum(AgeSexPyramid[,4])[agesUpper+1]-cumsum(AgeSexPyramid[,4])[agesLower+1]
agePopGroupM<-cumsum(AgeSexPyramid[,2])[agesUpper+1]-cumsum(AgeSexPyramid[,2])[agesLower+1]
agePopGroupF<-cumsum(AgeSexPyramid[,3])[agesUpper+1]-cumsum(AgeSexPyramid[,3])[agesLower+1]
agePopGroupM<-round(agePopGroupM-MalesEHPAD) ## Remove EHPAD individuals
agePopGroupF<-round(agePopGroupF-FemalesEHPAD) ## Remove EHPAD individuals

## Proportion of cases with outcome
propOutcomeICUByAge<-rep(0,NGroups)
propOutcomeDeathByAge<-rep(NaN,NGroups)
for(i in 1:NGroups){
  propOutcomeICUByAge[i]<-AdjFunc(dailyHospCounts[,i+1],delayFunCumExp,1/meanDelaysICUByAge[i])
  propOutcomeDeathByAge[i]<-AdjFunc(dailyHospCounts[,i+1],delayFunCumlnormTruncExp,c(meanDelaysDeathByAge_LNorm[i],medianDelaysDeathByAge_LNorm[i],maxDelay,propDeathPropExp[i],delayDeathExpPar[i]))
}

## Princess diamond data
PDF<-c(5,15,11,6,35,104,119,18) ## Number tested positive by age group - F (634 with known distribution)
PDM<-c(1,16,23,21,25,74,123,36+2) ## Number tested positive by age group - M (634 with known distribution)
AdditionalPositives<-712-sum(PDF)-sum(PDM)
wts<-AdditionalPositives*(cbind(PDF,PDM)/(sum(PDF)+sum(PDM)))
PDF.all<-wts[,1]+PDF ##Overall positive F
PDM.all<-wts[,2]+PDM ##Overall positive M
TotObservedPDDeaths=18

##Average contacts per day per age group
contactsByAgePreLockdown<-c(1.1139124,1.0324866, 0.9494718, 1.0871887, 0.9744327, 0.6554914, 0.6680607, 0.6431009)
contactsByAgePostLockdown<-c(0.7165773, 1.2285287, 0.9672835, 1.1851959, 0.9801547, 0.7740517, 0.8237709, 0.6586050)
contactsByAgePreLockdown<-contactsByAgePreLockdown/weighted.mean(contactsByAgePreLockdown,agePopGroupM+agePopGroupF)
contactsByAgePostLockdown<-contactsByAgePostLockdown/weighted.mean(contactsByAgePostLockdown,agePopGroupM+agePopGroupF)
contactsByAgeBase<-(contactsByAgePostLockdown+contactsByAgePreLockdown)/2

##Run stan
data <- list( NGroups=NGroups,
  				    hospByGroupM=countData$hospByGroupM,
  				    ICUByGroupM=countData$ICUByGroupM,
  				    deathsByGroupM=countData$deathsByGroupM,
 				      popByGroupM=agePopGroupM,
              hospByGroupF=countData$hospByGroupF,
              ICUByGroupF=countData$ICUByGroupF,
              deathsByGroupF=countData$deathsByGroupF,
              popByGroupF=agePopGroupF,
              popByGroup_activeM=PDM.all,
              popByGroup_activeF=PDF.all,
  				    propCasesWithOutcomeDeath=propOutcomeDeathByAge,
  				    propCasesWithOutcomeICU=propOutcomeICUByAge,
              relprobInfection=contactsByAgeBase,
              TotObservedPDDeaths=TotObservedPDDeaths
            )

fit <- stan(file="COVIDstanCodeFINAL.stan", #Note: Changed from original because everything is in 1 directory.
               data=data,
               iter=100000,
               chains=4,
               control = list(max_treedepth = 10,adapt_delta=0.80)
               )

chains<- extract(fit,inc_warmup=FALSE)

###Estimate overall probabilities
wts.infecBase<-cbind(agePopGroupF*contactsByAgeBase,agePopGroupM*contactsByAgeBase)
wts.infec<-wts.infecBase/sum(wts.infecBase)
wts.hosp<-cbind(countData$hospByGroupF,countData$hospByGroupM)
wts.hosp<-wts.hosp/sum(wts.hosp)

### Probability of hosp overall
fitprobHospOverallM<-t(apply(chains$probHospByAgeM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitprobHospOverallF<-t(apply(chains$probHospByAgeF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))

### Probability of ICU overall
fitprobICUOverallM<-t(apply(chains$probICUByAgeM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitprobICUOverallF<-t(apply(chains$probICUByAgeF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))

### Probability of Death overall
fitprobDeathOverallM<-t(apply(chains$probDeathByAgeM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitprobDeathOverallF<-t(apply(chains$probDeathByAgeF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))


### Probability of hosp given infection
fitprobHospM<-t(apply(chains$probHospM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitprobHospF<-t(apply(chains$probHospF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitRELprobHosp<-t(apply(chains$probHospM/chains$probHospF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))

tmpF<-(sweep(chains$probHospF,2,wts.infec[,1]/(wts.infec[,1]+wts.infec[,2]),"*"))
tmpM<-(sweep(chains$probHospM,2,wts.infec[,2]/(wts.infec[,1]+wts.infec[,2]),"*"))
tmp<-tmpF+tmpM
fitprobHosp<-t(apply(tmp,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))

tmpF<-(sweep(chains$probHospF,2,wts.infec[,1],"*"))
tmpM<-(sweep(chains$probHospM,2,wts.infec[,2],"*"))
tmp<-rowSums(tmpM)+rowSums(tmpF)
meanHospGivenInfected<-quantile(tmp,probs=c(0.025,0.25,0.5,0.75,0.975))
meanHospGivenInfectedF<-quantile(rowSums(sweep(chains$probHospF,2,wts.infec[,1]/sum(wts.infec[,1]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975))
meanHospGivenInfectedM<-quantile(rowSums(sweep(chains$probHospM,2,wts.infec[,2]/sum(wts.infec[,2]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975))


## Probability of ICU given infection
fitprobICUByAgeGivenInfectedM<-t(apply(chains$probICUByAgeGivenInfectionM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitprobICUByAgeGivenInfectedF<-t(apply(chains$probICUByAgeGivenInfectionF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitRELprobICUByAgeGivenInfected<-t(apply(chains$probICUByAgeGivenInfectionM/chains$probICUByAgeGivenInfectionF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
tmpF<-(sweep(chains$probICUByAgeGivenInfectionF,2,wts.infec[,1]/(wts.infec[,1]+wts.infec[,2]),"*"))
tmpM<-(sweep(chains$probICUByAgeGivenInfectionM,2,wts.infec[,2]/(wts.infec[,1]+wts.infec[,2]),"*"))
tmp<-tmpF+tmpM
fitprobICUByAgeGivenInfected<-t(apply(tmp,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))

tmpF<-(sweep(chains$probICUByAgeGivenInfectionF,2,wts.infec[,1],"*"))
tmpM<-(sweep(chains$probICUByAgeGivenInfectionM,2,wts.infec[,2],"*"))
tmp<-rowSums(tmpM)+rowSums(tmpF)
meanICUGivenInfected<-quantile(tmp,probs=c(0.025,0.25,0.5,0.75,0.975))
meanICUGivenInfectedF<-quantile(rowSums(sweep(chains$probICUByAgeGivenInfectionF,2,wts.infec[,1]/sum(wts.infec[,1]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975))
meanICUGivenInfectedM<-quantile(rowSums(sweep(chains$probICUByAgeGivenInfectionM,2,wts.infec[,2]/sum(wts.infec[,2]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975))

### Probability of Death given infection
fitprobDeathByAgeGivenInfectedM<-t(apply(chains$probDeathByAgeGivenInfectionM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitprobDeathByAgeGivenInfectedF<-t(apply(chains$probDeathByAgeGivenInfectionF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
fitRELprobDeathByAgeGivenInfected<-t(apply(chains$probDeathByAgeGivenInfectionM/chains$probDeathByAgeGivenInfectionF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
tmpF<-(sweep(chains$probDeathByAgeGivenInfectionF,2,wts.infec[,1]/(wts.infec[,1]+wts.infec[,2]),"*"))
tmpM<-(sweep(chains$probDeathByAgeGivenInfectionM,2,wts.infec[,2]/(wts.infec[,1]+wts.infec[,2]),"*"))
tmp<-tmpF+tmpM
fitprobDeathByAgeGivenInfected<-t(apply(tmp,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))

tmpF<-(sweep(chains$probDeathByAgeGivenInfectionF,2,wts.infec[,1],"*"))
tmpM<-(sweep(chains$probDeathByAgeGivenInfectionM,2,wts.infec[,2],"*"))
tmp<-rowSums(tmpM)+rowSums(tmpF)
meanDeathGivenInfected<-quantile(tmp,probs=c(0.025,0.25,0.5,0.75,0.975))
meanDeathGivenInfectedF<-quantile(rowSums(sweep(chains$probDeathByAgeGivenInfectionF,2,wts.infec[,1]/sum(wts.infec[,1]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975))
meanDeathGivenInfectedM<-quantile(rowSums(sweep(chains$probDeathByAgeGivenInfectionM,2,wts.infec[,2]/sum(wts.infec[,2]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975))


# Added more granular extract:

Male_probDeath<-t(apply(chains$probDeathByAgeGivenInfectionM,2,quantile,probs=(0:100)/100,na.rm=T))
Female_probDeath<-t(apply(chains$probDeathByAgeGivenInfectionF,2,quantile,probs=(0:100)/100,na.rm=T))

Male_probHosp<-t(apply(chains$probHospM,2,quantile,probs=(0:100)/100,na.rm=T))
Female_probHosp<-t(apply(chains$probHospF,2,quantile,probs=(0:100)/100,na.rm=T))

Male_probICU<-t(apply(chains$probICUByAgeGivenInfectionM,2,quantile,probs=(0:100)/100,na.rm=T))
Female_probICU<-t(apply(chains$probICUByAgeGivenInfectionF,2,quantile,probs=(0:100)/100,na.rm=T))


rowlabel = list()
for (i in 1:length(agesLower)){
  rowlabel[i] = paste(toString(agesLower[i]), "to", toString(agesUpper[i]))
}

rownames(Male_probDeath) <- rowlabel
rownames(Female_probDeath) <- rowlabel
rownames(Male_probHosp) <- rowlabel
rownames(Female_probHosp) <- rowlabel
rownames(Male_probICU) <- rowlabel
rownames(Female_probICU) <- rowlabel


write.csv(Male_probDeath,"../France_Male_p_death_by_age_range.csv")
write.csv(Female_probDeath,"../France_Female_p_death_by_age_range.csv")
write.csv(Male_probHosp,"../France_Male_p_hospitalization_by_age_range.csv")
write.csv(Female_probHosp,"../France_Female_p_hospitalization_by_age_range.csv")
write.csv(Male_probICU,"../France_Male_p_death_by_age_range.csv")
write.csv(Female_probICU,"../France_Female_p_death_by_age_range.csv")



# ### Probability of ICU given hosp
# fitprobICUByAgeGivenHospM<-t(apply(chains$probICUM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# fitprobICUByAgeGivenHospF<-t(apply(chains$probICUF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# fitRELprobICUByAgeGivenHosp<-t(apply(chains$probICUM/chains$probICUF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# tmpF<-(sweep(chains$probICUF,2,wts.hosp[,1]/(wts.hosp[,1]+wts.hosp[,2]),"*"))
# tmpM<-(sweep(chains$probICUM,2,wts.hosp[,2]/(wts.hosp[,1]+wts.hosp[,2]),"*"))
# tmp<-tmpF+tmpM
# fitprobICUByAgeGivenHosp<-t(apply(tmp,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# tmpF<-(sweep(chains$probICUF,2,wts.hosp[,1],"*"))
# tmpM<-(sweep(chains$probICUM,2,wts.hosp[,2],"*"))
# tmp<-rowSums(tmpM)+rowSums(tmpF)
# meanICUGivenHosp<-quantile(tmp,probs=c(0.025,0.25,0.5,0.75,0.975))
# meanICUGivenHospF<-quantile(rowSums(sweep(chains$probICUF,2,wts.hosp[,1]/sum(wts.hosp[,1]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975)) 
# meanICUGivenHospM<-quantile(rowSums(sweep(chains$probICUM,2,wts.hosp[,2]/sum(wts.hosp[,2]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975)) 
# 
# ### Probability of Death given hosp
# fitprobDeathByAgeGivenHospM<-t(apply(chains$probDeathM,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# fitprobDeathByAgeGivenHospF<-t(apply(chains$probDeathF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# fitRELprobDeathByAgeGivenHosp<-t(apply(chains$probDeathM/chains$probDeathF,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# tmpF<-(sweep(chains$probDeathF,2,wts.hosp[,1]/(wts.hosp[,1]+wts.hosp[,2]),"*"))
# tmpM<-(sweep(chains$probDeathM,2,wts.hosp[,2]/(wts.hosp[,1]+wts.hosp[,2]),"*"))
# tmp<-tmpF+tmpM
# fitprobDeathByAgeGivenHosp<-t(apply(tmp,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T))
# 
# tmpF<-(sweep(chains$probDeathF,2,wts.hosp[,1],"*"))
# tmpM<-(sweep(chains$probDeathM,2,wts.hosp[,2],"*"))
# tmp<-rowSums(tmpM)+rowSums(tmpF)
# meanDeathGivenHosp<-quantile(tmp,probs=c(0.025,0.25,0.5,0.75,0.975)) 
# meanDeathGivenHospF<-quantile(rowSums(sweep(chains$probDeathF,2,wts.hosp[,1]/sum(wts.hosp[,1]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975)) 
# meanDeathGivenHospM<-quantile(rowSums(sweep(chains$probDeathM,2,wts.hosp[,2]/sum(wts.hosp[,2]),"*")),probs=c(0.025,0.25,0.5,0.75,0.975)) 
# 
# 
# 
# ### Parameter estimates (Figure 2)
# xpol<-c(0,0,9.5,9.5)
# ypol_meanHosp<-c(meanHospGivenInfected[5],meanHospGivenInfected[1],meanHospGivenInfected[1],meanHospGivenInfected[5])
# ypol_meanICU<-c(meanICUGivenHosp[5],meanICUGivenHosp[1],meanICUGivenHosp[1],meanICUGivenHosp[5])
# ypol_meanDeathGivenHosp<-c(meanDeathGivenHosp[5],meanDeathGivenHosp[1],meanDeathGivenHosp[1],meanDeathGivenHosp[5])
# ypol_meanDeathGivenInfected<-c(meanDeathGivenInfected[5],meanDeathGivenInfected[1],meanDeathGivenInfected[1],meanDeathGivenInfected[5])
# 
# cols=c("light blue","salmon")
# colsB=c("dark blue","brown")
# meanCol="grey"
# 
# quartz(width=5,height=5)
# par(lend=1)
# par(mfrow=c(2,2))
# par(mar=c(3,3,1,1))
# 
# plot(1,1,type="n",axes=F,xlab="",ylab="",xlim=c(0.4,9),ylim=c(0,0.7))
# polygon(xpol,ypol_meanHosp,col=rgb(t(col2rgb(meanCol)/255),alpha=0.5),border=NA)
# lines(c(0,9.5),c(meanHospGivenInfected[3],meanHospGivenInfected[3]),col=1)
# boxplot(t(fitprobHospM),outline=F,col=cols[1],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[1],boxwex=0.4,add=T)
# boxplot(t(fitprobHospF),outline=F,col=cols[2],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[2],boxwex=0.4,add=T,at=(1:8)+0.4)
# axis(2,cex.axis=1,las=1,hadj=0.7,tck=-0.04)
# legend("topleft",c("Male","Female"),border=colsB,fill=c(cols),bty="n",cex=1)
# 
# plot(1,1,type="n",axes=F,xlab="",ylab="",xlim=c(0.4,9),ylim=c(0,0.6))
# polygon(xpol,ypol_meanICU,col=rgb(t(col2rgb(meanCol)/255),alpha=0.5),border=NA)
# lines(c(0,9.5),c(meanICUGivenHosp[3],meanICUGivenHosp[3]),col=1)
# boxplot(t(fitprobICUByAgeGivenHospM),outline=F,col=cols[1],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[1],boxwex=0.4,add=T)
# boxplot(t(fitprobICUByAgeGivenHospF),outline=F,col=cols[2],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[2],boxwex=0.4,add=T,at=(1:8)+0.4)
# axis(2,cex.axis=1,las=1,hadj=0.7,tck=-0.04)
# legend("topleft",c("Male","Female"),border=colsB,fill=c(cols),bty="n",cex=1)
# 
# plot(1,1,type="n",axes=F,xlab="",ylab="",xlim=c(0.4,9),ylim=c(0,0.6))
# polygon(xpol,ypol_meanDeathGivenHosp,col=rgb(t(col2rgb(meanCol)/255),alpha=0.5),border=NA)
# lines(c(0,9.5),c(meanDeathGivenHosp[3],meanDeathGivenHosp[3]),col=1)
# boxplot(t(fitprobDeathByAgeGivenHospM),outline=F,col=cols[1],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[1],boxwex=0.4,add=T)
# boxplot(t(fitprobDeathByAgeGivenHospF),outline=F,col=cols[2],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[2],boxwex=0.4,add=T,at=(1:8)+0.4)
# axis(2,cex.axis=1,las=1,hadj=0.7,tck=-0.04)
# legend("topleft",c("Male","Female"),border=colsB,fill=c(cols),bty="n",cex=1)
# 
# plot(1,1,type="n",axes=F,xlab="",ylab="",xlim=c(0.4,9),ylim=c(0,0.3))
# polygon(xpol,ypol_meanDeathGivenInfected,col=rgb(t(col2rgb(meanCol)/255),alpha=0.5),border=NA)
# lines(c(0,9.5),c(meanDeathGivenInfected[3],meanDeathGivenInfected[3]),col=1)
# boxplot(t(fitprobDeathByAgeGivenInfectedM),outline=F,col=cols[1],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[1],boxwex=0.4,add=T)
# boxplot(t(fitprobDeathByAgeGivenInfectedF),outline=F,col=cols[2],lty=1,range=1000,frame.plot=F,xaxt="n",ylab="",xlab="",lwd=0.75,yaxt="n",border=colsB[2],boxwex=0.4,add=T,at=(1:8)+0.4)
# axis(2,cex.axis=1,las=1,hadj=0.7,tck=-0.04)
# legend("topleft",c("Male","Female"),border=colsB,fill=c(cols),bty="n",cex=1)

# Added to extract relevnat data for IFR Paper:

