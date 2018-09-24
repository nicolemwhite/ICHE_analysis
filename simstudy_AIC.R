library(lme4)
library(ggplot2)
library(reshape2)
library(influence.ME)
library(data.table)


#simulatioin of poisson data
#monthly infection rates
Time<-seq(0,36,1)
IntStart<-12

#generate random effect
S<-5
alpha<-rnorm(S,0,sqrt(0.4))

data<-data.table(Site=rep(1:S,each=length(Time)),Time=rep(Time,S),Intervention=rep(ifelse(Time<IntStart,0,1),S),InterventionTime=rep(ifelse(Time<IntStart,0,Time-IntStart),S))

#add OBD
#for each site, start between 5000 and 20000 OBD at Time 0
OBD_init<-data.table(Site=1:S,OBD_0=round(runif(S,min=5000,max=20000)))
data<-merge(data,OBD_init,by='Site')
#assume constant increase of between -10 and 10% per month
data[,'OBD':=OBD_0+round(runif(.N,0.9*OBD_0,1.1*OBD_0))]

beta0<-log(2.5)   #log of baseline rate

#50% reduction in rates between 0 and 6 months InterventionTime
beta1<-log(-50/100+1)

#after 12 months intervention, start rising again to baseline level by end of study
beta2<-(beta0-(beta0+beta1))/12


data[,'InterventionTime12mths':=ifelse(InterventionTime>12,InterventionTime-12,0)]

data[,'lograte_E':=log(OBD/10000)+beta0+beta1*Intervention+beta2*InterventionTime12mths]
data[,'lograte':=lograte_E+alpha[Site]]
data[,'N':=rpois(.N,exp(lograte))]


glmm.fit.0<-glmer(N ~ (1 | Site),offset=log(OBD/10000),data=data, family=poisson('log'))

#Model 1: Binary intevention switch
glmm.fit.1<-glmer(N ~ Intervention + (1 | Site),offset=log(OBD/10000),data=data, family=poisson('log'))

#Model 2: Linear trend from start of intervention period
glmm.fit.2<-glmer(N ~ InterventionTime + (1 | Site),offset=log(OBD/10000),data=data, family=poisson('log'))

#Model 3: Step change + linear trend over intervention
glmm.fit.3<-glmer(N ~ Intervention + InterventionTime + (1 | Site),offset=log(OBD/10000),data=data, family=poisson('log'))


