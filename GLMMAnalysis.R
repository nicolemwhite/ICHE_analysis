library(lme4)
library(ggplot2)
library(reshape2)
library(influence.ME)
library(data.table)

#setwd('U:/Research/Projects/ihbi/aushsi/hospital_cleaning/Trial/Data analysis/Audit')
data = readRDS("D:/auditdata_all.RDS")

#fit different models to all data
#need to scale TimeFromStart_Int for model fitting
data$TimeFromStart_Int_scaled<-data$TimeFromStart_Int/10
data$TimeFromStart_Control_scaled<-data$TimeFromStart_Control/10

#models are Binomial GLMM with random intercept for each site
#Null
glmm.fit.0<-glmer(cbind(Clean_N, NotClean_N) ~ (1 | Site),data=data, family=binomial('logit'))


#Model 1: Binary intevention switch
glmm.fit.1<-glmer(cbind(Clean_N, NotClean_N) ~ Period + (1 | Site),data=data, family=binomial('logit'))

#Model 2: Linear trend from start of intervention period
glmm.fit.2<-glmer(cbind(Clean_N, NotClean_N) ~ TimeFromStart_Int_scaled + (1 | Site),data=data, family=binomial('logit'))

#Model 3: Step change + linear trend over intervention
glmm.fit.3<-glmer(cbind(Clean_N, NotClean_N) ~ Period + TimeFromStart_Int_scaled + (1 | Site),data=data, family=binomial('logit'))

############################################################
#glmm residuals
data$res<-residuals(glmm.fit.3,'deviance')
data$fit<-predict(glmm.fit.3,type='response')
ggplot(data,aes(x=fit,y=res,group=Site))+geom_point()+facet_wrap(~Site,scales='free')


#############################################################
#comparison with GLM (no random effects)
#Null
glm.fit.0<-glm(cbind(Clean_N, NotClean_N) ~ 1,data=data, family=binomial('logit'))

#Model 1: Binary intevention switch
glm.fit.1<-glm(cbind(Clean_N, NotClean_N) ~ Period,data=data, family=binomial('logit'))

#Model 2: Linear trend from start of intervention period
glm.fit.2<-glm(cbind(Clean_N, NotClean_N) ~ TimeFromStart_Int_scaled,data=data, family=binomial('logit'))

#Model 3: Step change + linear trend over intervention
glm.fit.3<-glm(cbind(Clean_N, NotClean_N) ~ Period + TimeFromStart_Int_scaled,data=data, family=binomial('logit'))


###########################################################
#Compared observed with predicted
numSims<-100

sim.p<-matrix(0,12,numSims)
for (t in 1:numSims){
  sim.output<-simulate(glmm.fit.3)
  sim.DF<-data.frame(data[,c('Site','Period','Audit_label','Audit_no')],sim.output[[1]])
  sim.data<-aggregate(Clean_N/(Clean_N+NotClean_N)~Audit_label,data=sim.DF,mean)
  
  sim.p[,t]<-sim.data[,2]
  #sim.cor[,t]<-compute_cor(data=sim.DF)
}
colnames(sim.p)<-colnames(sim.iqr)<-paste0('Sim',1:numSims)

#join simulations with observed data summareis
obs.data<-aggregate(Clean_N/(Clean_N+NotClean_N)~Audit_label,data=data,mean)
obs.iqr<-aggregate(Clean_N/(Clean_N+NotClean_N)~Period,data=sim.DF,IQR)

colnames(obs.data)[2]<-'Clean_p'

ObsSim_data<-data.frame(obs.data,sim.p)

#reorder audit label
ObsSim_data$Audit_label<-factor(ObsSim_data$Audit_label,levels=c(paste('Control',1:2,sep='_'),paste('Intervention',1:10,sep='_')))

ObsSim_data_long<-melt(ObsSim_data,id.vars=c('Audit_label','Clean_p'),variable.name='Sim',value.name='Clean_pSim')


g2<-ggplot(ObsSim_data_long,aes(x=Audit_label,y=Clean_pSim))+
  geom_boxplot(colour='dark blue',alpha=.2,size=1)+
  geom_point(data=obs.data,aes(x=Audit_label,y=Clean_p),colour='black',size=2)+
  g.theme + theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylab('Cleaning Success')+xlab('')+
  ggtitle(paste0('Binary + Linear (AIC = ',aic,')'))
jpeg('Results/Figures/ICHE paper/WhiteFigure5c.jpg', width=7, height=4, units='in', res=300)
print(g2)
dev.off()


g3<-ggplot(ObsSim_data_long,aes(x=Audit_label,y=Clean_p-Clean_pSim))+
  geom_boxplot(colour='dark blue',alpha=.2,size=1)+
  g.theme + theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylab('Observed-Predicted: Average Cleaning Success')+xlab('')




#############################################################
#Sensitivity analysis
sens_glmm<-influence(glmm.fit.3,"Site")

#Cook's D
cooksd_glmm<-cooks.distance(sens_glmm,parameters=1:2)

#pchange
pchange_glmm<-data.frame(Site=1:11,pchange(sens_glmm))
pchange_glmm_long<-melt(pchange_glmm,id.vars='Site',variable.name='Effect',value.name='pchange')


dfbetas_glmm<-data.frame(Site=1:11,dfbetas(sens_glmm))
colnames(dfbetas_glmm)<-c('Site','Intercept','Intervention','IntTime')
dfbetas_glmm_long<-data.table(melt(dfbetas_glmm,id.vars='Site',value.name='DFBETA'))

#plot changes in estimated effects
alt.est_glmm<-melt(sens_glmm$alt.fixed,varnames=c('Site','Effect'),value.name='Estimate')
alt.se_glmm<-melt(sens_glmm$alt.se,varnames=c('Site','Effect'),value.name='SE')

alt_glmm<-merge(alt.est_glmm,alt.se_glmm,by=c('Site','Effect'))

g3<-ggplot(subset(alt_glmm,Effect!='(Intercept)'),aes(x=factor(Site),y=Estimate,group=Effect,ymin=Estimate-1.96*SE,ymax=Estimate+1.96*SE))+geom_point()+geom_errorbar()+facet_wrap(~Effect,scales='free')
g3+
  xlab('Site')+
  g.theme+theme(strip.background = element_rect(fill='white'))

g4<-ggplot(subset(pchange_glmm_long,Effect!='X.Intercept.'),aes(x=Site,y=pchange,group=Effect))+geom_point()

