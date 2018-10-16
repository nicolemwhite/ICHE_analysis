library(lme4)
library(ggplot2)
library(reshape2)
library(influence.ME)
library(data.table)

data = readRDS("auditdata_all.RDS")

#define general theme elements for plotting (ggplot2)
g.theme<-theme_bw()+theme(text=element_text(size=12),legend.position = 'bottom',legend.direction='horizontal')


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
#Compare models using AIC
AIC.1<-extractAIC(glmm.fit.1)
AIC.2<-extractAIC(glmm.fit.2)
AIC.3<-extractAIC(glmm.fit.3)
AIC.tab<-rbind(AIC.1,AIC.2,AIC.3)

############################################################
#For best fitting model based on AIC, review deviance residuals for patterns/unexpected trends
data$res<-residuals(glmm.fit.3,'deviance')
data$fit<-predict(glmm.fit.3,type='response')

#residuals versus fitted values
ggplot(data,aes(x=fit,y=res,group=Site))+geom_point()+facet_wrap(~Site,scales='free')

#boxplots
ggplot(data,aes(x=factor(Site),y=res))+xlab('Site')+ylab('Deviance residual')+geom_boxplot()+geom_jitter(width=0.1)

############################################################
#Compared observed with predicted cleaning performance using simulation
#define number of simulations
numSims<-100

#matrix of predicted %FTP clean by audit (rows) and simulations (columns)
sim.p<-matrix(0,12,numSims)
for (t in 1:numSims){
  sim.output<-simulate(glmm.fit.3)
  sim.DF<-data.frame(data[,c('Site','Period','Audit_label','Audit_no')],sim.output[[1]])
  sim.data<-aggregate(Clean_N/(Clean_N+NotClean_N)~Audit_label,data=sim.DF,mean)
  sim.p[,t]<-sim.data[,2]
}
#assign column names
colnames(sim.p)<-paste0('Sim',1:numSims)

#join simulations with observed data summaries
obs.data<-aggregate(Clean_N/(Clean_N+NotClean_N)~Audit_label,data=data,mean)

colnames(obs.data)[2]<-'Clean_p'
ObsSim_data<-data.frame(obs.data,sim.p)

#reorder audit label for plotting
ObsSim_data$Audit_label<-factor(ObsSim_data$Audit_label,levels=c(paste('Control',1:2,sep='_'),paste('Intervention',1:10,sep='_')))

ObsSim_data_long<-melt(ObsSim_data,id.vars=c('Audit_label','Clean_p'),variable.name='Sim',value.name='Clean_pSim')

#Plot (Figure 2, Model 3 in paper)
ggplot(ObsSim_data_long,aes(x=Audit_label,y=Clean_pSim))+
  geom_boxplot(colour='dark blue',alpha=.2,size=1)+
  geom_point(data=obs.data,aes(x=Audit_label,y=Clean_p),colour='black',size=2)+
  g.theme + theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylab('Cleaning Success')+xlab('')+
  ggtitle(paste0('Binary + Linear (AIC = ',aic,')'))


#similar plot: observed-predicted so centered around zero
ggplot(ObsSim_data_long,aes(x=Audit_label,y=Clean_p-Clean_pSim))+
  geom_boxplot(colour='dark blue',alpha=.2,size=1)+
  g.theme + theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylab('Observed-Predicted: Average Cleaning Success')+xlab('')

#############################################################
#Sensitivity analysis of selected model (Model 3)

#define influence.ME object
sens_glmm<-influence(glmm.fit.3,"Site")

#Cook's distance following removal of each site
cooksd_glmm<-cooks.distance(sens_glmm,parameters=1:2)


#DFBETA
dfbetas_glmm<-data.frame(Site=1:11,dfbetas(sens_glmm))
colnames(dfbetas_glmm)<-c('Site','Intercept','Intervention','IntTime')
dfbetas_glmm_long<-data.table(melt(dfbetas_glmm,id.vars='Site',value.name='DFBETA'))

#Percentage change in paramter estimates when each site is removed (similar to DFBETA but different scale)
pchange_glmm<-data.frame(Site=1:11,pchange(sens_glmm))
pchange_glmm_long<-melt(pchange_glmm,id.vars='Site',variable.name='Effect',value.name='pchange')

#plot changes in estimated effects based on removal of each site from the dataset (focus on intervention effect)
alt.est_glmm<-melt(sens_glmm$alt.fixed,varnames=c('Site','Effect'),value.name='Estimate')
alt.se_glmm<-melt(sens_glmm$alt.se,varnames=c('Site','Effect'),value.name='SE')

alt_glmm<-merge(alt.est_glmm,alt.se_glmm,by=c('Site','Effect'))

#plot
ggplot(subset(alt_glmm,Effect!='(Intercept)'),aes(x=factor(Site),y=Estimate,group=Effect,ymin=Estimate-1.96*SE,ymax=Estimate+1.96*SE))+
  geom_point()+geom_errorbar()+facet_wrap(~Effect,scales='free')+
  xlab('Site')+
  g.theme+theme(strip.background = element_rect(fill='white'))

