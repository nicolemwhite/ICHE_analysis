library(lme4)
library(ggplot2)

data = readRDS("auditdata_all.RDS")

#fit different models to all data
#need to scale TimeFromStart_Int for model fitting
data$TimeFromStart_Int_scaled<-data$TimeFromStart_Int/10


#models are Binomial GLMM with random intercept for each site
#Model 1: Binary intevention switch
glmm.fit.1<-glmer(cbind(Clean_N, NotClean_N) ~ Period + (1 | Site),data=data, family=binomial('logit'))

#Model 2: Linear trend from start of intervention period
glmm.fit.2<-glmer(cbind(Clean_N, NotClean_N) ~ TimeFromStart_Int_scaled + (1 | Site),data=data, family=binomial('logit'))

#Model 3: Step change + linear trend over intervention
glmm.fit.3<-glmer(cbind(Clean_N, NotClean_N) ~ Period + TimeFromStart_Int_scaled + (1 | Site),data=data, family=binomial('logit'))
#########################################################
#PLOT PREDICTED MODEL TRENDS FROM EACH GLMM OVER RAW DATA

#create newdata object for trend line
#add extra zero to intervention period for smooth trend
#also add time from study start
new.data<-data.frame(Period=rep(c('Control','Intervention'),c(3,11)),TimeFromStart_Int_scaled=c(rep(0,3),seq(0,40,4))/10,TimePlot=c(0,4,8,seq(8,48,4)))

#Add GLMM predictions to new.data
new.data$pred.glmm_1<-predict(glmm.fit.1,newdata=new.data,re.form=NA,type='response')
new.data$pred.glmm_2<-predict(glmm.fit.2,newdata=new.data,re.form=NA,type='response')
new.data$pred.glmm_3<-predict(glmm.fit.3,newdata=new.data,re.form=NA,type='response')

#reshape new.data to allow for facets in ggplot
new.data_long<-melt(new.data,id.vars=c('Period','TimeFromStart_Int_scaled','TimePlot'),variable.name='Model string',value.name='pred')
new.data_long$Model<-factor(gsub(".*_", "", new.data_long$`Model string`),levels=1:3,labels=c('Binary','Linear','Binary+Linear'))

#raw data plot
g.data<-ggplot(data,aes(x=TimeFromStart_Control,y=Clean_N/(NotClean_N+Clean_N),group=Site))+geom_point(col='dark blue')
#define theme elements
g.theme<-theme_bw()+theme(text=element_text(size=12),legend.position = 'bottom',legend.direction='horizontal')
#generate plot with trend lines superimposed
g1<-g.data+
  geom_line(data=new.data_long,aes(x=TimePlot,y=pred,group=Period),size=2)+
  facet_wrap(~Model)+
  geom_vline(aes(xintercept=8),linetype='dashed',col='dark grey',size=2)+
  xlab('Study Week')+
  ylab('Cleaning Success')+
  g.theme

jpeg('GLMMtrendPlots.jpg', width=7, height=4, units='in', res=300)
print(g1)
dev.off()

###########################################################
#PLOT ACF OF RESIDUALS FOR EACH GLMM
jpeg('ACFs.jpg', width=7, height=4, units='in', res=300)
par(mfrow=(c(1,3)))
acf(residuals(glmm.fit.1),main="Binary")
acf(residuals(glmm.fit.2),main="Linear")
acf(residuals(glmm.fit.3),main="Binary+Linear")
dev.off()
