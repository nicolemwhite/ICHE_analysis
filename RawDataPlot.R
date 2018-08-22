library(ggplot2)
#load all data
data = readRDS("auditdata_all.RDS")

#set theme elements for ggplot
g.theme<-theme_bw()+theme(text=element_text(size=12))
#main plot of raw data over time
g<-ggplot(audit.clean,aes(x=TimeFromStart_Control,y=Clean_N/(NotClean_N+Clean_N),group=Site))+
  geom_line(size=1,colour='dark blue')+
  geom_point(size=2,colour='dark blue')+
  geom_vline(aes(xintercept=8),colour='dark grey',linetype='dashed',size=2)+
  xlab('Study week')+ylab('Cleaning success')+g.theme
jpeg('WhiteFigure3.jpg',,width=600,height=480,res=400)
print(g)
dev.off()