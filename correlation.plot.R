# correlation.plot.R
# plot showing correlation neighbouring cleaning success proportions
# Aug 2018
library(dplyr)
library(ggplot2)

# data from github ICHE_analysis/auditdata_all.RDS 
data = readRDS("auditdata_scrambled.RDS")

# make into wide
data = mutate(data, success = Clean_N / (Clean_N + NotClean_N))
wide = uncorr1 = uncorr2 = NULL
set.seed(12345)
for (s in unique(data$Site)){
  for (p in c('Control','Intervention')){
    this = filter(data, Site==s & Period == p)
    for (a in 2:max(this$Audit_no)){
       frame = data.frame(typenum=1, Site=s, Period=p, future=this$success[a], past=this$success[a-1])
       wide = rbind(wide, frame)
       # random selection as a comparison (same hospital)
       random = as.numeric(filter(data, Site==s) %>% sample_n(size=1) %>% select(success))
       frame = data.frame(typenum=2, Site=s, Period=p, future=random, past=this$success[a-1])
       uncorr1 = rbind(uncorr1, frame)
       # random selection as a comparison
       random = as.numeric(sample_n(data, size=1) %>% select(success))
       frame = data.frame(typenum=3, Site=s, Period=p, future=random, past=this$success[a-1])
       uncorr2 = rbind(uncorr2, frame)
    }
  }
}
# Pearson correlations
(c1 = with(wide, cor(past, future, method='pearson')))
(c2 = with(uncorr1, cor(past, future, method='pearson')))
(c3 = with(uncorr2, cor(past, future, method='pearson')))

## plot
to.plot = rbind(wide, uncorr1, uncorr2)
# make nice labels for facets
l1 = paste('Neighbour (r=', round(c1, 2), ')', sep='')
l2 = paste('Random, same hospital (r=', round(c2, 2), ')', sep='')
l3 = paste('Random, any hospital (r=', round(c3, 2), ')', sep='')
labels = c(l1, l2, l3)
to.plot$type = factor(to.plot$typenum, levels=1:3, labels=labels, ordered = TRUE)
cplot = ggplot(data=to.plot, aes(x=past, y=future))+
  geom_point(col='dark blue')+
  theme_bw()+
  facet_wrap(~type)+
  xlab('Cleaning success')+
  ylab('Cleaning success at next audit')+
  theme(panel.grid.minor = element_blank())
jpeg('WhiteFigure2.jpg', width=7, height=4, units='in', res=400)
print(cplot)
dev.off()
