# ICHE.figure.R
# figure for ICHE paper
library(ggplot2)
library(gridExtra)
library(grid)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set.seed(1234) # to give same pictures

# Few Locations, common change time
data1 = read.table(header=T, sep=',', text='
Location,Time,Period
A,1,1
A,2,1
A,3,2
A,4,2
A,5,2
A,6,2
B,1,1
B,2,1
B,3,2
B,4,2
B,5,2
B,6,2
C,1,1
C,2,1
C,3,1
C,4,1
C,5,1
C,6,1
')
data1$Period = factor(data1$Period, levels=1:2, labels=c('Before\nchange','After\nchange')) # for ordering

# 2) Many Locations, random change time
data2 = expand.grid(Location=LETTERS[1:8], Time=1:6)
Period = data.frame(Location=LETTERS[1:8], Change=sample(2:5, size=8, replace=T))
data2 = merge(data2, Period, by='Location')
data2$Period = as.numeric(data2$Time >= data2$Change) + 1
data2$Period = factor(data2$Period, levels=1:2, labels=c('Before\nchange','After\nchange')) # for ordering

# 3) Step-wedge
rand = sample(LETTERS[1:5], 5, replace = FALSE)
data3 = expand.grid(Location=rand, Time=1:6)
Period = data.frame(Location=rand, Change=2:6)
data3 = merge(data3, Period, by='Location')
data3$Period = as.numeric(data3$Time >= data3$Change) + 1
data3$Period = factor(data3$Period, levels=1:2, labels=c('Before\nchange','After\nchange')) # for ordering

# 4) Many Locations, random change time, with interim period
data4 = expand.grid(Location=LETTERS[1:8], Time=1:6)
Period = data.frame(Location=LETTERS[1:8], Change=sample(2:5, size=8, replace=T))
data4 = merge(data4, Period, by='Location')
data4$Period = as.numeric(data4$Time == data4$Change) + 1 # during
data4$Period[data4$Time > data4$Change] = 3 # add one to allow "during change"
data4$Period = factor(data4$Period, levels=1:3, labels=c('Before\nchange','During\nchange','After\nchange')) # for ordering

# panel 1
tplot1 = ggplot(data=data1, aes(x=Time, y=Location, fill=Period))+
  geom_tile(col='grey')+
  theme_bw()+
  ggtitle('3 hospitals')+
  xlab('')+ # use labels in grid.arrange
  ylab('')+ 
  scale_x_continuous(breaks=1:6)+
  scale_fill_manual('Time period', values=cbPalette[c(6,8)])+
  theme(panel.grid.minor = element_blank(), legend.position='none',
        axis.title.x=element_blank(), axis.title.y=element_blank())
# panel 2
tplot2 = ggplot(data=data2, aes(x=Time, y=Location, fill=Period))+
  geom_tile(col='grey')+
  theme_bw()+
  ggtitle('8 hospitals')+
  xlab('')+ # use labels in grid.arrange
  ylab('')+ 
  scale_x_continuous(breaks=1:6)+
  scale_fill_manual('Time\nperiod', values=cbPalette[c(6,8)])+
  theme(panel.grid.minor = element_blank(), legend.position='none',
        axis.title.x=element_blank(), axis.title.y=element_blank())
tplot2
# panel 3
tplot3 = ggplot(data=data3, aes(x=Time, y=Location, fill=Period))+
  geom_tile(col='grey')+
  theme_bw()+
  ggtitle('5 hospitals')+
  ylab('')+
  xlab('')+ # use labels in grid.arrange
  scale_x_continuous(breaks=1:6)+
  scale_fill_manual('Time\nperiod', values=cbPalette[c(6,8)])+
  theme(panel.grid.minor = element_blank(), legend.position='none',
        axis.title.x=element_blank(), axis.title.y=element_blank())
tplot3
# panel 4 - with legend so it can be extracted
tplot4 = ggplot(data=data4, aes(x=Time, y=Location, fill=Period))+
  geom_tile(col='grey')+
  theme_bw()+
  ggtitle('8 hospitals')+
  ylab('')+
  xlab('')+ # use labels in grid.arrange
  scale_x_continuous(breaks=1:6)+
  scale_fill_manual('Time\nperiod', values=cbPalette[6:8])+
  theme(panel.grid.minor = element_blank(), legend.position='top',
        axis.title.x=element_blank(), axis.title.y=element_blank())
tplot4
# extract Legend -  see https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 
legend <- g_legend(tplot4) 
# panel 3 - without legend for plot
tplot4 = tplot4 +
  theme(legend.position='none')
tplot4

# plot
jpeg('WhiteFigure1.jpg', width=7, height=4, units='in', res=300)
grid.arrange(legend, arrangeGrob(tplot1,tplot2,tplot3,tplot4, ncol=4), 
             heights=c(0.5/3, 2.5/3), ncol=1,
             bottom="Time", left="Location")
dev.off()


