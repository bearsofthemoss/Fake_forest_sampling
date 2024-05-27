
library(readxl)
library(reshape2)
library(ggplot2)

#for figure 4)   (same datafile though-  Alex_re_arrange_4_29)
c<-  as.data.frame(read_excel("Alex_re_arrange_4_29.xlsx"))
c<-c[!c$model=="M2",]
table(c$distribution)

table(c$distribution, c$model)

c[c$distribution=="triangle shaped","distribution"]<-"not used"
c[c$distribution=="Left truncated triangles","distribution"]<-"not used"
c[c$distribution=="Right truncated triangles","distribution"]<-"not used"


c <- c[!c$distribution=="not used", ]

table(c$distribution)

c[c$distribution=="Triangular left","distribution"]<- "Skewed left"
c[c$distribution=="Triangular right","distribution"]<- "Skewed right"
c[c$distribution=="Right truncated uniform","distribution"]<- "Truncated uniform right"
c[c$distribution=="Left truncated uniform","distribution"]<- "Truncated uniform left"




# melt data # from dataframe 'c'

cm<-melt(c, id.vars = c("model","structure","distribution","rows"))  
head(cm) 
table(cm$distribution)


############ ALEX CHANGE THE 8, 15, or 16 classes for the dists!
# this orders the distributions
cm$distribution<-factor(cm$distribution, levels=c( "Proportional","Parabolic",
                                                   "Skewed right","Skewed left",
                                                   "Truncated uniform left","Truncated uniform right"))

cm$model[cm$model == "M1"] <- "Model 1"
cm$model[cm$model == "M3"] <- "Model 3"
cm$model[cm$model == "M4"] <- "Model 4"

table(cm$distribution)

cm$variable<-as.factor(cm$variable)
##### Fig 4 as fa, fb, fc

cm
median(cm$value)


summary(cm$log_value)

my_breaks = c(1, 10,  50,  95)
color_scale <- rev(colorRampPalette(c('darkred','red', 'yellow', 'orange', '#78C679', '#41AB5D', '#238443', 'forestgreen',"darkgreen"))(8))


head(cm)

## 8/21
## drop model 1, 3, 4.  Then show columns as distributions

# 
di <- cm[cm$model=="Model 1", ]

library(scales)


di[di$structure=="arbogast","structure"] <- "Reverse-J"
di[di$structure=="uncless","structure"] <- "Even-aged distribution"
di[di$structure=="uni","structure"] <- "Uniform"


ggplot(di , aes(x=variable, y=rows))+
  geom_tile(aes(fill=value*100), colour=NA)+
  scale_fill_gradientn(name="Absolute Uncertainty (%)",colors = color_scale, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(distribution ~ structure, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,8,1))+
  theme(text = element_text(family = "Calibri"))


###########################################################################


fa<-ggplot(cm[cm$structure=="uncless",], aes(x=variable, y=rows))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradientn(name="",colors = color_scale, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Reverse-J") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))
fa


str(cm)
fb<-ggplot(cm[cm$structure=="arbogast",], aes(x=variable, y=rows ))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradientn(name="",colors = color_scale, trans = "log10",
                       breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Even-aged distribution") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))
fb

fc<-ggplot(cm[cm$structure=="uni",], aes(x=variable, y=rows ))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradientn(name="",colors = color_scale, trans = "log10",
                       breaks = my_breaks, labels = my_breaks) +
   ylab("Sampling strategy")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Uniform") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))
fc

library(ggpubr)
ggarrange(fa, fb, fc, common.legend = T, ncol=3, legend="bottom")


ggsave(file = "fake_tree_pdf3.png", dpi = 600, width = 8, height = 6, units = "in")


######  test
gm



ggplot(cm[cm$structure=="uncless",], aes(x=variable, y=rows))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradientn(name="",colors = color_scale, trans = "log10",
                       breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Reverse-J") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))



summary(cm$value)




###############  end here.  Hope the script runs :)






















#######################################################


### abandon below??    ## Outdated graphing attempts.AY- 12_6_2019
library(tidyr)
scm2<-melt(sc2, id.vars = c("Distribution2","row2"))
scm<-melt(sc, id.vars = c("Distribution","row"))
head(scm)


table(scm$Distributio
      
      n)
scm$Distribution<-factor(scm$Distribution, levels=c("Triangular left","Triangular right",
                                                    "Truncated triangles","Truncated uniform",
                                                    "triangle shaped","Parabolic","Proportional"))


ggplot(scm, aes(x=variable, y=row, fill=value ))+geom_tile()+
  geom_text(aes( label=round(value,1)), colour="white")+
  scale_fill_gradientn(colours = terrain.colors(7))+ylab("")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  ggtitle("Proportion of size class") + facet_wrap( ~ Distribution , nrow=7,ncol=1, scales="free",strip.position = c("left"))+
  xlab("Diameter size class")







