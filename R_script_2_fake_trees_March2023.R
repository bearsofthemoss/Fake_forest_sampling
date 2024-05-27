

## This script is intended to read in the data and make a series of figures.

# Alex Young
# June 2023

###load these packages
library(ggplot2)
library(tidyr)
library(ggpubr)
library(readxl)
library(reshape2)


#######################################

##(for figure 1)
getwd()

setwd("D:/Users/bears/Downloads")

#fake_F/fake.tree.Wayson.csv"

arb<-read.csv("fakeforest_arbogast.csv")
uni<-read.csv("fakeforest_uniform.csv")
v2<-read.csv("fakeforest_V2.csv")

par(mfrow=c(3,2))
plot(uni$x~ uni$y, main="uniform", )
plot(arb$x~ arb$y, main="reverse-J")
plot(v2$biomass~ v2$dbh, main="Fake forest v2")

hist(uni$y)
hist(arb$y)
hist(v2$dbh)

#(for figure 2)
sc<-  as.data.frame(read_excel("Alex_re_arrange_4_29.xlsx", sheet="size.class"))

#for figure 4)   (same datafile though-  Alex_re_arrange_4_29)
c<-  as.data.frame(read_excel("Alex_re_arrange_4_29.xlsx"))
c<-c[!c$model=="M2",]
table(c$distribution)


#################################
# for checking, 2 miscellaneous graphs

pdf('fake_tree_pdf1.pdf', 
   width = par('din')[1],
   height = par('din')[2])
par(mfrow=c(1,3), mar=c(5,5,5,1))
hist(v2$dbh, xlab="Diameter (cm)",main="(a) Reverse J distribution",ylab="Frequency", xlim=c(10,85),breaks=seq(10,85,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="red")
hist(arb$y, xlab="Diameter (cm)", main="(b) Even-aged distribution", xlim=c(10,85),breaks=seq(10,85,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="orange")
hist(uni$y, xlab="Diameter (cm)", main="(c) Uniform distribution", xlim=c(10,85), breaks=seq(10,85,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="blue")
dev.off()


# fitting lowess and spline
par(mfrow=c(1,1))
plot(arb$x ~ arb$y, main="arbogast", xlab="Initial Density",ylab="Number Killed")
lines(lowess(arb$y, arb$x), col="red")
lines(smooth.spline(arb$y,arb$x, df=5), lty=3, col="yellow")


###############################################################
## FIGURE 1
mex<-ggplot(v2, aes(dbh))+geom_histogram(binwidth = 10, fill="white", color="black")+theme_classic()+
  ggtitle("Diameter distribution in Mexico")+xlab("Diameter (cm)")+ylab("Frequency")+theme(text=element_text(size=20))+
  scale_x_continuous(breaks=seq(0,90,20))+theme(plot.title = element_text(hjust = 0.5))
mex

f.arb<-ggplot(arb, aes(y))+geom_histogram(binwidth = 10, fill="white", color="black")+theme_classic()+
  ggtitle("Arbogast distribution")+xlab("Diameter (cm)")+ylab("")+theme(text=element_text(size=20))+
  scale_x_continuous(breaks=seq(0,90,20))+theme(plot.title = element_text(hjust = 0.5))
f.arb

f.uni<-ggplot(uni, aes(uni$y))+geom_histogram(binwidth = 10, fill="white", color="black")+theme_classic()+
  ggtitle("Uniform distribution")+xlab("Diameter (cm)")+ylab("")+theme(text=element_text(size=20))+
  scale_x_continuous(breaks=seq(0,90,20))+theme(plot.title = element_text(hjust = 0.5))
f.uni

ggarrange(mex, f.arb, f.uni, ncol=3)

##############################################
## FIGURE 2


head(sc)
gs<-gather(sc, "size.class","value",3:10)
gs$row<-as.factor(gs$row)
head(gs)
table(gs$Distribution)

gs[gs$Distribution=="Forests in Mexico","Distribution"]<-"Reverse-J"
gs[gs$Distribution=="Proportional arbogast","Distribution"]<-"Truncated normal"
gs[gs$Distribution=="Proportional uniform","Distribution"]<-"Uniform"

table(gs$Distribution)

gs$group<-paste(gs$row, gs$Distribution)
gs$Distribution<-factor(gs$Distribution, levels=c( "Reverse-J", "Truncated normal","Uniform",
                                                    "Triangular left","Triangle shaped","Triangular right",
                                                    "Truncated triangles left","Parabolic","Truncated triangles right",
                                                    "Truncated uniform left","Truncated uniform right"))

gs<-gs[gs$value>0,]
table(gs$size.class)

tail(gs)
gs$size.class[gs$size.class == "sc1"] <- "10-20"
gs$size.class[gs$size.class == "sc2"] <- "20-30"
gs$size.class[gs$size.class == "sc3"] <- "30-40"
gs$size.class[gs$size.class == "sc4"] <- "40-50"
gs$size.class[gs$size.class == "sc5"] <- "50-60"
gs$size.class[gs$size.class == "sc6"] <- "60-70"
gs$size.class[gs$size.class == "sc7"] <- "70-80"
gs$size.class[gs$size.class == "sc8"] <- "80-90"

head(gs)
names<-c("Reverse-J", "Truncated normal","Uniform","Triangular left","Triangle right","Parabolic","Truncated uniform left","Truncated uniform right")
gp<-gp[gp$Distribution== names,]

dim(gp)
dim(gs)
table(gp$Distribution)

fffig2<-ggplot(gs, aes(x=size.class, y=value, color=row))+geom_point()+
  facet_wrap(~Distribution, ncol=3)+geom_line(aes(group=group))+ylab("Proportion of distribution (%)")+xlab("Tree diameter class (cm)")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
	theme(legend.position="none")
fffig2




ggsave(file = "fake_tree_pdf2.png", dpi = 600, width = 8, height = 6, units = "in")



## ARY June 2023-  I think the code below might be better presented in the 
##  two other R scripts, take2 fig3.R and take2 fig4.R.






#############  = meat
#####  FIGURE 4
## this has the distribution names once I broke them out?

# melt data # from dataframe 'c'
cm<-melt(c, id.vars = c("model","structure","distribution","rows"))  
head(cm) 
table(cm$distribution)


############ ALEX CHANGE THE 8, 15, or 16 classes for the dists!
# this orders the distributions
cm$distribution<-factor(cm$distribution, levels=c( "Proportional","Parabolic",
          "Triangular left","Triangular right",
          "triangle shaped","Left truncated triangles", "Right truncated triangles",
          "Left truncated uniform","Right truncated uniform"))
          
cm$model[cm$model == "M1"] <- "Model 1"
cm$model[cm$model == "M3"] <- "Model 2"
cm$model[cm$model == "M4"] <- "Model 4"

cm$variable<-as.factor(cm$variable)
##### Fig 4 as fa, fb, fc
fa<-ggplot(cm[cm$structure=="uncless",], aes(x=variable, y=rows ))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradient(low="green", high="yellow", limits=c(0,0.05), na.value ="red")+ylab("Row number")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Reverse-J") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))
fa

fb<-ggplot(cm[cm$structure=="arbogast",], aes(x=variable, y=rows ))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradient(low="green", high="yellow", limits=c(0,0.05), na.value ="red")+ylab("")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Truncated normal") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))
fb

fc<-ggplot(cm[cm$structure=="uni",], aes(x=variable, y=rows ))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradient(low="green", high="yellow", limits=c(0,0.05), na.value ="red")+ylab("")+
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +theme(plot.title = element_text(size = 25, face = "bold"))+
  ggtitle("Uniform") + facet_grid(distribution ~ model, scales="free", labeller = label_wrap_gen(width=10))+
  xlab("Sample size") +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))
fc

library(ggpubr)
ggarrange(fa, fb, fc, common.legend = T, ncol=3, legend="bottom")


ggsave(file = "fake_tree_pdf3.png", dpi = 600, width = 8, height = 6, units = "in")



###############  end here.  Hope the script runs :)




### abandon below??    ## Outdated graphing attempts.AY- 12_6_2019
library(tidyr)
scm2<-melt(sc2, id.vars = c("Distribution2","row2"))
scm<-melt(sc, id.vars = c("Distribution","row"))
head(scm)


table(scm$Distribution)
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







