

## This script is intended to read in the data and make a series of figures.

# Alex Young
# June 2023

###load these packages
library(ggplot2)
library(tidyr)
library(ggpubr)
library(readxl)
library(reshape2)
library(here)


#######################################

fd <- file.path(here::here(),"Data_folder","forest_distributions")

##(for figure 1)

arb<-read.csv(file.path(fd,"fakeforest_arbogast.csv"))
uni<-read.csv(file.path(fd,"fakeforest_uniform.csv"))
v2<-read.csv(file.path(fd,"fakeforest_V2.csv"))

# 
# par(mfrow=c(3,2))
# plot(uni$x~ uni$y, main="uniform", )
# plot(arb$x~ arb$y, main="reverse-J")
# plot(v2$biomass~ v2$dbh, main="Fake forest v2")
# 
# hist(uni$y)
# hist(arb$y)
# hist(v2$dbh)

#(for figure 2)
sc<-  as.data.frame(read_excel("Data_folder/Alex_re_arrange_4_29.xlsx", sheet="size.class"))

# #for figure 4)   (same datafile though-  Alex_re_arrange_4_29)
# c<-  as.data.frame(read_excel("Alex_re_arrange_4_29.xlsx"))
# c<-c[!c$model=="M2",]
# table(c$distribution)


#################################
# for checking, 2 miscellaneous graphs

# pdf('fake_tree_pdf1.pdf', 
#    width = par('din')[1],
#    height = par('din')[2])

par(mfrow=c(1,3), mar=c(5,5,5,1))
hist(v2$dbh, xlab="Diameter (cm)",main="(a) Reverse J distribution",ylab="Frequency", xlim=c(10,85),breaks=seq(10,85,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="red")
hist(arb$y, xlab="Diameter (cm)", main="(b) Even-aged distribution", xlim=c(10,85),breaks=seq(10,85,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="orange")
hist(uni$y, xlab="Diameter (cm)", main="(c) Uniform distribution", xlim=c(10,85), breaks=seq(10,85,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="blue")
dev.off()


### If you wanted to add lines to base R figures
# fitting lowess and spline
par(mfrow=c(1,1))
plot(arb$x ~ arb$y, main="arbogast", xlab="Diameter",ylab="Biomass")
lines(lowess(arb$y, arb$x), col="red")
lines(smooth.spline(arb$y,arb$x, df=5), lty=3, col="yellow")


###############################################################
## FIGURE 1
# Yet another way to do figure 1

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





