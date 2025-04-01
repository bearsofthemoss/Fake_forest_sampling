

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


rm(list=ls(all=TRUE))

#######################################

fd <- file.path(here::here(),"Data_folder","forest_distributions")

##(for figure 1)

arb<-read.csv(file.path(fd,"fakeforest_arbogast.csv"))
uni<-read.csv(file.path(fd,"fakeforest_uniform.csv"))
J_for<-read.csv(file.path(fd,"fakeforest_J_ed.csv"))

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
sc<-  as.data.frame(read_excel("Data_folder/forest_distributions/Alex_re_arrange_4_29.xlsx", sheet="size.class3"))

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
hist(J_for$dbh, xlab="Diameter (cm)",main="(a) Reverse J distribution",ylab="Frequency", xlim=c(10,100),breaks=seq(0,110,10),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="red")
hist(arb$dbh, xlab="Diameter (cm)", main="(b) Even-aged distribution", xlim=c(10,100),breaks=seq(0,110,10),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="orange")
hist(uni$dbh, xlab="Diameter (cm)", main="(c) Uniform distribution", xlim=c(10,100), breaks=seq(0,110,10),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="blue")
dev.off()




###############################################################
## FIGURE 1
# Yet another way to do figure 1

summary(J_for)
summary(arb)
summary(uni)

f.mex<-ggplot(J_for, aes(dbh))+geom_histogram(binwidth = 10, fill="red", color="black")+theme_classic()+
  ggtitle("Reverse J distribution")+xlab("")+ylab("Frequency")+theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  f.mex

f.arb<-ggplot(arb, aes(dbh))+geom_histogram(binwidth = 10, fill="orange", color="black")+theme_classic()+
  ggtitle("Even-aged distribution")+xlab("")+ylab("")+theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  f.arb

f.uni<-ggplot(uni, aes(dbh))+geom_histogram(binwidth = 10, fill="blue", color="black")+theme_classic()+
  ggtitle("Uniform distribution")+xlab("Diameter (cm)")+ylab("")+theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
f.uni

ggarrange(mex, f.arb, f.uni, ncol=1)





