

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

fd <- file.path(here::here(),"Data_folder")

##(for figure 1)

arb<-read.csv(file.path(fd,"fakeforest_arbogast.csv"),header=FALSE)
uni<-read.csv(file.path(fd,"fakeforest_uniform.csv"),header=FALSE)
J<-read.csv(file.path(fd,"fakeforestJ.csv"),header=FALSE)


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
sc<-  as.data.frame(read_excel("Data_folder/forest_distributions/Alex_re_arrange_4_29.xlsx", sheet="size.class"))

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
hist(J$V1, xlab="Diameter (cm)",main="(a) Reverse J distribution",ylab="Frequency", xlim=c(10,101),breaks=seq(10,101,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="red")
hist(arb$V1, xlab="Diameter (cm)", main="(b) Even-aged distribution", xlim=c(10,101),breaks=seq(10,101,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="orange")
hist(uni$V1, xlab="Diameter (cm)", main="(c) Uniform distribution", xlim=c(10,101), breaks=seq(10,101,5),cex.axis=1.5, cex.lab=1.9, cex.sub=1.8, cex.main=1.8, col="blue")
dev.off()


summary(J$V1)
summary(arb$V1)
summary(uni$V1)

###############################################################
## FIGURE 1
# Yet another way to do figure 1

summary(v2)
summary(arb)
summary(uni)

mex<-ggplot(J, aes(V1))+geom_histogram(binwidth = 10, fill="red", color="black")+theme_classic()+
  ggtitle("Reverse J")+xlab("")+ylab("Frequency")+theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

mex

f.arb<-ggplot(arb, aes(V1))+geom_histogram(binwidth = 10, fill="orange", color="black")+theme_classic()+
  ggtitle("Even-aged")+xlab("")+ylab("Frequency")+theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  f.arb

f.uni<-ggplot(uni, aes(V1))+geom_histogram(binwidth = 10, fill="blue", color="black")+theme_classic()+
  ggtitle("Uniform")+xlab("Diameter (cm)")+ylab("Frequency")+theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
f.uni


dpi=300    #pixels per square inch
tiff(here::here("Fig_1.tif"), width=10*dpi, height=5*dpi, res=dpi)
ggarrange(mex, f.arb, f.uni, ncol=1)
dev.off()







