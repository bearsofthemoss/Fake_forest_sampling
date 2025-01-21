
###load these packages
library(ggplot2)
library(tidyr)
library(ggpubr)
library(readxl)
library(reshape2)
library(here)
library(tidyr)


#######################################

fd <- file.path(here::here(),"Data_folder")

##(for figure 1)

arb<-read.csv(file.path(fd,"fakeforest_arbogast.csv"),header=FALSE)
uni<-read.csv(file.path(fd,"fakeforest_uniform.csv"),header=FALSE)
J<-read.csv(file.path(fd,"fakeforestJ.csv"),header=FALSE)


biom <- read.csv(here::here("Data_folder","AmericasData.csv"))


arb$type <- "Even-aged"
uni$type <- "Uniform"
J$type <- "Reverse J"


ff <- rbind( arb, uni, J)

head(ff)

plot(ff$V1, ff$V2)

summary(ff$V2)

ggplot(ff, aes(x=V1, y=V2))+
 
  facet_wrap(~type, nrow=1)+
  stat_binhex(bins=80) +
  scale_fill_gradientn(
    colors=2:7,
    trans="log10",
    limits = c(1,400))+
  geom_point(data=biom, aes(x=DBH.cm., y=Dry.total.AGB.kg.), color="black")+
#  xlim(0,90)+
  labs(x="Diameter (cm)",y="Biomass (kg)",
       fill="Count of simulated trees")+
  theme_bw()+theme(panel.grid = element_blank())+
 scale_x_continuous(expand = c(0, 0), limits=c(0,90)) + scale_y_continuous(expand = c(0, 0))



# also this one

f2 <- ggplot(biom, aes(x=DBH.cm. , y=Dry.total.AGB.kg., col=Site))+
  geom_point(data=J, aes(x=V1, y=V2), alpha=0.5, color="gray")+
  geom_point()+
  labs(x="Diameter (cm)", y="Dry total AGB (kg)")+
  theme_bw()+
  #facet_wrap(~Site)+
  scale_color_manual(values=c("blue","purple","orange","brown","red"))+
  xlim(0,100)

dpi=300    #pixels per square inch
tiff(here::here("Fig_2.tif"), width=10*dpi, height=5*dpi, res=dpi)
f2
dev.off()




