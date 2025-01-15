
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
v2$y <- v2$dbh
v2$x <- v2$biomass

arb$type <- "Even-aged"
uni$type <- "Uniform"
v2$type <- "Reverse J"


ff <- rbind( arb, uni, v2[ , c("x","y","type")])

plot(ff$x, ff$y)

summary(ff$y)

ggplot(ff, aes(x=y, y=x))+
  facet_wrap(~type, nrow=3)+
  stat_binhex(bins=80) +
  scale_fill_gradientn(
    colors=1:6,
    limits = c(0,850))+
#  xlim(0,90)+
  labs(x="Diameter (cm)",y="Biomass",
       fill="Count")+
  theme_bw()+theme(panel.grid = element_blank())+
 scale_x_continuous(expand = c(0, 0), limits=c(0,90)) + scale_y_continuous(expand = c(0, 0))

### If you wanted to add lines to base R figures
# fitting lowess and spline
par(mfrow=c(1,3))

plot(arb$x ~ arb$y, main="arbogast", xlab="Diameter",ylab="Biomass")
lines(lowess(arb$y, arb$x), col="red", lwd=8)
lines(smooth.spline(arb$y,arb$x, df=5),lwd=8, lty=3, col="yellow")


plot(uni$x ~ uni$y, main="uniform", xlab="Diameter",ylab="Biomass")
lines(lowess(uni$y, uni$x), col="red", lwd=8)
lines(smooth.spline(uni$y,uni$x, df=5),lwd=8, lty=3, col="yellow")

plot(v2$biomass ~ v2$dbh, main="J", xlab="Diameter",ylab="Biomass")
lines(lowess(v2$dbh, v2$biomass), col="red", lwd=8)
lines(smooth.spline(v2$dbh,v2$biomass, df=5),lwd=8, lty=3, col="yellow")





