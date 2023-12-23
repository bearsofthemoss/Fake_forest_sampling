library(dplyr)
library(here)
library(ggplot2)

wd <- here::here()

getwd()

datapath <- file.path(wd, "Data_folder","New_model_run_Nov_2023")

combined <- read.csv(file.path(datapath, "combined.csv"))



head(combined)
table(combined$Diam_distribution)
table(combined$model)
table(combined$distribution)
table(combined$Sample)


# re-name the distribrution names
combined[combined$distribution=="Triangular left","distribution" ]<- "Skewed left"
combined[combined$distribution=="Triangular rigth","distribution" ]<- "Skewed right"
combined[combined$distribution=="Truncated Triangles","distribution" ]<- "Truncated triangle 16 X"
combined[combined$distribution=="Triangular Ushaped","distribution" ]<- "U-shaped triangle"


## add left and right truncated uniform
combined[combined$Est=="E1" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E2" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E3" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E4" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E5" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E6" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E7" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
combined[combined$Est=="E8" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"

combined[combined$Est=="E9" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E10" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E11" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E12" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E13" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E14" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E15" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
combined[combined$Est=="E16" & combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"


table(combined$distribution)
# 
combined$dist_name<-factor(combined$distribution, levels=c( "Proportional","Parabolic",
                                                            "Skewed right","Skewed left",
                                                            "Truncated uniform left","Truncated uniform right"))

# Removes U shaped triangle 
combined <- combined[!is.na(combined$dist_name),]
summary(combined$dist_name)

# take out 'E' from sampling strategy
combined$Est <- as.numeric(gsub("E", "", combined$Est))
combined$tree_count<-combined$Sample
combined$Sample<- paste0("n",combined$Sample)


combined$Sample <- factor(combined$Sample, 
                          levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

color_scale <- (colorRampPalette(c('darkred','darkred','darkred','red','red', 'yellow','yellow', 'orange', '#78C679', '#41AB5D', '#238443','forestgreen',"darkgreen"))(20))



# Define the colors and breakpoints
my_colors <- c("darkred", "red", "orange", "yellow", "lightgreen", "green", "darkgreen")
my_breaks = c(1,5,10,25,50,90)



# make a df with just model 1
m1 <- combined[combined$model=="Model 1",]

library(ggplot2)
ggplot(m1 , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=p25*100), colour=NA)+
  scale_fill_gradientn(name="Percent of simulations with <15% difference",colors = my_colors, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(dist_name ~ Diam_distribution, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
        legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,16,1))+
  theme(text = element_text(family = "Calibri"))


###

# make a df with just model 2
a <- combined[combined$Diam_distribution=="Arb",]

library(ggplot2)
ggplot(a , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=p10*100), colour=NA)+
  scale_fill_gradientn(name="Percent of simulations with <10% difference",colors = my_colors, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(dist_name ~ model, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
        legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,16,1))+
  theme(text = element_text(family = "Calibri"))


#############  5, 10, 20, 30, 40, 50

# Work with just the J forest type
j <- combined[combined$Diam_distribution=="J",]
j <- j[,-1]
head(j)

library(tidyr)


jg <- gather(j, "prop","value" , c(5,7,9,10,11))
table(jg$prop)
head(jg)
table(jg$model)

jg$prop<-factor(jg$prop, levels=c("p10","p20","p30","p40","p50"))

ggplot(jg , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=value*100), colour=NA)+
  scale_fill_gradientn(name="Percent of simulations within X% difference",colors = my_colors, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(dist_name ~ prop, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,16,1))+
  theme(text = element_text(family = "Calibri"))



#############



# Work with the 30 trees being sampled category
p53 <- as.data.frame(combined[combined$Sample=="n30", ])
p53 <-p53[order(-p53$p10), c("Diam_distribution","distribution","Est","model","Sample","p10")]

names(p53)
an <- aggregate(p53$p10, by=list(model= p53$model, dist = p53$distribution, diam_dist = p53$Diam_distribution, Est = p53$Est), FUN="max")

dim(an)
dim(p53)
# p53 is only do 30 trees, and is ordered by the percent of the runs with <10% difference 


all <- p53[order(-p53$p10), c("p10","Diam_distribution","distribution","Est","model")]
#all <- all[all$model == "Model 1",]
a <- all[all$Diam_distribution=="Arb",]
a <- a[order(-a$p10),]
dim(a)
a <- a[1:50,]
at <- as.data.frame(table(a$distribution, a$Est, a$model))
at <- at[at$Freq>0,]
at$diam_dist <- "Arb"

j <- all[all$Diam_distribution=="J",]
j <- j[order(-j$p10),]
dim(j)
j <- j[1:50,]
jt <- as.data.frame(table(j$distribution, j$Est, j$model))
jt <- jt[jt$Freq>0,]
jt$diam_dist <- "J"

u <- all[all$Diam_distribution=="Uni",]
u <- u[order(-u$p10),]
dim(u)
u <- u[1:50,]
ut <- as.data.frame(table(u$distribution, u$Est, u$model))
ut <- ut[ut$Freq>0,]
ut$diam_dist <- "Uniform"

co <- rbind(at, jt, ut)

ggplot(co, aes(x=Var3, y=Freq, fill=Var2))+
  facet_grid(Var1~diam_dist)+geom_col()+
  labs(x="Model", y="Number of occurance in the top 50",
       fill="Implementation DBH sampling")+theme_bw()+
  ggtitle("The top 50 sampling strategy and distributions for each forest")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))



########


m1 <- combined[combined$model=="Model 1",]
mu <- m1[m1$dist_name=="Truncated uniform right",]

library(ggplot2)
ggplot(mu , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=p25*100), colour=NA)+
  scale_fill_gradientn(name="Percent of simulations with <15% difference",colors = my_colors, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(dist_name ~ Diam_distribution, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
        legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,16,1))+
  theme(text = element_text(family = "Calibri"))
