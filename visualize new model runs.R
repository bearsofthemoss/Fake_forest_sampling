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
combined[combined$distribution=="Truncated Uniform","distribution" ]<- "Truncated uniform 16X"
combined[combined$distribution=="Triangular Ushaped","distribution" ]<- "U-shaped triangle"


# 
combined$dist_name<-factor(combined$distribution, levels=c( "Proportional","Parabolic",
                                                            "Skewed right","Skewed left",
                                                            "Truncated triangle 16 X","Truncated uniform 16X", "U-shaped triangle"))

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
  geom_tile(aes(fill=p15*100), colour=NA)+
  scale_fill_gradientn(name="Percent of simulations with <15% difference",colors = my_colors, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(dist_name ~ Diam_distribution, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,16,1))+
  theme(text = element_text(family = "Calibri"))


#############  5, 10, 20, 30, 40, 50

# Work with just the J forest type
j <- combined[combined$Diam_distribution=="J",]
head(j)

library(tidyr)


jg <- gather(j, "prop","value" , c(5,7,9,10,11))
table(jg$prop)

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

# p53 is only do 30 trees, and is ordered by the percent of the runs with <10% difference 


all <- p53[order(-p53$p10), c("p10","Diam_distribution","distribution","Est","model")]
all <- all[all$model == "Model 1",]
a <- all[all$Diam_distribution=="Arb",]
a <- a[order(-a$p10),]
dim(a)
a <- a[1:50,]
at <- as.data.frame(table(a$distribution, a$Est))
at <- at[at$Freq>0,]
at$diam_dist <- "Arb"

j <- all[all$Diam_distribution=="J",]
j <- j[order(-j$p10),]
dim(j)
j <- j[1:50,]
jt <- as.data.frame(table(j$distribution, j$Est))
jt <- jt[jt$Freq>0,]
jt$diam_dist <- "J"

u <- all[all$Diam_distribution=="Uni",]
u <- u[order(-u$p10),]
dim(u)
u <- u[1:50,]
ut <- as.data.frame(table(u$distribution, u$Est))
ut <- ut[ut$Freq>0,]
ut$diam_dist <- "Uniform"

co <- rbind(at, jt, ut)

ggplot(co, aes(x=Var1, y=Freq, fill=Var2))+
  facet_wrap(~diam_dist, ncol=3)+geom_col()+
  labs(x="Sampling strategy", y=" ")+theme_bw()+
  ggtitle("The sampling strategy and distribution the top 50 'best' runs came from for each forest type, Model 1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))

