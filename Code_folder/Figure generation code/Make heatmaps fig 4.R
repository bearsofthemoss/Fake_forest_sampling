library(dplyr)
library(here)
library(ggplot2)

avg_path <- file.path(here::here(), "Data_folder","New_model_run_averages")


avg <- read.csv(file.path( avg_path , "avg_combined.csv"))
avg <- avg[,-1]


# order the levels
avg$Sample <- factor(avg$Sample, 
                          levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

avg$dist_name<-factor(avg$distribution, levels=c( "Proportional","Parabolic",
                                                                    "Skewed right","Skewed left",
                                                                    "Truncated uniform left","Truncated uniform right"))





avg_my_colors <- rev( c("darkred", "red", "orange", "yellow", "lightgreen", "green", "darkgreen"))

summary(avg$value)

avg_my_breaks = c(1,10 ,100,10000)



# Plot heat map with the averages
m1 <- avg[avg$model=="Model 4",]

library(ggplot2)
g1 <- ggplot(m1 , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradientn(name="Absolute uncertainty",trans = "log10",
                       colors = avg_my_colors, 
                       breaks = avg_my_breaks, 
                       labels = avg_my_breaks) +
  ylab("Sampling strategy")+ xlab("Number of trees") +
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

g1

###


##########################################################


# make a df with just model 2
a <- combined[combined$Diam_distribution=="Arb",]

m1

library(ggplot2)
ggplot(m1 , aes(x=Sample, y=Est))+
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
