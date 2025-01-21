library(dplyr)
library(here)
library(ggplot2)


# this heat map to show the average absolute uncertainty

avg_path <- file.path(here::here(), "Data_folder","forest_distributions")


avg <- read.csv(file.path( avg_path , "avg_combined.csv"))
avg <- avg[,-1]
table(avg$distribution)

# order the levels
avg$Sample <- factor(avg$Sample, 
                     levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

avg[avg$distribution== "Truncated uniform left", "distribution"] <- "Weighted left"
avg[avg$distribution== "Truncated uniform right", "distribution"] <- "Weighted right"

avg[avg$distribution== "Triangle shaped", "distribution"] <- "Triangular"

table(avg$dist_name)

avg$dist_name<-factor(avg$distribution, levels=c( "Proportional","Parabolic",
                                                #  "Skewed right","Skewed left",
                                                  "Weighted left","Weighted right",
                                                "Triangular"))



table(avg$dist_name)

avg <- avg[!is.na(avg$dist_name),]
# color_scale <- (colorRampPalette(c('darkred','darkred','darkred','red','red', 'yellow','yellow', 'orange', '#78C679', '#41AB5D', '#238443','forestgreen',"darkgreen"))(20))
# 


# Define the colors and breakpoints
my_colors <- rev(c("darkred", "red", "orange", "tan", "lightgreen", "darkgreen"))
my_breaks = c(1,5,50, 500 )

summary(avg$value)

##########################################################
head(a)



a <- avg[avg$model=="Model 3",]
dim(a)
 a <- a[a$value < 2000000,]

 table(a$dist_name)
 
library(ggplot2)
f4_fig <- ggplot(a , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=value), colour=NA)+
  # scale_fill_gradientn(name="Percent of simulations with <10% difference",colors = my_colors,
  #                      trans = "log10",
  #                      breaks = my_breaks, labels = my_breaks) +
  # 
  scale_fill_gradientn(name="Absolute uncertainty (units?)",
                       colors = my_colors,
                       trans = "log10",
                       #limits = c(0,100),
                       breaks = my_breaks, 
                       labels = my_breaks) +
  
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

f4_fig

dpi=300    #pixels per square inch
tiff(here::here("Fig_4.tif"), width=10*dpi, height=5*dpi, res=dpi)

f4_fig
dev.off()



