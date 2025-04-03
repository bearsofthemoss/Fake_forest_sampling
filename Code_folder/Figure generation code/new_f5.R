
library(dplyr)
library(here)
library(ggplot2)


# proportion dataset
prop_path <- file.path(here::here(), "Data_folder","forest_distributions")

prop <- read.csv(file.path( prop_path , "prop_combined.csv"))
prop <- prop[,-1]

# Order the factors
prop$Sample <- factor(prop$Sample, 
                      levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

prop$dist_name<-factor(prop$distribution, levels=c( "Proportional",
                                                    #"Parabolic",
                                                    "Skewed right","Skewed left",
                                                    "Truncated uniform left","Truncated uniform right","Triangular"))

###########

head(prop)

# order the levels
prop$Sample <- factor(prop$Sample, 
                     levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

prop[prop$distribution=="Truncated uniform left", "distribution"] <- "Weighted left"
prop[prop$distribution=="Truncated uniform right", "distribution"] <- "Weighted right"

table(prop$dist_name)

prop$dist_name<-factor(prop$distribution, levels=c( "Proportional",
                                                    #"Parabolic",
#                                                  "Skewed right","Skewed left",
                                                  "Weighted left","Weighted right","Triangular"))

##############
head(prop)

prop <- prop[!is.na(prop$dist_name), ]
prop$group <- paste(prop$Est, prop$dist_name)


prop$distribution <- factor(prop$distribution, levels=c("Proportional",
                                                        #"Parabolic",
                                                        "Weighted left","Weighted right", "Triangular"))

head(prop)
table(prop$Diam_distribution, prop$distribution)

only_prop <- prop[prop$distribution=="Proportional",]
no_prop <-  prop[prop$distribution!="Proportional",]

dim(only_prop)
dim(no_prop)

table(only_prop$Diam_distribution)

# now make a parab, skew weighted left, and weighted right version
#prop_para <- only_prop
prop_wL <- only_prop
prop_wR <- only_prop
prop_tri <- only_prop

# hide in prop as an est
#prop_para$Est <- "Proportional"
prop_wL$Est <- "Proportional"
prop_wR$Est<- "Proportional"
prop_tri$Est<- "Proportional"

# Now rename the distribution so it groups right
#prop_para$distribution <- "Parabolic"
prop_wL$distribution <- "Weighted left"
prop_wR$distribution <- "Weighted right"
prop_tri$distribution <- "Triangular"

# combine in the proportional, now hidden as an 'Est'
use_prop <- rbind(no_prop,
                  # prop_para,
                  prop_wL, prop_wR, prop_tri) 


use_prop$sel_color <- use_prop$Est=="Proportional"
use_prop[use_prop$sel_color=="FALSE","sel_color"] <- "Alternate sampling strategy"
use_prop[use_prop$sel_color=="TRUE","sel_color"] <- "Proportional to diameter distribution"

f5_fig <- ggplot(use_prop[use_prop$model=="Model 1",], 
       aes(x=(tree_count), y= p5,col=Est, 
           shape= as.factor( Est)))+
  geom_point()+geom_line(aes(group=group))+
  facet_grid(Diam_distribution~distribution)+
  #scale_shape_manual(values=c( 8, 14,10, 12,  9,  3, 17, 15,19))+
  
  scale_colour_manual(name = "Sampling strategy",
                      values = c("black", "black","black","black","black",
                                 "black","black", "black","red")) +   
  scale_shape_manual(name = "Sampling strategy",
                     values = c( 8, 14,10, 12,  9,  3, 17, 15,19))+
  labs(x="Sample size", y="Probability of <10% difference")+
  scale_x_log10()+
  theme_bw()+theme(panel.grid = element_blank())+
  ggtitle("Model 1")


f5_fig

dpi=300    #pixels per square inch
tiff(here::here("Fig_5.tif"), width=10*dpi, height=5*dpi, res=dpi)
f5_fig
dev.off()
