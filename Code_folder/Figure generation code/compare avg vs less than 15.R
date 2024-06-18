
library(dplyr)
library(here)
library(ggplot2)


# proportion dataset
prop_path <- file.path(here::here(), "Data_folder","New_model_run_proportion")

prop <- read.csv(file.path( prop_path , "prop_combined.csv"))
prop <- prop[,-1]

# Order the factors
prop$Sample <- factor(prop$Sample, 
                      levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

prop$dist_name<-factor(prop$distribution, levels=c( "Proportional","Parabolic",
                                                    "Skewed right","Skewed left",
                                                    "Truncated uniform left","Truncated uniform right"))

#################

# now average dataset


avg_path <- file.path(here::here(), "Data_folder","New_model_run_averages")


avg <- read.csv(file.path( avg_path , "avg_combined.csv"))
avg <- avg[,-1]


# order the levels
avg$Sample <- factor(avg$Sample, 
                     levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

avg$dist_name<-factor(avg$distribution, levels=c( "Proportional","Parabolic",
                                                  "Skewed right","Skewed left",
                                                  "Truncated uniform left","Truncated uniform right"))


###########


below <- aggregate(list(less_than15 = prop$p15) , by = list(dist_name = prop$dist_name,
                              number_trees = prop$tree_count,
                              Est = prop$Est,
                             Diam_distribution = prop$Diam_distribution,
                              model = prop$model
                             ),
          FUN="mean", na.rm=T)
below$id <- paste(below$model, below$Diam_distribution,below$Est, below$number_trees, below$dist_name, below$Diam_distribution)


ma <- aggregate(list(average = avg$value) , by = list(dist_name = avg$dist_name,
                                     number_trees = avg$tree_count,
                                     Est = avg$Est,
                                     model = avg$model,
                                     Diam_distribution = avg$Diam_distribution),
                 FUN="mean", na.rm=T)
ma$id <- paste(ma$model, ma$Diam_distribution,ma$Est,  ma$number_trees, ma$dist_name, ma$Diam_distribution)


head(ma)
head(below)

ma$less_than15 <- below$less_than15[match(ma$id, below$id)]



# Specify the starting and ending colors
# start_color <- "darkred"
# mid_color <- "lightgreen"
# end_color <- "darkgreen"
# 
# # Create a color palette with 9 colors
# color_palette <- colorRampPalette(c(start_color, mid_color, end_color))(10)

# Display the resulting color palette
print(color_palette)

head(ma)

ggplot(ma[ma$average< 5000,], aes(x=average, y=less_than15 , col=Est, shape=dist_name))+
  facet_grid(  Diam_distribution~ as.factor(number_trees), scales="free")+ geom_point()+
  
  #scale_color_manual(values = color_palette) +
  
  labs(x="Average uncertainty value (%)", y="Proportion of model runs < 15% uncertainty")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())



ggplot(ma[ma$average< 5000,], aes(x=average, y=less_than15 , col=dist_name, shape=Diam_distribution))+
 # facet_grid(  Diam_distribution~ as.factor(number_trees), scales="free")+ 
  geom_point()+
  
  #scale_color_manual(values = color_palette) +
  
  labs(x="Average uncertainty value (%?)", y="Proportion of model runs < 15% uncertainty")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())


head(avg)
