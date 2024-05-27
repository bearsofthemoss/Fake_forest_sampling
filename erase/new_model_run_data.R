
library(here)
library(reshape2)
library(ggplot2)
library(scales)


wd <- here::here()
wd
datapath <- file.path(wd, "Data_folder","New_model_run_Nov_2023")

### For first folder
parabolic <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "1_Uni_resultsModel1","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 1"
dists$Diam_distribution <- "Uniform"
m1Uni <- dists


# For folder 2
parabolic <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "2_Uni_resultsModel3","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 3"
dists$Diam_distribution <- "Uniform"
m3Uni <- dists

# For folder 3
parabolic <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "3_Uni_resultsModel4","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 4"
dists$Diam_distribution <- "Uniform"
m4Uni <- dists

#######
Uni <- rbind(m1Uni, m3Uni, m4Uni)


### For folder 4
parabolic <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "4_J_resultsModel1","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 1"
dists$Diam_distribution <- "Reverse-J"
m1J <- dists


# For folder 5
parabolic <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "5_J_resultsModel3","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 3"
dists$Diam_distribution <- "Reverse-J"
m3J <- dists

# For folder 6
parabolic <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "6_J_resultsModel4","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 4"
dists$Diam_distribution <- "Reverse-J"
m4J <- dists

#######
J <- rbind(m1J, m3J, m4J)


##############################################

### For folder 7
parabolic <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "7_Arb_resultsModel1","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 1"
dists$Diam_distribution <- "Arb"
m1Arb <- dists


# For folder 8
parabolic <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "8_Arb_resultsModel3","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 3"
dists$Diam_distribution <- "Arb"
m3Arb <- dists

# For folder 9
parabolic <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Parabolic .csv"),skip=1)
proportional <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Proportional .csv"),skip=1)
skewL <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Triangular_left .csv"),skip=1)
skewR <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Triangular_rigth .csv"),skip=1)
UshapeTriangle <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Triangular_Ushaped .csv"),skip=1)
truncTriangle <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Truncated_Triangles .csv"),skip=1)
truncUniform <-read.csv(file.path(datapath, "9_Arb_resultsModel4","TableProb_ Truncated_Uniform .csv"),skip=1)

parabolic$distribution <- "Parabolic"
proportional$distribution <- "Proportional"
skewL$distribution <- "Skewed left"
skewR$distribution <- "Skewed right"
UshapeTriangle$distribution <- "U-shaped triangle"
truncTriangle$distribution <- "Truncated triangle"
truncUniform$distribution <- "Truncated uniform"

dists <- rbind(parabolic, proportional, skewL, skewR, UshapeTriangle, truncTriangle, truncUniform)
dists$model <- "Model 4"
dists$Diam_distribution <- "Arb"
m4Arb <- dists

#######
Arb <- rbind(m1Arb, m3Arb, m4Arb)


##############################################

combined <- rbind(Uni, J, Arb)


table(combined$Diam_distribution)
table(combined$model)
table(combined$distribution)
table(combined$Sample)
head(combined)

###################



# cm$distribution<-factor(cm$distribution, levels=c( "Proportional","Parabolic",
#                                                    "Skewed right","Skewed left",
#                                                    "Truncated uniform left","Truncated uniform right"))

# order the distributions
combined$distribution<-factor(combined$distribution, levels=c( "Proportional","Parabolic",
                                                   "Skewed right","Skewed left",
                                                   "Truncated triangle","Truncated uniform", "U-shaped triangle"))





my_breaks = c(1, 10,  50,  90)
color_scale <- rev(colorRampPalette(c('darkred','red', 'yellow', 'orange', '#78C679', '#41AB5D', '#238443', 'forestgreen',"darkgreen"))(8))

combined$Est <- as.numeric(gsub("E", "", combined$Est))
combined$Sample<- paste0("n",combined$Sample)


combined$Sample <- factor(combined$Sample, 
  levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

summary(combined$p5)
### 
ggplot(combined , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=p5*100), colour=NA)+
  scale_fill_gradientn(name="Absolute Uncertainty (%)",colors = color_scale, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  facet_grid(distribution ~ Diam_distribution, scales="free_y", 
             labeller = label_wrap_gen(width=10),
             space="free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_continuous(breaks= seq(1,8,1))+
  theme(text = element_text(family = "Calibri"))

