library(dplyr)
library(here)
library(ggplot2)

wd <- here::here()

getwd()

datapath <- file.path(wd, "Data_folder","New_model_run_averages","AverageHeatMap")


read_and_label <- function(folder, model, diam_dist) {
  file_names <- c("Parabolic", "Proportional", "Triangular_left", "Triangular_rigth", 
                  "Triangular_Ushaped", "Truncated_Triangles", "Truncated_Uniform")
  
  dfs <- lapply(file_names, function(name) {
    file_path <- file.path(datapath, paste0(folder, "_", diam_dist, "_resultsModel", model), 
                           paste0("TableMean_ ", name, " .csv"))
    if(file.exists(file_path)) {
      df <- read.csv(file_path, skip = 1)
      df$distribution <- gsub("_", " ", name)
      return(df)
    } else {
      warning(paste("File not found:", file_path))
      return(NULL)
    }
  })
  
  do.call(rbind, dfs) %>%
    mutate(model = paste0("Model ", model),
           Diam_distribution = diam_dist)
}

# function(folder, model, diam_dist)
m1Uni <- read_and_label("1", "1", "Uni")
m3Uni <- read_and_label("2", "3", "Uni")
m4Uni <- read_and_label("3", "4", "Uni")

m1J <- read_and_label("4", "1", "J")
m3J <- read_and_label("5", "3", "J")
m4J <- read_and_label("6", "4", "J")

m1Arb <- read_and_label("7", "1", "Arb")
m3Arb <- read_and_label("8", "3", "Arb")
m4Arb <- read_and_label("9", "4", "Arb")

combined<- rbind(
  m1Uni, m3Uni, m4Uni,
  m1J, m3J, m4J,
  m1Arb, m3Arb, m4Arb
)

#write.csv(combined, file.path(datapath, "combined.csv"))




###### Make graphs with the averages


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




m1Uni
table(m1Uni$distribution)
#mu <- combined[combined$distribution=="Truncated Uniform",]

mu <- combined
head(mu)

library(tidyr)
mug <- gather(mu[ , -1], "number_trees", "value", 3:11)


mug$number_trees <- substring(mug$number_trees, 2)

mug$number_trees<- paste0("n", mug$number_trees)  
  
mug$number_trees <- factor(mug$number_trees, 
                    levels=c("n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

mug$Est <- factor(mug$Est, 
                           levels=c("E1", "E2", "E3","E4","E5","E6","E7","E8",
                                    "E9", "E10", "E11","E12","E13","E14","E15","E16"))


# 
# color_scale <- (colorRampPalette(c('darkred','darkred','darkred','red','red', 'yellow','yellow', 'orange', '#78C679', '#41AB5D', '#238443','forestgreen',"darkgreen"))(20))
# 

summary(mug$value)
# Define the colors and breakpoints
my_colors <- rev(c("darkred", "red", "orange", "yellow", "lightgreen", "green", "darkgreen","darkgreen"))
my_breaks = c(5,1,50, 200,10000)



mug$distribution<-factor(mug$distribution, levels=c( "Proportional","Parabolic",
                                                            "Skewed right","Skewed left",
                                                            "Truncated uniform left","Truncated uniform right"))

# Removes U shaped triangle 

g2 <- ggplot(mug , aes(x=number_trees, y=Est))+
  geom_tile(aes(fill=value), colour=NA)+
  scale_fill_gradientn(name="Average uncertainty value (%)",colors = my_colors,trans = "log10", breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Number of trees") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted")) +
  theme(panel.background = element_rect( colour ="black", fill ="white" )) +
facet_grid(distribution ~ Diam_distribution , scales="free_y",
           labeller = label_wrap_gen(width=10),
           space="free")+
theme(plot.title = element_text(size = 25, face = "bold"),
      legend.position = "bottom")+
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))



library(ggpubr)
ggarrange(g1, g2, nrow=1 )
