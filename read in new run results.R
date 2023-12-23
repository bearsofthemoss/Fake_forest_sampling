library(dplyr)
library(here)
library(ggplot2)

wd <- here::here()

getwd()

datapath <- file.path(wd, "Data_folder","New_model_run_Nov_2023")


read_and_label <- function(folder, model, diam_dist) {
  file_names <- c("Parabolic", "Proportional", "Triangular_left", "Triangular_rigth", 
                  "Triangular_Ushaped", "Truncated_Triangles", "Truncated_Uniform")
  
  dfs <- lapply(file_names, function(name) {
    file_path <- file.path(datapath, paste0(folder, "_", diam_dist, "_resultsModel", model), 
                           paste0("TableProb_ ", name, " .csv"))
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



m1Uni
table(m1Uni$distribution)
tu <- combined[combined$distribution=="Truncated Uniform",]
table(tu$Sample)

tu$Sample
tu$Est <- as.numeric(gsub("E", "", tu$Est))
tu$Sample<- paste0("n",tu$Sample)
tu$Sample <- factor(tu$Sample, 
                          levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

color_scale <- (colorRampPalette(c('darkred','darkred','darkred','red','red', 'yellow','yellow', 'orange', '#78C679', '#41AB5D', '#238443','forestgreen',"darkgreen"))(20))



# Define the colors and breakpoints
my_colors <- c("darkred", "red", "orange", "yellow", "lightgreen", "green", "darkgreen")
my_breaks = c(1,5,10,25,50,90)

str(tu)



ggplot(tu , aes(x=Sample, y=Est))+
  geom_tile(aes(fill=p5*100), colour=NA)+
  scale_fill_gradientn(name="Percent of simulations with <5% difference",colors = my_colors, trans = "log10",breaks = my_breaks, labels = my_breaks) +
  ylab("Sampling strategy")+ xlab("Sample size (number of trees)") +
  theme(panel.grid.minor = element_line( colour ="black", linetype ="dotted", size = 0.5)) +
  theme(panel.background = element_rect( colour ="black", fill ="white" ,size = 0.5 )) +
  theme(plot.title = element_text(size = 25, face = "bold"))+
  #facet_grid(distribution ~ Diam_distribution, scales="free_y", 
   #          labeller = label_wrap_gen(width=10),
    #         space="free")+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))+
  theme(plot.title = element_text(hjust = 0.5),strip.text.y = element_text(size = 8, angle = 0))+
  scale_y_reverse()
