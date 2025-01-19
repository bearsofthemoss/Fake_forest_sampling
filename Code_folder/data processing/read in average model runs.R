library(dplyr)
library(here)
library(ggplot2)

datapath <- file.path( here::here(),"Data_folder","forest_distributions")


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

avg_combined<- rbind(
  m1Uni, m3Uni, m4Uni,
  m1J, m3J, m4J,
  m1Arb, m3Arb, m4Arb
)

# Take out first column
avg_combined <- avg_combined[ , -1]

names(avg_combined)

# gather the columns of sample size
avg_combined <- gather(avg_combined, "tree_count","value",2:11 )


names(avg_combined)


###### Make graphs with the averages


# re-name the distribrution names
avg_combined[avg_combined$distribution=="Triangular left","distribution" ]<- "Skewed left"
avg_combined[avg_combined$distribution=="Triangular rigth","distribution" ]<- "Skewed right"
avg_combined[avg_combined$distribution=="Truncated Triangles","distribution" ]<- "Truncated triangle 16 X"
avg_combined[avg_combined$distribution=="Triangular Ushaped","distribution" ]<- "U-shaped triangle"


## add left and right truncated uniform
avg_combined[avg_combined$Est=="E1" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E2" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E3" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E4" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E5" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E6" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E7" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
avg_combined[avg_combined$Est=="E8" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"

avg_combined[avg_combined$Est=="E9" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E10" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E11" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E12" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E13" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E14" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E15" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
avg_combined[avg_combined$Est=="E16" & avg_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"


table(avg_combined$distribution)
# 
avg_combined$dist_name<-factor(avg_combined$distribution, levels=c( "Proportional","Parabolic",
                                                            "Skewed right","Skewed left",
                                                            "Truncated uniform left","Truncated uniform right"))

# Removes U shaped triangle 
avg_combined <- avg_combined[!is.na(avg_combined$dist_name),]


# take out 'E' from sampling strategy
avg_combined$Est <- as.numeric(gsub("E", "", avg_combined$Est))
avg_combined$tree_count<- as.numeric(gsub("X", "", avg_combined$tree_count))


avg_combined$Sample<- paste0("n",avg_combined$tree_count)



### Fix number for truncated uniform right

tur_avg <- avg_combined[avg_combined$distribution=="Truncated uniform right", ]



tur_avg[tur_avg$Est==16,"Est"] <- 1
tur_avg[tur_avg$Est==15,"Est"] <- 2
tur_avg[tur_avg$Est==14,"Est"] <- 3
tur_avg[tur_avg$Est==13,"Est"] <- 4
tur_avg[tur_avg$Est==12,"Est"] <- 5
tur_avg[tur_avg$Est==11,"Est"] <- 6
tur_avg[tur_avg$Est==10,"Est"] <- 7
tur_avg[tur_avg$Est==9,"Est"] <- 8


avg_combined <- avg_combined[!avg_combined$distribution=="Truncated uniform right",]
avg_combined <- rbind(tur_avg, avg_combined)




write.csv(avg_combined,  file.path( datapath,"avg_combined.csv"))
