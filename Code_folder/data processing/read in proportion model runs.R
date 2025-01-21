library(dplyr)
library(here)
library(ggplot2)

datapath <- file.path( here::here(),"Data_folder","forest_distributions")


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

prop_combined<- rbind(
  m1Uni, m3Uni, m4Uni,
  m1J, m3J, m4J,
  m1Arb, m3Arb, m4Arb
)


################


# re-name the distribrution names
prop_combined[prop_combined$distribution=="Triangular left","distribution" ]<- "Skewed left"
prop_combined[prop_combined$distribution=="Triangular rigth","distribution" ]<- "Skewed right"
prop_combined[prop_combined$distribution=="Truncated Triangles","distribution" ]<- "Truncated triangle 16 X"
prop_combined[prop_combined$distribution=="Triangular Ushaped","distribution" ]<- "Triangular"


## add left and right truncated uniform
prop_combined[prop_combined$Est=="E1" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E2" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E3" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E4" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E5" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E6" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E7" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"
prop_combined[prop_combined$Est=="E8" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform left"

prop_combined[prop_combined$Est=="E9" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E10" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E11" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E12" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E13" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E14" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E15" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"
prop_combined[prop_combined$Est=="E16" & prop_combined$distribution=="Truncated Uniform","distribution"] <- "Truncated uniform right"


table(prop_combined$distribution)
# 
prop_combined$dist_name<-factor(prop_combined$distribution, levels=c( "Proportional","Parabolic",
                                                            "Skewed right","Skewed left",
                                                            "Truncated uniform left","Truncated uniform right","Triangular"))

# Removes U shaped triangle 
prop_combined <- prop_combined[!is.na(prop_combined$dist_name),]
summary(prop_combined$dist_name)


#
# take out 'E' from sampling strategy
prop_combined$Est <- as.numeric(gsub("E", "", prop_combined$Est))
prop_combined$tree_count<-prop_combined$Sample
prop_combined$Sample<- paste0("n",prop_combined$Sample)


prop_combined$Sample <- factor(prop_combined$Sample, 
                          levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

### Fix number for truncated uniform right

tur <- prop_combined[prop_combined$distribution=="Truncated uniform right", ]


tur[tur$Est==16,"Est"] <- 1
tur[tur$Est==15,"Est"] <- 2
tur[tur$Est==14,"Est"] <- 3
tur[tur$Est==13,"Est"] <- 4
tur[tur$Est==12,"Est"] <- 5
tur[tur$Est==11,"Est"] <- 6
tur[tur$Est==10,"Est"] <- 7
tur[tur$Est==9,"Est"] <- 8

prop_combined <- prop_combined[!prop_combined$distribution=="Truncated uniform right",]
prop_combined <- rbind(tur, prop_combined)



###############
write.csv(prop_combined, file.path( datapath,"prop_combined.csv"))



