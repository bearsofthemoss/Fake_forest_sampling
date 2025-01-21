


#  Cost idea ####

#How expensive is each strategy compared to the certainty it provides?


## wrap the panels by size classes.


#x axis shows cost for a particular strategy, y shows the probability of being <10

# value each tree.   Call it $1000 per tree


# Load in the size class proportions ####

library(readxl)
library(here)
library(tidyr)
library(ggplot2)

fd <- file.path(here::here(),"Data_folder","forest_distributions")

#(for figure 2)
sc<-  read_excel(
  file.path(fd,"Alex_re_arrange_4_29.xlsx"), sheet="size.class.ay")


head(sc)
gs<-gather(sc, "size.class","value",3:10)
gs$row<-as.factor(gs$row)
head(gs)
table(gs$Distribution)

gs[gs$Distribution=="Triangle shaped","Distribution"]<-"not used"
gs[gs$Distribution=="Truncated triangles left","Distribution"]<-"not used"
gs[gs$Distribution=="Truncated triangles right","Distribution"]<-"not used"


gs[gs$Distribution=="Triangular left","Distribution"]<- "not used" # was skewed left
gs[gs$Distribution=="Triangular right","Distribution"]<- "not used"# was skewed right


gs <- gs[!gs$Distribution=="not used", ]

gs[gs$Distribution=="Truncated uniform left","Distribution"]<- "Weighted left"
gs[gs$Distribution=="Truncated uniform right","Distribution"]<- "Weighted right"


gs[gs$Distribution=="Proportional uniform","Distribution"]<-"Uniform"
gs[gs$Distribution=="Forests in Mexico","Distribution"]<-"Reverse-J"
gs[gs$Distribution=="Proportional arbogast","Distribution"]<-"Even-aged distribution"


table(gs$Distribution)

gs$group<-paste(gs$row, gs$Distribution)
gs$Distribution<-factor(gs$Distribution, levels=c( "Reverse-J", "Even-aged distribution","Uniform",
                                                   "Parabolic","Weighted left","Weighted right"))

gs<-gs[gs$value>0,]
table(gs$size.class)

tail(gs)
gs$size.class[gs$size.class == "sc1"] <- "10-21"
gs$size.class[gs$size.class == "sc2"] <- "22-32"
gs$size.class[gs$size.class == "sc3"] <- "33-44"
gs$size.class[gs$size.class == "sc4"] <- "45-55"
gs$size.class[gs$size.class == "sc5"] <- "56-66"
gs$size.class[gs$size.class == "sc6"] <- "66-78"
gs$size.class[gs$size.class == "sc7"] <- "79-89"
gs$size.class[gs$size.class == "sc8"] <- "90-100"

table(gs$Distribution)

head(gs)


# Load proportion data ####

# proportion dataset
prop_path <- file.path(here::here(), "Data_folder","forest_distributions")

prop <- read.csv(file.path( prop_path , "prop_combined.csv"))
prop <- prop[,-1]

# Order the factors
prop$Sample <- factor(prop$Sample, 
                      levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

prop$dist_name<-factor(prop$distribution, levels=c( "Proportional","Parabolic",
                                                    "Skewed right","Skewed left",
                                                    "Truncated uniform left","Truncated uniform right"))


head(prop)

# order the levels
prop$Sample <- factor(prop$Sample, 
                      levels=c("n10","n15", "n30", "n45","n80","n130","n215","n360","n600","n1000"))

prop[prop$distribution=="Truncated uniform left", "distribution"] <- "Weighted left"
prop[prop$distribution=="Truncated uniform right", "distribution"] <- "Weighted right"

table(prop$dist_name)

prop$dist_name<-factor(prop$distribution, levels=c( "Proportional","Parabolic",
                                                    #                                                  "Skewed right","Skewed left",
                                                    "Weighted left","Weighted right"))


head(prop)

prop <- prop[!is.na(prop$dist_name), ]
prop$group <- paste(prop$Est, prop$dist_name)


prop$distribution <- factor(prop$distribution, levels=c("Proportional","Parabolic","Weighted left","Weighted right"))

head(prop)
table(prop$Diam_distribution, prop$distribution)

only_prop <- prop[prop$distribution=="Proportional",]
no_prop <-  prop[prop$distribution!="Proportional",]

dim(only_prop)
dim(no_prop)

table(only_prop$Diam_distribution)

# now make a parab, skew weighted left, and weighted right version
prop_para <- only_prop
prop_wL <- only_prop
prop_wR <- only_prop

# hide in prop as an est
prop_para$Est <- "Proportional"
prop_wL$Est <- "Proportional"
prop_wR$Est<- "Proportional"

# Now rename the distribution so it groups right
prop_para$distribution <- "Parabolic"
prop_wL$distribution <- "Weighted left"
prop_wR$distribution <- "Weighted right"


# combine in the proportional, now hidden as an 'Est'
use_prop <- rbind(no_prop, prop_para, prop_wL, prop_wR) 


use_prop$sel_color <- use_prop$Est=="Proportional"
use_prop[use_prop$sel_color=="FALSE","sel_color"] <- "Alternate sampling strategy"
use_prop[use_prop$sel_color=="TRUE","sel_color"] <- "Proportional to diameter distribution"


############

# Figure out how many trees of each size

head(gs)

# turn into proportion
gs$value <- gs$value /100
head(use_prop)
props <- spread(gs, "size.class", "value")



# Alex works on cost #### 

use_prop$ff_unit <- paste(use_prop$Diam_distribution,use_prop$dist_name,use_prop$Est)
tree_count <- use_prop[ , c("ff_unit","tree_count")]
head(tree_count)


# brute force it?
gs_Uni <- gs
gs_Arb <- gs
gs_J <- gs

gs_Uni$Diam_dist <- "Uni"
gs_Arb$Diam_dist <- "Arb"
gs_J$Diam_dist <- "J"

gs <- rbind(gs_Uni, gs_Arb, gs_J)


gs$ff_unit <- paste(gs$Diam_dist, gs$Distribution, gs$row)

head(tree_count)


head(gs)

