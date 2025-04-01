
library(dplyr)
library(here)
library(ggplot2)

fd <- file.path(here::here(),"Data_folder","forest_distributions")
J_forest<-read.csv(file.path(fd,"fakeforest_arbogast_modified.csv"))

f1_bar<-ggplot(data=J_forest, aes(x=grp)) +
  geom_bar() +
  geom_text(stat='count', aes(label=after_stat(count), vjust=-1))

f1_bar
