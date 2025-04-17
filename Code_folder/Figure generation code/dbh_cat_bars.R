library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)

# Looks like this is Fig 1 in the paper

# Define the file path and read the data
fd <- file.path(here::here(), "Data_folder", "forest_distributions")
arb_forest <- read.csv(file.path(fd, "fakeforest_arbogast_modified.csv"))
uni_forest <- read.csv(file.path(fd, "fakeforest_uniform_modified.csv"))
J_forest <- read.csv(file.path(fd, "fakeforest_J_modified.csv"))

A_biomass <- read.csv(file.path(fd, "arbogast_biogrp.csv"))
U_biomass <- read.csv(file.path(fd, "uniform_biogrp.csv"))
J_biomass <- read.csv(file.path(fd, "J_biogrp.csv"))

# Calculate counts for each group in the forest data
A_counts <- arb_forest %>% 
  count(grp)
U_counts <- uni_forest %>% 
  count(grp)
J_counts <- J_forest %>% 
  count(grp)

# Join the counts with the biomass data using the common group variable
# (Assuming that biomass has a column named 'grp' and a column 'percent')
A_counts <- left_join(A_counts, A_biomass, by = "grp")
U_counts <- left_join(U_counts, U_biomass, by = "grp")
J_counts <- left_join(J_counts, J_biomass, by = "grp")

# Define a common expansion to avoid clipping at the top
y_expansion <- scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

## Arbogast graph
A_bar <- ggplot(data = A_counts, aes(x = grp, y = n)) +
  theme_classic() +
  ggtitle("Arbogast distribution") +
  xlab("") +
  ylab("Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 8),
    aspect.ratio = 0.4
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -1, size = 3) +
  y_expansion

## Uniform graph
U_bar <- ggplot(data = U_counts, aes(x = grp, y = n)) +
  theme_classic() +
  ggtitle("Uniform distribution") +
  xlab("") +
  ylab("Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 8),
    aspect.ratio = 0.4
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -1, size = 3) +
  y_expansion

## J forest graph
J_bar <- ggplot(data = J_counts, aes(x = grp, y = n)) +
  theme_classic() +
  ggtitle("J distribution") +
  xlab("") +
  ylab("Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 8),
    aspect.ratio = 0.4
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -1, size = 3) +
  y_expansion

## Merge into one figure with specified relative heights
ggarrange(J_bar, A_bar, U_bar, ncol = 1, heights = c(5, 5, 5))
