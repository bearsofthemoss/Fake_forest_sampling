library(dplyr)
library(here)
library(ggplot2)

# proportion dataset
prop_path <- file.path(here::here(), "Data_folder", "forest_distributions")
prop <- read.csv(file.path(prop_path, "prop_combined.csv"))
prop <- prop[,-1]

# Order the factors
prop$Sample <- factor(prop$Sample, 
                      levels = c("n10", "n15", "n30", "n45", "n80", "n130", "n215", "n360", "n600", "n1000"))

prop$dist_name <- factor(prop$distribution, 
                         levels = c("Proportional", "Parabolic", "Skewed right", "Skewed left",
                                    "Truncated uniform left", "Truncated uniform right", "Triangular"))

# Reorder the factor for Sample (again)
prop$Sample <- factor(prop$Sample, 
                      levels = c("n10", "n15", "n30", "n45", "n80", "n130", "n215", "n360", "n600", "n1000"))

# Rename distributions for clarity
prop[prop$distribution == "Truncated uniform left", "distribution"] <- "Weighted left"
prop[prop$distribution == "Truncated uniform right", "distribution"] <- "Weighted right"

table(prop$dist_name)

# Reset the levels for dist_name
prop$dist_name <- factor(prop$distribution, 
                         levels = c("Proportional", "Parabolic", "Skewed right", "Skewed left", "Triangular"))

# Remove any rows with NA for dist_name and create a grouping variable
prop <- prop[!is.na(prop$dist_name), ]
prop$group <- paste(prop$Est, prop$dist_name)

# Set the order of distribution factor
prop$distribution <- factor(prop$distribution, 
                            levels = c("Proportional", "Parabolic", "Skewed left", "Skewed right", "Triangular"))

# Split data into proportional and non-proportional parts
only_prop <- prop[prop$distribution == "Proportional", ]
no_prop <- prop[prop$distribution != "Proportional", ]

# Create modified versions of only_prop for the different distributions
prop_sL <- only_prop
prop_sR <- only_prop
prop_tri <- only_prop
prop_Para <- only_prop

prop_sL$Est <- "Proportional"
prop_sR$Est <- "Proportional"
prop_tri$Est <- "Proportional"
prop_Para$Est <- "Proportional"

prop_Para$distribution <- "Parabolic"
prop_sL$distribution <- "Skewed left"
prop_sR$distribution <- "Skewed right"
prop_tri$distribution <- "Triangular"

# Combine all data into one dataset
use_prop <- rbind(no_prop, prop_Para, prop_sL, prop_sR, prop_tri)

use_prop$sel_color <- use_prop$Est == "Proportional"
use_prop[use_prop$sel_color == "FALSE", "sel_color"] <- "Alternate sampling strategy"
use_prop[use_prop$sel_color == "TRUE", "sel_color"] <- "Proportional to diameter distribution"

# Define the models for which you want to produce plots
models <- c("Model 1", "Model 3", "Model 4")
dpi <- 300  # pixels per inch

# Loop over each model to generate and save the plot
for (mod in models) {
  # Filter data for the current model
  plot_data <- subset(use_prop, model == mod)
  
  # Create the plot
  f5_fig <- ggplot(plot_data, 
                   aes(x = tree_count, y = p5, col = Est, shape = as.factor(Est))) +
    geom_point() +
    geom_line(aes(group = group)) +
    facet_grid(Diam_distribution ~ distribution) +
    scale_colour_manual(name = "Sampling strategy",
                        values = c("black", "black", "black", "black", "black",
                                   "black", "black", "black", "red")) +
    scale_shape_manual(name = "Sampling strategy",
                       values = c(8, 14, 10, 12, 9, 3, 17, 15, 19)) +
    labs(x = "Sample size", y = "Probability of <10% difference") +
    scale_x_log10() +
    theme_bw() +
    theme(panel.grid = element_blank(), axis.text = element_text(size = 7)) +
    ggtitle(mod)
  
  # Create a filename that includes the model name (replacing spaces with underscores)
  file_name <- file.path(here::here(), paste0("Fig_5_skew_", gsub(" ", "_", mod), ".tif"))
  
  # Save the plot as a TIFF file
  tiff(file_name, width = 7 * dpi, height = 6 * dpi, res = dpi)
  print(f5_fig)
  dev.off()
}

