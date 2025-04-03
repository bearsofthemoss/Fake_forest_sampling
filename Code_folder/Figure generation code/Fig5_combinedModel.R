library(dplyr)
library(here)
library(ggplot2)

# Load the dataset
prop_path <- file.path(here::here(), "Data_folder", "forest_distributions")
prop <- read.csv(file.path(prop_path, "prop_combined.csv"))
prop <- prop[,-1]

# Order the Sample factor
prop$Sample <- factor(prop$Sample, 
                      levels = c("n10", "n15", "n30", "n45", "n80", "n130", "n215", "n360", "n600", "n1000"))

# Set up the distribution factor
prop$dist_name <- factor(prop$distribution, 
                         levels = c("Proportional", "Skewed right", "Skewed left",
                                    "Truncated uniform left", "Truncated uniform right", "Triangular"))
prop$Sample <- factor(prop$Sample, 
                      levels = c("n10", "n15", "n30", "n45", "n80", "n130", "n215", "n360", "n600", "n1000"))

# Rename distributions for clarity
prop[prop$distribution == "Truncated uniform left", "distribution"] <- "Weighted left"
prop[prop$distribution == "Truncated uniform right", "distribution"] <- "Weighted right"

# Reset the levels for dist_name with only the distributions we need
prop$dist_name <- factor(prop$distribution, 
                         levels = c("Proportional", "Skewed right", "Skewed left", "Triangular"))

# Remove rows with NA in dist_name and create a grouping variable
prop <- prop[!is.na(prop$dist_name), ]
prop$group <- paste(prop$Est, prop$dist_name)

# Order the distribution factor
prop$distribution <- factor(prop$distribution, 
                            levels = c("Proportional", "Skewed left", "Skewed right", "Triangular"))

# Split data into proportional and non-proportional parts
only_prop <- prop[prop$distribution == "Proportional", ]
no_prop <- prop[prop$distribution != "Proportional", ]

# Create modified versions for different distributions using the proportional data
prop_sL <- only_prop
prop_sR <- only_prop
prop_tri <- only_prop

prop_sL$Est <- "Proportional"
prop_sR$Est <- "Proportional"
prop_tri$Est <- "Proportional"

prop_sL$distribution <- "Skewed left"
prop_sR$distribution <- "Skewed right"
prop_tri$distribution <- "Triangular"

# Combine the datasets
use_prop <- rbind(no_prop, prop_sL, prop_sR, prop_tri)

# Create a column for sampling strategy (optional)
use_prop$sel_color <- use_prop$Est == "Proportional"
use_prop[use_prop$sel_color == "FALSE", "sel_color"] <- "Alternate sampling strategy"
use_prop[use_prop$sel_color == "TRUE", "sel_color"] <- "Proportional to diameter distribution"

# Subset the data for the three models of interest
selected_models <- c("Model 1", "Model 3", "Model 4")
plot_data <- subset(use_prop, model %in% selected_models)

dpi <- 300
update_geom_defaults("point", list(size = .1))
# Create one combined plot with all Models superimposed
combined_plot <- ggplot(plot_data, aes(x = tree_count, y = p5, color = model)) +
  geom_point() + 
  # CHANGED: Set smaller point size to make symbols smaller
  geom_point(size = .1) +
  # CHANGED: Added size = 0.5 for thinner lines and used interaction for proper grouping
  geom_line(aes(group = interaction(model, group)), linewidth = 0.1) +
  facet_grid(Diam_distribution ~ distribution) +
  # CHANGED: Set custom colors: Model 1 as black, Model 3 as medium gray, Model 4 as light gray
  scale_color_manual(values = c("Model 1" = "black", 
                                "Model 3" = "gray50", 
                                "Model 4" = "gray80"),
                     name = "Model") +
  labs(x = "Sample size", y = "Probability of <10% difference") +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Combined Models: Model 1, Model 3, Model 4")

# Save the combined plot as a TIFF file
file_name <- file.path(here::here(), "Fig_5_skew_combined.tif")
tiff(file_name, width = 10 * dpi, height = 5 * dpi, res = dpi)
print(combined_plot)
dev.off()

# Optionally, display the plot in your R session
combined_plot
