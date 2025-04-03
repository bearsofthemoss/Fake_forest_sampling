### Add DBH size class column for ease of use later

dat_path <- file.path(here::here(), "Data_folder", "forest_distributions")
# Read your data file
dat <- read.csv(file.path( dat_path , "fakeforest_J.csv"))

# Calculate the maximum DBH to define our intervals
max_dbh <- max(dat$dbh, na.rm = TRUE)

# Create breaks starting just below 10 and going above max_dbh in increments of 10.
# Using 9.5 and 20.5 ensures that dbh values 10 through 20 fall in the first interval.
breaks <- seq(10, 100, by = 10)

# Create group labels like "grp1", "grp2", ...
group_labels <- paste0("grp", 1:(length(breaks) - 1))
group_labels <-list('10-20','20-30','30-40','40-50','50-60','60-70','70-80','80-90','90-100')
# Add a new column 'grp' that groups dbh values into the defined intervals.
dat$grp <- cut(dat$dbh, 
               breaks = breaks, 
               labels = group_labels, 
               include.lowest = TRUE)

# Check the first few rows
head(dat)
write.csv(dat, file.path(dat_path,"fakeforest_J_modified.csv"), row.names = FALSE)


## Calc percent biomass for each DBH range ##
grouped_biomass <- aggregate(biomass ~ grp, data = dat, FUN = sum, na.rm = TRUE)
tot_bio <- sum(grouped_biomass$biomass)
grouped_biomass$percent <- (grouped_biomass$biomass / tot_bio) * 100
write.csv(grouped_biomass, file.path(dat_path,"J_biogrp.csv"), row.names = FALSE)
