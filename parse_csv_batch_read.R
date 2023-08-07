combined_data <- data.frame()

# Set the directory where your CSV files are located
arb_directory <- "Data_folder/Arboghat/"

# Get the list of CSV files in the directory
arb_files <- list.files(arb_directory, pattern = ".csv", full.names = TRUE)
arb_files
# Loop through each CSV file
#for (i in 1:length(arb_files)) {
i=2
  # Read the CSV file and add a new column with the file name
  data <- read.csv(arb_files[i])
  data$Source <- substring(arb_files[i], first=22)
  data$name<-   substr(data$Source, 1, nchar(data$Source) - 4) 
  data$model<-tail(strsplit(data$name, "_")[[1]], n = 1)
  

    
  # Combine the data with the existing combined data using rbind
  combined_data <- rbind(combined_data, data)
}

# Print the combined data
print(combined_data)
table(combined_data$Source)
