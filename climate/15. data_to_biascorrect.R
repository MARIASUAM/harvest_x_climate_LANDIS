# GENERATE TABLE OF DATA TO BE CORRECTED

### SETUP ### 
library(dplyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

### DATA TO BE CORRECTED ###
# Locate climate scenario files to correct
all_files <- list.files(paste0(di, "inputs_climate"))
indexes <- c(1:16, 22:57)

# Generate table with data to correct
data_to_correct <- data.frame()
for (i in seq_along(indexes)) {
  print(all_files[indexes[i]])
  scenario <- strsplit(all_files[indexes[i]], split = "[[:punct:]]")[[1]][1]
  model <- strsplit(all_files[indexes[i]], split = "[[:punct:]]")[[1]][2]
  reg <- strsplit(strsplit(all_files[indexes[i]], split = "[[:punct:]]")[[1]][3],
                  split = "[[:alpha:]]")[[1]][4]
  temp <- read.table(paste0(di, "inputs_climate/", all_files[indexes[i]]), header = TRUE) %>%
    dplyr::select(Year, Month, Prec) %>%
    mutate(Scenario = scenario, 
           Model = model,
           Region = reg)
  
  data_to_correct <- rbind(data_to_correct, temp)
}

# Export
write.table(data_to_correct, paste0(di, "data/climate/data_to_biascorrect.txt"), sep = ";")
