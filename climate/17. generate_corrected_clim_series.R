## Regenerate climate files for LANDIS

### SETUP ###
library(dplyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

# Load precipitation bias-corrected data
prec <- read.table(paste0(di, "data/climate/data_biascorrected2.txt"), sep = ";", header = TRUE) %>%
  select(-X, -quant_prec, -Prec) %>%
  dplyr::rename(Prec = prec_corr) %>%
  mutate(Region = as.character(Region))

# Load other climate variables
all_files <- list.files(paste0(di, "inputs_climate"))
indexes <- c(1:16, 22:57)

clim_series <- data.frame()
for (i in seq_along(indexes)) {
  print(all_files[indexes[i]])
  scenario <- strsplit(all_files[indexes[i]], split = "[[:punct:]]")[[1]][1]
  model <- strsplit(all_files[indexes[i]], split = "[[:punct:]]")[[1]][2]
  reg <- strsplit(strsplit(all_files[indexes[i]], split = "[[:punct:]]")[[1]][3],
                  split = "[[:alpha:]]")[[1]][4]
  temp <- read.table(paste0(di, "inputs_climate/", all_files[indexes[i]]), header = TRUE) %>%
    dplyr::select(-Prec) %>%
    mutate(Scenario = scenario, 
           Model = model,
           Region = reg)
  clim_series <- rbind(clim_series, temp)
}

# Merge precipitation and other climate variables
full_clim <- full_join(clim_series, prec) %>%
  relocate(Year, Month, TMax, TMin, Prec, PAR, CO2, Scenario, Model, Region)

# Split and export
regions <- c("1", "2", "3", "4")
scenarios <- c("current", "newcurrent", "rcp45", "rcp85")
models <- c("ACCESS", "CESM1", "CMCC", "MIROC5")

for(i in seq_along(regions)) {
  for(j in seq_along(scenarios)) {
    for(h in seq_along(models)){
      series <- full_clim %>%
        filter(Region == regions[i],
               Scenario == scenarios[j],
               Model == models[h]) %>%
        select(-Region, -Scenario, -Model)
      
      file_name <- paste(scenarios[j], "_", models[h], "_reg", regions[i], "_biascorrected.txt", sep = "")
      write.table(series, paste0(di, "inputs_climate_adjusted/", file_name), row.names = FALSE)
    }
  }
}
