# Generate climate series for 2100-2200

di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/inputs_climate/"

library(dplyr)

# Generate random series among the years 2080-2100
new_years <- data.frame(Year = c(2101:2200))
random_series <- data.frame(Sim_year = c(2101:2200),
                            Orig_year = sample(c(2080:2100), length(2101:2200), replace = TRUE))

# Load series 1800-2100
scenarios <- c("current", "rcp45", "rcp85")
models <- c("ACCESS", "MIROC5", "CESM1", "CMCC")
regions <- c("reg1", "reg2", "reg3", "reg4")

files <- data.frame()
for (i in 1:length(scenarios)) {
  for (j in 1:length(models)) {
    for (h in 1:length(regions)) {
      files <- rbind(files,
                     data.frame(Scenario = scenarios[i],
                                Model = models[j],
                                Region = regions[h],
                                file_name = paste(scenarios[i], "_", models[j], "_", regions[h], ".txt", sep = "")))
    }
  }
}

# Load time series
for (i in 1:length(files$Scenario)) {
  temp <- read.table(paste(di, files$file_name[i], sep = ""), header = TRUE)
  
  new_series <- random_series %>%
    left_join(temp, by = c("Orig_year" = "Year")) %>%
    rename(Year = Sim_year) %>%
    select(-Orig_year)
  
  whole_series <- rbind(temp, new_series)
  
  write.table(whole_series, paste(di, "series_1800-2200_", files$file_name[i], sep = ""))
}

