# Generate climate time series for each climate model and each scenario

# Packages
library(ncdf4)
library(raster)
library(dplyr)

# Setup folders
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/"
folder_harddisk <- "/Volumes/Data/Climatic_Data/EnviDat/envicloud/chelsa/chelsa_V1/chelsa_cmip5_ts/"

# Load reference maps
climate_regions <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/output_files/ecoregions/try_6_20200403/20200406_4_4_clustering.tif")
aoi <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/output_files/study_area_raster_3042.asc", sep = "")

# Create accessory objects
months <- c(1:12)
df <- data.frame(Climate_region = climate_regions[]) 

# Create reference table

ref_table <- read.table(paste(di, "envidat_paths.txt", sep = ""), header = FALSE)
ref_table$V1 <- as.character(ref_table$V1)

ref_table$file_name <- c()
ref_table$model <- c()
ref_table$variable <- c()
ref_table$scenario <- c()
ref_table$temporary <- c()
ref_table$period <- c()
ref_table$start_year <- c()
ref_table$end_year <- c()

for(i in 1:length(ref_table$V1)) {
  ref_table$file_name[i] <- strsplit(ref_table$V1[i], "/")[[1]][8]
  ref_table$model[i] <- strsplit(as.character(ref_table$file_name[i]), split = "_")[[1]][3]
  ref_table$variable[i] <- strsplit(as.character(ref_table$file_name[i]), split = "_")[[1]][2]
  ref_table$scenario[i] <- strsplit(as.character(ref_table$file_name[i]), split = "_")[[1]][4]
  ref_table$temporary[i] <- strsplit(as.character(ref_table$file_name[i]), split = "_")[[1]][5]
  ref_table$start_year[i] <- strsplit(as.character(ref_table$temporary[i]), split = '-')[[1]][1]
  ref_table$period[i] <- strsplit(as.character(ref_table$temporary[i]), split = "[.]")[[1]][1]
  ref_table$end_year[i] <- strsplit(as.character(ref_table$period[i]), split = '-')[[1]][2]
}

!!Define subset to process: 
files_df <- ref_table %>% 
  filter(variable == "tasmin") %>% # variable (pr, tasmin, tasmax)
  filter(model == "CESM1-BGC") # "ACCESS1-3", "CESM1-BGC", "CMCC-CM", "MIROC5")

# Problems: CHELSAcmip5ts_tasmax_CESM1-BGC_rcp45_2050-2069_V1.1.nc 
Change varname in line 75 and colnames in line 100

# Execute loop
for(j in 1:length(files_df$V1)) {
  print("j")
  print(j)
  print("of")
  print(length(files_df$V1))
  
  # Create df for data
  my_stack_df <- data.frame() # Create df for data
  
  # Identify file name
  file_name <- files_df$file_name[j] 
  print("File name: ")
  print(file_name)
  
  # Identify time period and create time table
  file_years <- c(files_df$start_year[j]:files_df$end_year[j])
  file_dates <- merge(months, file_years)
  
  # Load file as stack
  my_stack <- stack(paste(folder_harddisk, "cropped_", file_name, sep = ""), varname = "air_temperature") # change to precipitation_flux/air_temperature when needed
  print("stack loaded")
  my_stack <- projectRaster(from = my_stack, to = climate_regions) # Reproject and resample with climregions
  my_stack <- my_stack * aoi # Mask with aoi
  print("stack reprojected and masked")
  
  # Extract values for climate regions and group in variable table
  for (k in 1:dim(my_stack)[3]) {
    # print("second loop: k")
    # print(k)
    # print("of")
    # print(dim(my_stack)[3])
    temp <- my_stack[[k]]
    temp_df <- df
    temp_df$pr <- temp[]
    temp_df <- temp_df %>%
      group_by(Climate_region) %>%
      summarise_all(mean, na.rm = TRUE) %>% # FALSE?
      filter(is.na(Climate_region) == FALSE)
    temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
    temp_df$Year <- file_dates$y[k]
    temp_df$Month <- file_dates$x[k]
    my_stack_df <- rbind(my_stack_df, temp_df)
  }
  
  # colnames(my_stack_df) <- c("prec_region_1", "prec_region_2", "prec_region_3", "prec_region_4", "Year", "Month")
  colnames(my_stack_df) <- c("tmin_region_1", "tmin_region_2", "tmin_region_3", "tmin_region_4", "Year", "Month")
  # colnames(my_stack_df) <- c("tmax_region_1", "tmax_region_2", "tmax_region_3", "tmax_region_4", "Year", "Month")
  
  # Export table
  name_table <- paste(files_df$model[j], 
                      files_df$variable[j], 
                      files_df$scenario[j], 
                      files_df$period[j], sep = "_")
  write.table(my_stack_df, file = paste("/Volumes/Data/Climatic_Data/EnviDat/envicloud/chelsa/chelsa_V1/climregions_tables/", name_table, ".txt", sep = ""), row.names = FALSE)
  print("exporting table")
}
