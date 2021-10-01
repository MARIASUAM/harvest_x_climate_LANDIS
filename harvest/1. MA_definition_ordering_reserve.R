# Rasterize classification of Management Areas based on ordered vs non ordered areas, plus reserve areas

library(rgdal)
library(raster)
library(dplyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/management/"
di_experiments <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/simulations/inputs_management/"

# Load shape 1
zones_reserve <- readOGR(dsn = "/Users/maria.suarez.munoz/Documents/Mapas/01_PORN", layer = "zones_A_all_PA")
crs(zones_reserve)
# plot(zones_reserve)

# Load shape 2
zones_MMPP <- readOGR(dsn = di, layer = "municipios_con_montes")
crs(zones_MMPP)
# plot(zones_MMPP)

# Load reference rasters
aoi <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/output_files/study_area_raster_3042.asc")
active_cells <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/output_files/LANDIS_ecoregions_map.tif")
active_cells[active_cells != 0] <- 1 # Active cells

# Create empty raster
res(aoi)
gridsize <- 100
empty_raster <- raster(extent(aoi), res = gridsize, vals = NA)

# Create attributes and rasterize shape 1 and mask by inactive cells
zones_reserve@data <- zones_reserve@data %>%
  mutate(Reserve = 1) # 1 = TRUE
zones_reserve_raster <- rasterize(zones_reserve, empty_raster, field = zones_reserve@data[,14])
crs(zones_reserve_raster) <- crs(zones_reserve)
zones_reserve_raster[is.na(zones_reserve_raster) == TRUE] <- 2 # Non reserve
# plot(zones_reserve_raster)
zones_reserve_raster <- zones_reserve_raster * active_cells
# plot(zones_reserve_raster)

# Create attributes, rasterize shape 2 and mask by inactive cells
zones_MMPP@data <- zones_MMPP@data %>%
  mutate(MA_type = 1) # Areas under planning ("ordered") = 1
zones_MMPP_raster <- rasterize(zones_MMPP, empty_raster, field = zones_MMPP@data[,3])
crs(zones_MMPP_raster) <- crs(zones_MMPP)
zones_MMPP_raster[is.na(zones_MMPP_raster) == TRUE] <- 2 # Areas outside planning ("not ordered") = 2
# plot(zones_MMPP_raster)
zones_MMPP_raster <- zones_MMPP_raster * active_cells
# plot(zones_MMPP_raster)

# Merge classification
df <- data.frame(Cell = 1:length(zones_MMPP_raster),
                 MA_type = zones_MMPP_raster[],
                 reserve = zones_reserve_raster[]) %>%
  mutate(MA = paste(MA_type, reserve, sep = "-"))

# Create legend for MAs
MA_legend <- data.frame(MA_description = unique(df$MA),
                        MA_code = c(0, 1, 2, 3, 3))

# Create raster for new MA classification
df <- df %>% left_join(MA_legend, by = c("MA" = "MA_description"))
MA_raster <- empty_raster
values(MA_raster) <- df$MA_code
plot(MA_raster)

dataType(MA_raster) # check datatype of the raster
writeRaster(MA_raster, paste(di_experiments, "management_areas.tif", sep = ""),  datatype='INT4S', NAflag = 0, overwrite = TRUE)
