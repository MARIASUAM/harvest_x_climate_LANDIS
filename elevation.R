# Analysis of elevation

# Setup
library(rgdal)
library(raster)
library(dplyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

# Load MDT (Modelo Digital del Terreno)
mdt <- raster("/Users/maria.suarez.munoz/Documents/Mapas/MDT25/MDT25.tif")
# extent(mdt)

# Load MUCVA
mucva <- readOGR(dsn = "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/input_files/initialcommunities_shrublands/", 
                 layer = "MUCVA10_1996_2006_aoi_active")
crs(mucva)

# Load reference file
active_cells <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/0. sim_root/inputs_basic/ecoregions.tif")
crs(active_cells) # should be 3042

# Reproject MUCVA
mucva_reprj <- spTransform(mucva, crs(active_cells))

# Mask MDT by pines mask
dense_pines <- subset(mucva_reprj, COD_USO == 520) # CONIFERAS DENSAS

mdt_masked <- mask(mdt, dense_pines)

plot(mdt_masked)
hist(mdt_masked)

cellStats(mdt_masked, min, na.rm = TRUE)
cellStats(mdt_masked, max, na.rm = TRUE)
cellStats(mdt_masked, mean, na.rm = TRUE)
