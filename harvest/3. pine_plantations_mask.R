# Create mask of pine plantations
library(rgdal)
library(dplyr)
library(raster)

# Load input file
mucva <- readOGR(dsn = "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/input_files/initialcommunities_shrublands/", 
               layer = "MUCVA10_1996_2006_aoi_active")
mucva@data$DES_USO <- as.character(mucva@data$DES_USO)
crs(mucva)

# Create legend
legend <- mucva@data %>%
  dplyr::select(COD_USO, DES_USO) %>%
  group_by_all() %>%
  summarise(count = n())

# Selected categories:
levels(mucva@data$DES_USO)
# "FOR. ARBOL. DENSA: CONIFERAS"                                  
# "FOR. ARBOL. DENSA: CONIFERAS+EUCALIPTOS"
# "FOR. ARBOL. DENSA: QUERCINEAS+CONIFERAS"
# "MATORRAL DENSO ARBOLADO: CONIFERAS DENSAS"                     
# "MATORRAL DENSO ARBOLADO: CONIFERAS DISPERSAS"
# "MATORRAL DENSO ARBOLADO: QUERCINEAS+CONIFERAS"  
# "MATORRAL DISP. ARBOLADO: CONIFERAS. DENSO"                     
# "MATORRAL DISP. ARBOLADO: CONIFERAS. DISPERSO" 
# "MATORRAL DISP. ARBOLADO: QUERCINEAS+CONIFERAS"  
# "PASTIZAL ARBOLADO: CONIFERAS. DENSO"
# "PASTIZAL ARBOLADO: CONIFERAS. DISPERSO"
# "PASTIZAL ARBOLADO: QUERCINEAS+CONIFERAS"
# "TALAS Y PLANTACIONES FORESTALES RECIENTES"   

# Load reference file
active_cells <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/0. sim_root/inputs_basic/ecoregions.tif")
active_cells[active_cells != 0] <- 2 # Active cells
active_cells[active_cells == 0] <- NA
crs(active_cells) # should be 3042
plot(active_cells)
writeRaster(active_cells, "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/active_cells.tif",  overwrite = TRUE)

# Reproject MUCVA
# mucva_reprj <- projectRaster(from = mucva_raster, to = active_cells) # old, reprojection was done after rasterizing
mucva_reprj <- spTransform(mucva, crs(active_cells))
# plot(mucva_reprj)

# Rasterize MUCVA
empty_raster <- raster(nrow = nrow(active_cells), ncol = ncol(active_cells), extent(active_cells), vals = NA)
mucva_raster <- rasterize(mucva_reprj, active_cells, field=c("COD_USO"), update = TRUE, updateValue = "!NA")
mucva_raster_nonupdated <- rasterize(mucva_reprj, empty_raster, field=c("COD_USO"))

plot(mucva_raster, col=rainbow(10000))
plot(mucva_raster_nonupdated, col=rainbow(10000))

# Create mask
pines_mask <- mucva_raster
pines_mask[pines_mask == 520] <- 1
pines_mask[pines_mask == 550] <- 1
pines_mask[pines_mask == 570] <- 1
pines_mask[pines_mask == 621] <- 1
pines_mask[pines_mask == 625] <- 1
pines_mask[pines_mask == 650] <- 1
pines_mask[pines_mask == 721] <- 1
pines_mask[pines_mask == 725] <- 1
pines_mask[pines_mask == 750] <- 1
pines_mask[pines_mask == 821] <- 1
pines_mask[pines_mask == 825] <- 1
pines_mask[pines_mask == 850] <- 1
pines_mask[pines_mask == 901] <- 1

pines_mask[pines_mask != 1] <- NA

plot(pines_mask)

# Export
writeRaster(pines_mask, "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/pines_mask.asc",  overwrite = TRUE)

# Load and rewrite as .img
extent(pines_mask)

output <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/210831_proactiveplus_rcp85/output/agbiomass/jcommunis/AGBiomass0.img")
extent(output)

extent(pines_mask) <- extent(output)
origin(pines_mask) <- origin(output)
plot(pines_mask)

writeRaster(pines_mask, "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/pines_mask.img",  overwrite = TRUE)
