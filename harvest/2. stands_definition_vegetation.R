## Create stands map based on vegetation attributes

library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)

di_inputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/input_files/initialcommunities_shrublands/"
di_outputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/management/"
di_experiments <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/0. sim_root/inputs_management/"

# Load reference raster
active_cells <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/output_files/LANDIS_ecoregions_map.tif")
active_cells[active_cells != 0] <- 1

# Load vegetation map
vege <- readOGR(dsn = di_inputs, layer = "vege10_3042")
crs(vege)

# Inspect relevant attributes
# View(vege@data)
# plot(vege@data$Shape_Area)
# unique(vege@data$OBJECTID)

# Load Management Areas raster
MA <- raster(paste(di_experiments, "management_areas.tif", sep = ""))
plot(MA)

# Create empty raster
res(active_cells)
gridsize <- 100
empty_raster <- raster(extent(active_cells), res = gridsize, vals = NA)

# Rasterize vege
vege_raster <- rasterize(vege, empty_raster, field = vege@data[,1])
crs(vege_raster) <- crs(vege)
vege_raster <- vege_raster * active_cells
# plot(vege_raster)

# Create raster with combination of stands and MAs
all_cells <- data.frame(Cell = 1:length(vege_raster[]),
                        Stand = vege_raster[],
                        MA = MA[]) %>%
  mutate(Stand_MA = as.factor(paste(Stand, MA, sep = "-")))

Stands_ID <- data.frame(Stand_ID = 1:length(unique(all_cells$Stand_MA)),
                        Stand_MA = unique(all_cells$Stand_MA))

all_cells <- left_join(all_cells, Stands_ID)

# Create raster of Stands_ID and mask by inactive cells
all_cells <- all_cells[order(all_cells$Cell),]
stands_vegetation <- empty_raster
values(stands_vegetation) <- all_cells$Stand_ID
stands_vegetation <- stands_vegetation * active_cells

# Export stands raster
dataType(stands_vegetation) # check datatype of the raster
writeRaster(stands_vegetation, paste(di_experiments, "stands_vegetation.tif", sep = ""),  datatype='INT4S', NAflag = 0, overwrite = TRUE)

# Import stands raster for analysis
stands_vegetation <- raster(paste(di_experiments, "stands_vegetation.tif", sep = ""))
plot(stands_vegetation)
stands_summary <- data.frame(Stands_ID = stands_vegetation[]) %>%
  group_by(Stands_ID) %>%
  dplyr::summarise(surface_ha = n()) %>%
  filter(is.na(Stands_ID) == FALSE)

plot(stands_summary$surface_ha)

filt <- stands_summary %>%
  filter(surface_ha < 8000)
hist(filt$surface_ha, breaks = c(0, 10, 20, 30, 40, 50, 100, 200, 400, 600))

mean(filt$surface_ha)

summary(stands_summary$surface_ha)
