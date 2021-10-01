# Trend in AGB

mgmt.scenarios <- c("210927_conserv_current",
                    "210927_conserv_rcp45",
                    "210927_conserv_rcp85",
                    "210927_nomanag_current",
                    "210927_nomanag_rcp45",
                    "210927_nomanag_rcp85",
                    "210927_proactive_current",
                    "210927_proactive_rcp45",
                    "210927_proactive_rcp85",
                    "210927_proactiveplus_current",
                    "210927_proactiveplus_rcp45",
                    "210927_proactiveplus_rcp85")

replicates <- c(1:3) # 1:5 when completed
times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

### SETUP 
library(raster)
library(spatialEco)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red
lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Load AGB maps
trend_stack <- stack()
for (i in seq_along(mgmt.scenarios)){
  agb_stack <- stack()
  for (j in seq_along(times)){
    replicate_stack <- stack()
    for (h in seq_along(replicates)){
      temp <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Total_AGB_", times[j], ".asc", sep = ""))
      replicate_stack <- stack(replicate_stack, temp)
    }
    avg_among_replicates <- calc(replicate_stack, mean)
    names(avg_among_replicates) <- paste("time", times[j], sep = "_")
    agb_stack <- stack(agb_stack, avg_among_replicates)
  }
}

agb_masked <- mask(agb_stack, pines_mask)

aver <- raster.kendall(agb_stack, p.value = FALSE, 
                       z.value = FALSE, intercept = FALSE, 
                       confidence = FALSE, tau = FALSE)

aver <- mask(aver, pines_mask)

plot(aver)

aver
