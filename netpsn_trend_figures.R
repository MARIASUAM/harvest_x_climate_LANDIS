# Plot NetPsn Kendall results

library(ggplot2)
library(raster)
library(rasterVis)

mgmt.scenarios <- c("211129_nomanag_current",
                    "211129_conserv_current",
                    "211129_proactive_current",
                    "211129_proactiveplus_current",
                    "211129_nomanag_rcp45",
                    "211129_conserv_rcp45",
                    "211129_proactive_rcp45",
                    "211129_proactiveplus_rcp45",
                    "211129_nomanag_rcp85",
                    "211129_conserv_rcp85",
                    "211129_proactive_rcp85",
                    "211129_proactiveplus_rcp85") # Folder names with each scenario

# Labels
labels <- data.frame(Scenario = c("211129_nomanag_current",
                                  "211129_conserv_current",
                                  "211129_proactive_current",
                                  "211129_proactiveplus_current",
                                  "211129_nomanag_rcp45",
                                  "211129_conserv_rcp45",
                                  "211129_proactive_rcp45",
                                  "211129_proactiveplus_rcp45",
                                  "211129_nomanag_rcp85",
                                  "211129_conserv_rcp85",
                                  "211129_proactive_rcp85",
                                  "211129_proactiveplus_rcp85"), # Folder names with each scenario
                     Harv_sce = c("NoManagement", "Conservative", "Proactive", "ProactivePlus",
                                  "NoManagement", "Conservative", "Proactive", "ProactivePlus",
                                  "NoManagement", "Conservative", "Proactive", "ProactivePlus"),
                     Clim_sce = c("Current", "Current", "Current", "Current",
                                  "RCP4.5", "RCP4.5", "RCP4.5", "RCP4.5", 
                                  "RCP8.5", "RCP8.5", "RCP8.5", "RCP8.5"))

### SETUP
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Load maps
pvalues <- stack()
taus <- stack()
for (i in seq_along(mgmt.scenarios)) {
  temp_pvalue <- raster(paste0(di, outputs_folder, "NetPsn_Kendall/", mgmt.scenarios[i], "_kendall_results_pvalue.asc"))
  names(temp_pvalue) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pvalues <- stack(pvalues, temp_pvalue)
  
  temp_tau <- raster(paste0(di, outputs_folder, "NetPsn_Kendall/", mgmt.scenarios[i], "_kendall_results_tau.asc"))
  names(temp_tau) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  taus <- stack(taus, temp_tau)
}

# Plot taus
jpeg(file = paste(di, outputs_folder, "NetPsn_Kendall_tau.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(taus) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  scale_fill_gradient2(low = '#d7191c', mid = '#ffffbf', high = '#2c7bb6', na.value = "transparent") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("") +
  ylab("") +
  xlab("")
dev.off()

# Plot p-values
# gplot(pvalues) + 
#   geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable, ncol = 4) +
#   coord_equal() +
#   theme_classic() +
#   scale_fill_gradient2(low = '#d7191c', mid = '#ffffbf', high = '#2c7bb6', na.value = "transparent") +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         legend.text = element_text(size = 14),
#         legend.title = element_blank()) +
#   ggtitle("") +
#   ylab("") +
#   xlab("")

m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)
pvalues_reclass <- reclassify(pvalues, rclmat, include.lowest=TRUE)

jpeg(file = paste(di, outputs_folder, "NetPsn_Kendall_pvalue.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(pvalues_reclass) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values=c('#1b9e77','#d95f02'),
                    name="",
                    breaks=c(1, 2),
                    labels=c("p-value < 0.05", "p-value >= 0.05"),
                    na.value  = "transparent") +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("") +
  ylab("") +
  xlab("")
dev.off()
