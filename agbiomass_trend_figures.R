# Plot AGB Kendall results

library(ggplot2)
library(raster)
library(rasterVis)

mgmt.scenarios <- c(...) # Folder names with each scenario

# Labels
labels <- data.frame(Scenario = c(...), # Folder names with each scenario
                     Harv_sce = c("NoManagement", "Conservative", "Proactive", "ProactivePlus",
                                  "NoManagement", "Conservative", "Proactive", "ProactivePlus",
                                  "NoManagement", "Conservative", "Proactive", "ProactivePlus"),
                     Clim_sce = c("Current", "Current", "Current", "Current",
                                  "RCP4.5", "RCP4.5", "RCP4.5", "RCP4.5", 
                                  "RCP8.5", "RCP8.5", "RCP8.5", "RCP8.5"))

### SETUP
di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Load maps - total AGB
pvalues <- stack()
taus <- stack()
for (i in seq_along(mgmt.scenarios)) {
  temp_pvalue <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_kendall_results_pvalue.asc"))
  names(temp_pvalue) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pvalues <- stack(pvalues, temp_pvalue)

  temp_tau <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_kendall_results_tau.asc"))
  names(temp_tau) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  taus <- stack(taus, temp_tau)
}

# Plot taus - total AGB
jpeg(file = paste(di, outputs_folder, "AGB_Kendall_tau.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
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
m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)
pvalues_reclass <- reclassify(pvalues, rclmat, include.lowest=TRUE)

jpeg(file = paste(di, outputs_folder, "AGB_Kendall_pvalue.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
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

# Load maps - oaks AGB
pvalues_oaks <- stack()
taus_oaks <- stack()

for (i in seq_along(mgmt.scenarios)) {
  temp_pvalue_oaks <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_kendall_results_pvalue.asc"))
  names(temp_pvalue_oaks) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pvalues_oaks <- stack(pvalues_oaks, temp_pvalue_oaks)

  temp_tau_oaks <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_kendall_results_tau.asc"))
  names(temp_tau_oaks) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  taus_oaks <- stack(taus_oaks, temp_tau_oaks)
}

# Plot taus - oaks AGB
jpeg(file = paste(di, outputs_folder, "Oaks_AGB_Kendall_tau.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(taus_oaks) +
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
  ggtitle("Oaks AGB") +
  ylab("") +
  xlab("")
dev.off()

# Plot p-values oaks
m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)
pvalues_reclass_oaks <- reclassify(pvalues_oaks, rclmat, include.lowest=TRUE)

jpeg(file = paste(di, outputs_folder, "Oaks_AGB_Kendall_pvalue.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(pvalues_reclass_oaks) +
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
  ggtitle("Oaks AGB") +
  ylab("") +
  xlab("")
dev.off()

# Load maps - pines AGB
pvalues_pines <- stack()
taus_pines <- stack()

for (i in seq_along(mgmt.scenarios)) {
  temp_pvalue_pines <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_kendall_results_pvalue.asc"))
  names(temp_pvalue_pines) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pvalues_pines <- stack(pvalues_pines, temp_pvalue_pines)

  temp_tau_pines <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_kendall_results_tau.asc"))
  names(temp_tau_pines) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  taus_pines <- stack(taus_pines, temp_tau_pines)
}

# Plot taus - pines AGB
jpeg(file = paste(di, outputs_folder, "Pines_AGB_Kendall_tau.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(taus_pines) +
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
  ggtitle("Pines AGB") +
  ylab("") +
  xlab("")
dev.off()

# Plot p-values pines
m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)
pvalues_reclass_pines <- reclassify(pvalues_pines, rclmat, include.lowest=TRUE)

jpeg(file = paste(di, outputs_folder, "Pines_AGB_Kendall_pvalue.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(pvalues_reclass_pines) +
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
  ggtitle("Pines AGB") +
  ylab("") +
  xlab("")
dev.off()

# Load maps - oaks AGB first cohort
pvalues_oaks <- stack()
taus_oaks <- stack()

for (i in seq_along(mgmt.scenarios)) {
  temp_pvalue_oaks <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_first_cohort_kendall_results_pvalue.asc"))
  names(temp_pvalue_oaks) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pvalues_oaks <- stack(pvalues_oaks, temp_pvalue_oaks)
  
  temp_tau_oaks <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_first_cohort_kendall_results_tau.asc"))
  names(temp_tau_oaks) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  taus_oaks <- stack(taus_oaks, temp_tau_oaks)
}

# Plot taus - oaks AGB
jpeg(file = paste(di, outputs_folder, "Oaks_AGB_first_cohort_Kendall_tau.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(taus_oaks) + 
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
  ggtitle("Oaks AGB - first cohort") +
  ylab("") +
  xlab("")
dev.off()

# Plot p-values oaks
m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)
pvalues_reclass_oaks <- reclassify(pvalues_oaks, rclmat, include.lowest=TRUE)

jpeg(file = paste(di, outputs_folder, "Oaks_AGB_first_cohort_Kendall_pvalue.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(pvalues_reclass_oaks) + 
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
  ggtitle("Oaks AGB - first cohort") +
  ylab("") +
  xlab("")
dev.off()

# Load maps - pines AGB first cohort
pvalues_pines <- stack()
taus_pines <- stack()

for (i in seq_along(mgmt.scenarios)) {
  temp_pvalue_pines <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_first_cohort_kendall_results_pvalue.asc"))
  names(temp_pvalue_pines) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pvalues_pines <- stack(pvalues_pines, temp_pvalue_pines)
  
  temp_tau_pines <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_first_cohort_kendall_results_tau.asc"))
  names(temp_tau_pines) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  taus_pines <- stack(taus_pines, temp_tau_pines)
}

# Plot taus - pines AGB
jpeg(file = paste(di, outputs_folder, "Pines_AGB_first_cohort_Kendall_tau.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(taus_pines) + 
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
  ggtitle("Pines AGB - first cohort") +
  ylab("") +
  xlab("")
dev.off()

# Plot p-values pines
m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)
pvalues_reclass_pines <- reclassify(pvalues_pines, rclmat, include.lowest=TRUE)

jpeg(file = paste(di, outputs_folder, "Pines_AGB_first_cohort_Kendall_pvalue.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(pvalues_reclass_pines) + 
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
  ggtitle("Pines AGB - first cohort") +
  ylab("") +
  xlab("")
dev.off()
