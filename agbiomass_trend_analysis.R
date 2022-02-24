# Analyse AGB Kendall results

library(raster)
library(dplyr)

mgmt.scenarios <- c(...) # Folder names with each scenario

# Labels
labels <- data.frame(Scenario = c(...), # Folder names with each scenario
                     Harv_sce = c("No Management", "Conservative", "Proactive", "ProactivePlus",
                                  "No Management", "Conservative", "Proactive", "ProactivePlus",
                                  "No Management", "Conservative", "Proactive", "ProactivePlus"),
                     Clim_sce = c("Current", "Current", "Current", "Current",
                                  "RCP4.5", "RCP4.5", "RCP4.5", "RCP4.5", 
                                  "RCP8.5", "RCP8.5", "RCP8.5", "RCP8.5"))

### SETUP
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

pines <- data.frame(Pines = pines_mask[]) %>%
  filter(is.na(Pines) == FALSE) %>%
  group_by(Pines) %>%
  summarise(count = n())

nr_cells <- pines$count

# Load maps of total AGB as data frame
df <- data.frame(Trend = c("neg_trend", "no_trend", "pos_trend"))

m <- c(0, 0.05, 1,  0.05, 1, 2)
rclmat <- matrix(m, ncol = 3, byrow=TRUE)

for (i in seq_along(mgmt.scenarios)) {
  temp_tau <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_kendall_results_tau.asc"))

  temp_pvalue <- raster(paste0(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_kendall_results_pvalue.asc"))
  pvalues_reclass <- reclassify(temp_pvalue, rclmat, include.lowest=TRUE)
  
  temp <- data.frame(tau = temp_tau[],
                     pval = pvalues_reclass[])

  reclass <- temp %>%
    filter(pval == 1) %>% # Only cell with significant trend are analysed
    mutate(tau_recl = ifelse(tau > 0.5, "pos_trend",
                        ifelse(tau < -0.5, "neg_trend", "no_trend"))) %>%
    dplyr::select(-pval, -tau) %>%
    group_by(tau_recl) %>%
    summarise(count = n())
  
  colnames(reclass) <- c("Trend", paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-"))
  
  df <- full_join(df, reclass)
}

trends <- as.data.frame(t(as.matrix(df[,-1])))
colnames(trends) <- c("neg_trend", "no_trend", "pos_trend")

percentages <- trends %>%
  mutate(per_neg_trend = neg_trend * 100/ nr_cells,
         per_no_trend = no_trend * 100/ nr_cells,
         per_pos_trend = pos_trend * 100/ nr_cells,
         tot = per_neg_trend + per_no_trend + per_pos_trend)





