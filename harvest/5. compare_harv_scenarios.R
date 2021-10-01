# Compare harvest outputs for different scenarios (MSM 2021.04-06)

### SETUP: NAME OF SCENARIOS and MAPS ####################
mgmt.scenarios <- c("210831_conserv_current",
                    "210831_nomanag_current",
                    "210831_proactive_current",
                    "210831_proactiveplus_current",
                    "210831_conserv_rcp45",
                    "210831_conserv_rcp85",
                    "210831_nomanag_rcp45",
                    "210831_nomanag_rcp85",
                    "210831_proactive_rcp45",
                    "210831_proactive_rcp85",
                    "210831_proactiveplus_rcp45",
                    "210831_proactiveplus_rcp85")
MA_map <- "management_areas.tif" # MA map 
#################################################

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
library(raster)
library(dplyr)
library(rasterVis); library(RColorBrewer)
library(ggplot2)
library(readtext)

#Generate color palette with n number of colors (colors = prescriptions)
n <- 17
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_palette = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# Load MAs map and calculate surface
MAs <- raster(paste(di, "0. sim_root/inputs_management/", MA_map, sep =""))
plot(MAs)

MAs_surface <- data.frame(MA = MAs[]) %>%
  mutate(Surface = 1) %>% # each cell is 1 ha
  group_by(MA) %>%
  summarise(MA_surface = sum(Surface)) %>%
  filter(is.na(MA) != TRUE)

# write.csv(MAs_surface, paste(di, "data/MAs_surface.csv", sep = ""))

# Load harvest outputs and build comparison table
summary_comp <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  summary <- read.table(paste(di, mgmt.scenarios[i], "/output/harvest/summary-log.csv", sep = ""), header = TRUE, sep = ",") %>%
    mutate(Scenario = paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][2], strsplit(mgmt.scenarios[i], split = "_")[[1]][3], sep = "_")) 
  summary_comp <- rbind(summary_comp, summary)
}

# Compare implementation among scenarios
implemented <- summary_comp %>%
  dplyr::select(Scenario, Time, ManagementArea, Prescription, HarvestedSites) %>%
  left_join(MAs_surface, by = c("ManagementArea" = "MA")) %>%
  mutate(Exec_Harvest_Area = HarvestedSites * 100 / MA_surface) %>%
  dplyr::select(Scenario, Time, ManagementArea, Prescription, Exec_Harvest_Area, HarvestedSites)

implemented %>% 
  mutate(Prescription = as.character(Prescription)) %>% 
  filter(Prescription != " Coppice" & 
           Prescription != " Herbivory") %>% 
  ggplot(aes(x = Time, y = Exec_Harvest_Area, fill = Prescription)) +  
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea ~ Scenario) +
  labs(x="Year", y = "HarvArea (%)") +  
  theme(legend.position="bottom") + guides(fill=guide_legend(ncol=2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("IMPLEMENTATION CHECK - CONIFER TREATMENTS")

implemented %>% 
  mutate(Prescription = as.character(Prescription)) %>% 
  filter(Prescription != " Coppice" & 
           Prescription != " Herbivory" ) %>% 
  dplyr::select(Scenario, Time, ManagementArea, Exec_Harvest_Area) %>%
  group_by(Scenario, Time, ManagementArea) %>%
  summarise(Total_Exec_Harvest_Area = sum(Exec_Harvest_Area)) %>%
  ggplot(aes(x = Time, y = Total_Exec_Harvest_Area)) +  
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea ~ Scenario) +
  labs(x="Year", y = "HarvArea (%)") +  
  theme(legend.position="bottom") + guides(fill=guide_legend(ncol=2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("IMPLEMENTATION CHECK - CONIFER TREATMENTS grouped")
  

implemented %>% 
  mutate(Prescription = as.character(Prescription)) %>% 
  filter(Prescription == " Coppice") %>% 
  ggplot(aes(x = Time, y = Exec_Harvest_Area, fill = Prescription)) +  
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea ~ Scenario) +
  labs(x="Year", y = "HarvArea (%)") +  
  theme(legend.position="none") + guides(fill=guide_legend(ncol=2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("IMPLEMENTATION CHECK - COPPICE")

implemented %>% 
  mutate(Prescription = as.character(Prescription)) %>% 
  filter(Prescription == " Herbivory") %>% 
  ggplot(aes(x = Time, y = Exec_Harvest_Area, fill = Prescription)) +  
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea ~ Scenario) +
  labs(x="Year", y = "HarvArea (%)") +  
  theme(legend.position="none") + guides(fill=guide_legend(ncol=2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("IMPLEMENTATION CHECK - HERBIVORY")

# Biomass harvested each timestep by managementArea and scenario
biomass <- summary_comp %>%
  dplyr::select(Scenario, Time, ManagementArea, Prescription, TotalBiomassHarvested)

ggplot(biomass, aes(x = Time, y = TotalBiomassHarvested, fill = Prescription)) +  
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea ~ Scenario) +
  labs(x="Year", y = "Biomass harvested (Mg)") +  
  theme(legend.position="bottom") + guides(fill=guide_legend(ncol = 2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("Biomass harvested - ALL SPECIES")

biomass_subset <- summary_comp %>%
  dplyr::select(Scenario, Time, ManagementArea, Prescription, 
                BiomassHarvestedMg_phalepensis, BiomassHarvestedMg_pnigra, BiomassHarvestedMg_ppinaster, BiomassHarvestedMg_psylvestris,
                BiomassHarvestedMg_qilex, BiomassHarvestedMg_qpyrenaica, BiomassHarvestedMg_qfaginea) %>%
  mutate(Tree_biomass = BiomassHarvestedMg_phalepensis + BiomassHarvestedMg_pnigra + BiomassHarvestedMg_ppinaster + BiomassHarvestedMg_psylvestris +
           BiomassHarvestedMg_qilex + BiomassHarvestedMg_qpyrenaica + BiomassHarvestedMg_qfaginea)

ggplot(biomass_subset, aes(x = Time, y = Tree_biomass, fill = Prescription)) +  
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea ~ Scenario) +
  labs(x="Year", y = "Biomass harvested (Mg)") +  
  theme(legend.position="bottom") + guides(fill=guide_legend(ncol = 2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("Biomass harvested - TREE SPECIES")

# Compare Plan Aprovechamientos - harvested biomass per treated hectare
summary_comp %>%
  filter(Prescription != " Herbivory",
         Prescription != " Coppice") %>%
  mutate(harv_bio_per_treated_ha = TotalBiomassHarvested / HarvestedSites) %>%
  ggplot(aes(x = Time, y = harv_bio_per_treated_ha, fill = Prescription)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea * Prescription ~ Scenario) +
  labs(x="Year", y = "Harvested biomass per treated hectare (tn/ha)") +  
  theme(legend.position="bottom") + guides(fill=guide_legend(ncol = 2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("Harvested biomass per treated hectare - ALL SPECIES")

summary_comp %>%
  filter(Prescription != " Herbivory",
         Prescription != " Coppice") %>%
  mutate(Tree_biomass = BiomassHarvestedMg_phalepensis + BiomassHarvestedMg_pnigra + BiomassHarvestedMg_ppinaster + BiomassHarvestedMg_psylvestris +
           BiomassHarvestedMg_qilex + BiomassHarvestedMg_qpyrenaica + BiomassHarvestedMg_qfaginea,
         harv_tree_bio_per_treated_ha = Tree_biomass / HarvestedSites) %>%
  ggplot(aes(x = Time, y = harv_tree_bio_per_treated_ha, fill = Prescription)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  facet_grid(ManagementArea * Prescription ~ Scenario) +
  labs(x="Year", y = "Harvested biomass per treated hectare (tn/ha)") +  
  theme(legend.position="bottom") + guides(fill=guide_legend(ncol = 2))  + #scale_fill_brewer(palette = "Paired") 
  scale_fill_manual(values = col_palette) +
  ggtitle("Harvested biomass per treated hectare - TREE SPECIES")

