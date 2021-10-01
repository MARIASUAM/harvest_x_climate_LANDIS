w_dir <- "G:/My Drive/proj_LANDIS/experiments/"
setwd(w_dir)

# Create an object named after each folder where you want to run LANDIS-II.
runs<- c("210927_proactiveplus_current_rep1",
         "210927_proactiveplus_rcp45_rep1",
         "210927_proactiveplus_rcp85_rep1",
         "210927_proactiveplus_current_rep2",
         "210927_proactiveplus_rcp45_rep2",
         "210927_proactiveplus_rcp85_rep2",
         "210927_proactiveplus_current_rep3",
         "210927_proactiveplus_rcp45_rep3",
         "210927_proactiveplus_rcp85_rep3",
         "210927_proactiveplus_current_rep4",
         "210927_proactiveplus_rcp45_rep4",
         "210927_proactiveplus_rcp85_rep4",
         "210927_proactiveplus_current_rep5",
         "210927_proactiveplus_rcp45_rep5",
         "210927_proactiveplus_rcp85_rep5") 

# Looping through folder names to run the batch files 
for(i in 1:length(runs)){ 
  print(paste(w_dir, i, "/", sep = ""))
  subdirectory_name <- paste(w_dir, runs[i], "/", sep = "")
  setwd(subdirectory_name)
  shell('RUN.bat')
  print(paste(runs[i], "done!", sep = " "))
  closeAllConnections()
}
