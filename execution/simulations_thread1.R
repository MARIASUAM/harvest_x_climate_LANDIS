w_dir <- "..." # Path to simulations folder (working directory)
setwd(w_dir)

# Create an object named after each folder where you want to run LANDIS-II.
runs <- c(...) # Folder names with each replicate 

# Looping through folder names to run the batch files 
for(i in 1:length(runs)){ 
  print(paste(w_dir, i, "/", sep = ""))
  subdirectory_name <- paste(w_dir, runs[i], "/", sep = "")
  setwd(subdirectory_name)
  shell('RUN.bat')
  print(paste(runs[i], "done!", sep = " "))
  closeAllConnections()
}
