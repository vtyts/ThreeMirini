library(raster)
library(rgdal)
library(dplyr)

#ATTENTION!!! Change all the direction names prior to starting the code to avoid unexpected errors (INCLUDING setwd) and read the comments to the lines BEFORE running them!
path_to_env_layers <- "F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/Palearctic_environmental_layers" #path to your environmental layers in .asc extension
variable_set <- list.files(path = path_to_env_layers, pattern = '*.asc')
setwd("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_maxent_results/Qgis_export_results") #folder in which you keep all the shp files for all models, NEVER CHANGE WORKING DIRECTORY SOMEWHERE IN THE MIDDLE OF THIS CODE AFTER THIS LINE!
output_file <- "../punctatus_red_vars_variables_ranges.csv" #../ means that we take the step out of our working direction, so in this example my file will be placed in punctatus_maxent_results folder

result_files <- tools::file_path_sans_ext(list.files(pattern = '*.shp')) #creates a list of chosen shape files. If you store all the shape files in one folder (even for past and future), you can change the pattern to (for example) '*current.shp'. Just check that your pattern still has * at the beginning and .shp an the end. If you aren't sure that your pattern worked, view the result_files value.
min_cols <- paste(tools::file_path_sans_ext(variable_set), 'min,')
max_cols <- paste(tools::file_path_sans_ext(variable_set), 'max,')
col_names <- append('model,', sort(append(min_cols, max_cols)))
write(paste(as.character(col_names), collapse = ''), file=output_file) #creates an output file with column names, if you already have one named exactly like stated in output_file value, this line will rewrite it. ENTIRELY.

#First we will create the record row. If you want to add record data for several species you can change the swd file and in cat function 'record' to 'record_speciesname' as many times as you want. This code will append the existing result file, but you need to run it entirely from line 17 to 24!!!!
swd_file <- read.csv('../../punctatus_for_maxent.csv')  #your swd file, you don't necessary need to write it using relative paths as I wrote here
col_list <- colnames(swd_file)[c(4:length(colnames(swd_file)))]
cat('record,', file=output_file, append = TRUE, sep = '')
for (x in 1:length(col_list)) {
  cat(c(max(swd_file[,col_list[x]]), min(swd_file[,col_list[x]]), ''), file=output_file, append = TRUE, sep = ',')
  #plot(rmasked) #if you want a list of plots to appear you can uncomment here
}
cat ('\n', file=output_file, append = TRUE, sep = ',')

#Now we will add the results of the models, list of which is in result_files value. Each row will start with the name corresponding to the one in the result_files list, so you can run it for the same models names but different species as long as your files names were different, all in one folder, worked with the pattern in line 10 and therefore are in the result_files value.
for (x in 1:(length(result_files))) {
  myshp <- readOGR(dsn = ".", layer=result_files[x])
  e <- extent(myshp)
  cat(c(result_files[x], ','), file=output_file, append = TRUE, sep = '')
  message('Model ', result_files[x])
  for (i in 1:length(variable_set)) {
    myraster <- raster(paste(path_to_env_layers, '/', variable_set[i], sep = ''))
    r <- crop(myraster, e)
    rmasked <- mask(r, myshp)
    cat(c(summary(rmasked)["Max.", 1], summary(rmasked)["Min.", 1], ''), file=output_file, append = TRUE, sep = ',')
    message(tools::file_path_sans_ext(variable_set)[i], ' added')
    #plot(rmasked) #if you want the plots to appear you can uncomment here
  }
  cat ('\n', file=output_file, append = TRUE, sep = ',')
  message('Model ', result_files[x], ' finished')
}


# VERY IMPORTANT!!! Run the next lines ONLY ONE TIME after you've finished adding everything you wanted. It rewrites your result table with decimals and other value order, you won't be able to add values correctly to it anymore!
finished_table <- read.csv(output_file)
finished_table[,c(2:5, 10:23)] <- finished_table[,c(2:5, 10:23)]/10 #now temperature values are with decimals
for (x in 1:length(grep('*min', colnames(finished_table)))) {
  finished_table <- finished_table %>% relocate(grep('*min', colnames(finished_table))[x], .before = grep('*max', colnames(finished_table))[x])
} #reorders it so min is the first and max is the second
write.csv(finished_table[,-ncol(finished_table)], file = output_file, row.names = F)
