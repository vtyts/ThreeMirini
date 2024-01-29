setwd("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_maxent_results") #setting directory to the one, in which we keep all the maxent results NB! This directory can have other files and folders as long as the folders for the area calculation are called with the same pattern
result_dirs <- dir(pattern="LQ_1_5_red_vars*", recursive = FALSE) #creates a list of chosen directories
output_file <- "./punctatus_red_vars_formulas_for_Qgis.csv" #don't forget to change the name of the output file
species_row_name <- 'punctatus (average)' #don't forget to change

write(c('folder,formula,'), file=output_file)
for (x in 1:(length(result_dirs))) {
  res_table <- read.csv(paste(result_dirs[x],'/maxentResults.csv', sep = ''), row.names = 1)
  name_of_layer <- tools::file_path_sans_ext(list.files(path = result_dirs[x], pattern = '*avg.asc'))[length(list.files(path = result_dirs[x], pattern = '*avg.asc'))]
  cat(c(result_dirs[x], paste('("', name_of_layer, '@1" > ', res_table[species_row_name, 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold'], ') = 0 AND ("', name_of_layer, '@1" <= ', res_table[species_row_name, 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold'], ') = 1', sep = ''), '\n'), file=output_file, append = TRUE, sep = ',')
}
