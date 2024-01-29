setwd("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_maxent_results") #setting directory to the one, in which we keep all the maxent results NB! There must be ONLY those folders in it from which we want to obtain results!!! All extra ones not containing maxentResults.csv file will cause an error!
result_dirs <- list.dirs(recursive = FALSE)
output_file <- "./punctatus_all_sets_of_pars_and_vars.csv" #don't forget to change the name of the output file
species_row_name <- 'punctatus (average)' #don't forget to change

write(c('folder,AUC (Training),AUC (Test),AUC difference,Omission rate,'), file=output_file)
for (x in 1:(length(result_dirs))) {
  res_table <- read.csv(paste(result_dirs[x],'/maxentResults.csv', sep = ''), row.names = 1)
  cat(c(result_dirs[x], res_table[species_row_name,'Training.AUC'], res_table[species_row_name,'Test.AUC'], (res_table[species_row_name,'Training.AUC']-res_table[species_row_name,'Test.AUC']), res_table[species_row_name,'Maximum.training.sensitivity.plus.specificity.test.omission'], '\n'), file=output_file, append = TRUE, sep = ',')
}