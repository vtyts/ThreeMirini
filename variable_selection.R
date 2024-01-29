##This is the code for variable selection. To proceed you need swd file, palearctic bio folder, maxent, background_biased file. NB! No spaces in files path! Full pathway only! Works only on 3.6.3 R version.

##Start ----
library(MaxentVariableSelection)
library(raster)
library(ggplot2)

maxent <- ("F:/Aaaa_Im_almost_a_scientist/BUGS//maxent/maxent.jar") #path to maxent
gridfolder <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/CNM/palearctic_climate") #bioNN.asc folder
contributionthreshold <- 0.1 #const
correlationthreshold <- 0.9 #const
betamultiplier <- seq(1, 6, 0.5) #const
occurrencelocations <- "F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_for_maxent.csv" #swd file from ENMeval.r script
backgroundlocations <- "F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/background_biased_punctatus.csv" #file from ENMeval.r script

write(c('additionalargs,betamultiplier,N_vars,Vars,'), file="F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_selection_result.csv")
message_no_mess <- function(path_to_file, argument) {
  var_result <- read.delim(path_to_file)
  message(c('Analysis with ', argument, ' argument resulted in ', 
            length(na.omit(var_result)[-c(1,2), 1]), 
            ' variables:'))
  print(as.character(na.omit(var_result)[-c(1,2), 1]))
  message('betamultiplier = ', var_result[2,2])
  cat(c(argument, var_result[2,2], length(na.omit(var_result)[-c(1,2), 1]), paste(as.character(na.omit(var_result)[-c(1,2), 1]), collapse = ' '), '\n'), file="F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_selection_result.csv", append = TRUE, sep = ',')
}

## L ----
outdir <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_L") #future name of output folder
additionalargs <- "LINEAR=TRUE noquadratic nohinge noproduct nothreshold noautofeature" #The results must be calculated with six types of arguments in additionalargs: (L), LQ, LQH, H, LQHP, LQHPT

VariableSelection(maxent, outdir, gridfolder, occurrencelocations, backgroundlocations, additionalargs, contributionthreshold, correlationthreshold, betamultiplier)

message_no_mess(path_to_file = 'F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_L/VariableSelectionMinAICc.txt', argument = 'L')


## LQ ----
outdir <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQ") #future name of output folder
additionalargs <- "LINEAR=TRUE QUADRATIC=TRUE nohinge noproduct nothreshold noautofeature" #The results must be calculated with six types of arguments in additionalargs: L, (LQ), LQH, H, LQHP, LQHPT

VariableSelection(maxent, outdir, gridfolder, occurrencelocations, backgroundlocations, additionalargs, contributionthreshold, correlationthreshold, betamultiplier)

message_no_mess(path_to_file = 'F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQ/VariableSelectionMinAICc.txt', argument = 'LQ')


## LQH ----
outdir <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQH") #future name of output folder
additionalargs <- "LINEAR=TRUE QUADRATIC=TRUE HINGE=TRUE noproduct nothreshold noautofeature" #The results must be calculated with six types of arguments in additionalargs: L, LQ, (LQH), H, LQHP, LQHPT

VariableSelection(maxent, outdir, gridfolder, occurrencelocations, backgroundlocations, additionalargs, contributionthreshold, correlationthreshold, betamultiplier)

message_no_mess(path_to_file = 'F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQH/VariableSelectionMinAICc.txt', argument = 'LQH')


## H ----
outdir <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_H") #future name of output folder
additionalargs <- "nolinear noquadratic HINGE=TRUE noproduct nothreshold noautofeature" #The results must be calculated with six types of arguments in additionalargs: L, LQ, LQH, (H), LQHP, LQHPT

VariableSelection(maxent, outdir, gridfolder, occurrencelocations, backgroundlocations, additionalargs, contributionthreshold, correlationthreshold, betamultiplier)

message_no_mess(path_to_file = 'F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_H/VariableSelectionMinAICc.txt', argument = 'H')


## LQHP ----
outdir <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQHP") #future name of output folder
additionalargs <- "LINEAR=TRUE QUADRATIC=TRUE HINGE=TRUE PRODUCT=TRUE nothreshold noautofeature" #The results must be calculated with six types of arguments in additionalargs: L, LQ, LQH, H, (LQHP), LQHPT

VariableSelection(maxent, outdir, gridfolder, occurrencelocations, backgroundlocations, additionalargs, contributionthreshold, correlationthreshold, betamultiplier)

message_no_mess(path_to_file = 'F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQHP/VariableSelectionMinAICc.txt', argument = 'LQHP')


## LQHPT ----
outdir <- ("F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQHPT") #future name of output folder
additionalargs <- "LINEAR=TRUE QUADRATIC=TRUE HINGE=TRUE PRODUCT=TRUE THRESHOLD=TRUE noautofeature" #The results must be calculated with six types of arguments in additionalargs: L, LQ, LQH, H, LQHP, (LQHPT)

VariableSelection(maxent, outdir, gridfolder, occurrencelocations, backgroundlocations, additionalargs, contributionthreshold, correlationthreshold, betamultiplier)

message_no_mess(path_to_file = 'F:/Aaaa_Im_almost_a_scientist/BUGS/Lygus_endless/ThreeMirini/punctatus_var_selection/punctatus_var_LQHPT/VariableSelectionMinAICc.txt', argument = 'LQHPT')