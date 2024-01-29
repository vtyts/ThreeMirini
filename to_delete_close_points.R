# Code for eliminating locality points closer than 50m to each other----
## 1. Creating separate steps ----
library("geosphere") #for calculation of geographic distances

locmatrix <- read.delim('punctatus_localities.tsv', header = T) #necessary headers: latitude, longitude, date
dist_locmatrix <- distm(x=locmatrix[c('longitude','latitude')], fun=distGeo) #that is a distance matrix in METERS!!!
log_locmatrix <- dist_locmatrix < 50000 #all distances less than 50km are TRUE (1)
log_locmatrix[row(log_locmatrix)==col(log_locmatrix)] <- FALSE #eliminating the diagonal TRUE
points_elim <- c() #clear vector for points to delete

if (any(log_locmatrix==1)) repeat {
  max_sum <- which(colSums(log_locmatrix)==max(colSums(log_locmatrix)))
  points_elim <- append(points_elim, max_sum[which.min(locmatrix[max_sum, 'date'])])
  log_locmatrix[, points_elim] <- FALSE
  log_locmatrix[points_elim, ] <- FALSE
  
  if (any(log_locmatrix==1)==FALSE) {
    message(c('You need to delete ', length(points_elim), ' points: '))
    print(sort(points_elim))
    break}
  
  } else message('CONGRATULATION! Your points are already perfect!')

write.csv(locmatrix[-points_elim, c(2,1)], 'perfect_punctatus_local.csv', quote = F, row.names = F)
write.table(locmatrix[-points_elim,], file = 'all_perfect_punctatus_local.tsv', sep = '\t', quote = F)

## 2. Combining steps into one function (not done yet) ---- 
#locpoints_eliminator <- function(locmatrix, pref_dist, show_plot, show_N, show_year) {}