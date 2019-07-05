# 23/05/2019
# Josh Jones
# This is a 2 part job. This script only does job 2.
# 1) As most river networks don't come with a bain ID we have to 
# consolidate touching lines into basins
# 2) The we can use barriers to cut these rivers 
# and calculate barrier free length (BFL)

# Time difference of 1.809308 mins for Corsica only

# usage: Rscript bfl_sunbird.R FR_rivers.shp FR_barriers.shp 2192 2

library(dplyr)
library(sf)

source("st_parallel.R")

START <- Sys.time()

# what time is it?
timedate <- paste0(format(Sys.time(), "%H%M%S"), "_", Sys.Date())

# inputs
args <- commandArgs(trailingOnly = TRUE)
cat(args, sep = "\n")
basinRivers <- args[1] # format basin_rivers.shp, make sure it's basin_rivers.shp !!!!!!
barriers <- args[2] # format barriers.shp # snapped barriers because snapping takes too long in sf - use QGIS (SAGA)
epsg <- as.numeric(args[3]) # format 2192
ncores <- as.numeric(args[4]) # format 1

# barriers <- 'CO_barriers_ED50_snapped.shp'
# basinRivers <- 'basinRivers143026_2019-07-04.shp'
# epsg <- 2192
# ncores <- 10

message(paste0('Calculating barrier free length for ', basinRivers, ' using ', ncores, ' cores.'))

# rivers
basinRivers <- st_read(dsn = basinRivers) %>% st_transform(epsg)

# barriers
snapped <- st_read(dsn = barriers) %>%
  select(LabelAtlas) %>%
  st_transform(epsg)

##############
# Second, cut rivers with points
##############

# # Snap barriers to river
# st_snap_points = function(x, y, max_dist = 1000) {
# 
#   if (inherits(x, "sf")) n = nrow(x)
#   if (inherits(x, "sfc")) n = length(x)
# 
#   out = do.call(c,
#                 lapply(seq(n), function(i) {
#                   nrst = st_nearest_points(st_geometry(x)[i], y)
#                   nrst_len = st_length(nrst)
#                   nrst_mn = which.min(nrst_len)
#                   if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
#                   return(st_cast(nrst[nrst_mn], "POINT")[2])
#                 })
#   )
#   return(out)
# }
# 
# snapped <- st_snap_points(barriers, basinRivers, 100) %>%
#   st_cast("POINT") %>%
#   st_zm(drop = TRUE, what = "ZM")

message('Barriers snapped to rivers.')

# Buffer barriers
buffBarriers <- st_parallel(snapped, st_buffer, ncores, dist = 0.0001)
message('Barriers buffered for cutting river.')

# st_write(bufferedBarriers, paste0("bufferedBarriers", timedate, ".shp"), delete_layer = TRUE) # overwrites

# Difference river network using buffered barriers
# A helper function that erases all of y from x: 
st_erase = function(x, y) st_difference(x, st_union(st_union(y))) # use st_union instead of st_combine
diffRivers <- st_parallel(basinRivers, st_erase, ncores, buffBarriers) # can get stuck here with invalid geom
message('Rivers erased using buffered barriers.')
# st_write(differencedRivers, paste0("differencedRivers", timedate, ".shp"), delete_layer = TRUE) # overwrites

# Create river polygons and split them into seperate segments
buffRivers <- st_parallel(diffRivers, st_buffer, ncores, dist = 0.00009)
message('Rivers buffered.')
dissRivers <- st_parallel(buffRivers, st_union, ncores)
message('Rivers dissolved.')
singlepartpoly <- st_cast(dissRivers, 'POLYGON') %>% 
  as('Spatial') %>% 
  st_as_sf() %>%
  mutate(id = as.factor(row_number()))
message('Rivers now singlepart polygons.')

########
# need to parallelise this bit
########
singlepartline <- do.call(rbind,lapply(1:nrow(diffRivers),function(i){st_cast(diffRivers[i,],"LINESTRING")}))
message('Rivers now singlepart lines.')

# Bring attributes back together
joinedRivers <- st_parallel(singlepartline, st_join, ncores, singlepartpoly)
message('Attributes joined back to basin rivers.')

# calculate components of BFL
joinedRivers$seglen <- st_length(joinedRivers)
basinlen <- joinedRivers %>% group_by(basinID) %>% summarise(basinlen = sum(seglen)) %>% st_drop_geometry()
BFL <- joinedRivers %>% group_by(id) %>% summarise(BFL = sum(seglen)) %>% st_drop_geometry()

# calculate BFLS
BFLS <- joinedRivers %>% 
  left_join(basinlen, by = 'basinID') %>% 
  left_join(fraglen, by = 'id') %>% 
  mutate(BFLS = as.numeric(BFL/basinlen) )

message('BFLS finished!')

# write output
st_write(BFLS, paste0("BFLS", timedate, ".shp"), delete_layer = TRUE) # overwrites

# timing
END <- Sys.time()
message('Time taken:')
END - START
