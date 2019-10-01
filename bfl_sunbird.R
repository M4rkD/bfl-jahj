# 23/05/2019
# Josh Jones
# This is a 2 part job. This script only does job 2.
# 1) As most river networks don't come with a bain ID we have to
# consolidate touching lines into basins
# 2) The we can use barriers to cut these rivers
# and calculate barrier free length (BFL)

# Completes in 34 seconds for Corsica using 2 cores

# usage: Rscript bfl_sunbird.R FR_rivers.shp FR_barriers.shp 2192 2

library(dplyr)
library(sf)
library(parallel)
library(lwgeom)

# both versions of the st_parallel function
source("st_parallel.R")

parallel_total <- 0
start_time <- Sys.time()

# inputs
basinRivers <- "FR_five_basins.shp"
barriers <- "FR_five_basins_barriers.shp"
epsg <- 2192
ncores <- 1

Rprof(gc.profiling = TRUE, line.profiling = TRUE)

message("number cores detected: ", detectCores())
message("number cores running: ", ncores)

message(paste0('Calculating barrier free length for ', basinRivers, ' using ', ncores, ' cores.'))

# rivers
basinRivers <- st_read(dsn = basinRivers) %>% st_transform(epsg)

# barriers
barriers <- st_read(dsn = barriers) %>%
  select(LabelAtlas) %>%
  st_transform(epsg)

snapped <- barriers

# study site boundary buffered to 10 km
boundary <- 'FR_boundary_10km.shp'
boundary <- st_read(dsn = boundary) %>%
  st_transform(epsg)

##############
# Second, cut rivers with points
##############

# Functions

# Snap barriers to river
st_snap_points_par = function(x, y, max_dist = 1000) {

  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  out = do.call(c,
                mclapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

# A helper function that erases all of y from x:
# st_erase = function(x, y) st_difference(x, st_union(st_union(y))) # using st_union instead of st_combine

st_erase = function(x, y) st_difference(x, st_union(y)) # we only need one st_union, not two...

# Snap barriers
##############
## snapping is quick in QGIS so skip this step and use QGIS
## if your dataset is large
##############
message('Snapping barriers snapped to the river...')
# snapped <- st_snap_points_par(barriers, basinRivers, 100) %>%
#   st_cast("POINT") %>%
#   st_zm(drop = TRUE, what = "ZM")  %>%
#   as('Spatial') %>%
#   st_as_sf() %>%
#   st_transform(epsg)

# Buffer barriers
message('Buffering barriers for cutting the river...')
buffBarriers <- st_parallel(snapped, st_buffer, ncores, dist = 0.0001) %>%
  as('Spatial') %>%
  st_as_sf() %>%
  st_transform(epsg)

# to make things faster we could only process a subset
# select river sections with barriers on them and export then only
# conduct the st_erase step on those sections only
# then bind the rows back together
# i.e. rows_without_barriers + cut_rows_with_barriers

# Difference river network using buffered barriers
message('Erasing rivers erased using buffered barriers...')
diffRivers <- st_erase(basinRivers, buffBarriers) %>%
  st_transform(epsg) # can get stuck here with invalid geom

#   st_erase = function(x, y) st_difference(x, st_union(y)) # we only need one st_union, not two...
# union <- st_parallel(buffBarriers, st_union, ncores) %>%
#   st_transform(epsg)
# diffRivers <- st_parallel(basinRivers, st_difference, ncores, union) %>%
#   st_transform(epsg)


# Create river polygons and split them into seperate segments
message('Buffering differenced rivers...')
buffRivers <- st_parallel(diffRivers, st_buffer, ncores, dist = 0.00009)

# Clip river polygons to study site boundary
clipRiver <- st_parallel(buffRivers, st_crop, ncores, boundary)

# Fix geom just in case
message('Fixing invald geometry...')
geomfix <- st_parallel(clipRiver, st_make_valid, ncores) %>%
    as('Spatial') %>%
    st_as_sf()

# Dissolving buffered and clipped rivers
message('Dissolving river polygons...')
dissRivers <- st_parallel(geomfix, st_union, ncores) %>%
    as('Spatial') %>%
    st_as_sf()


message('Casting river polygon into singlepart polygons...')
singlepartpoly <- st_cast(dissRivers, 'POLYGON') %>%
  mutate(id = as.factor(row_number()))


# # I think this is parallelised. Although it's only 0.5 seconds faster than in series...
# message('Converting river polygons into river lines in parallel...')
# singlepartline <- do.call(rbind, mclapply(1:nrow(diffRivers),
#   function(i){st_cast(diffRivers[i,],"LINESTRING")}), ncores)


message('Converting river polygons into river lines in parallel...')
singlepartline <- do.call(rbind, mclapply(1:nrow(diffRivers),
  function(i){st_cast(diffRivers[i,],"LINESTRING")}))

# Bring attributes back together
message('Joining attributes joined back to basin rivers...')
joinedRivers <- st_parallel(singlepartline, st_join, ncores, singlepartpoly)

# calculate components of BFL
message('Generating final output..')
joinedRivers$seglen <- st_length(joinedRivers)

basinlen <- joinedRivers %>%
    group_by(basinID) %>%
    summarise(basinlen = sum(seglen)) %>%
    st_drop_geometry()

fraglen <- joinedRivers %>%
  group_by(id) %>%
  summarise(fraglen = sum(seglen)) %>%
  st_drop_geometry()

BFLS <- joinedRivers %>%
  left_join(basinlen, by = 'basinID') %>%
  left_join(fraglen, by = 'id') %>%
  mutate(BFLS = as.numeric(fraglen/basinlen))

# write output
message('Writing output...')
timedate <- paste0(format(Sys.time(), "%H%M%S"), "_", Sys.Date())
st_write(BFLS, paste0("BFLS", timedate, ".shp"), delete_layer = TRUE) # overwrites

Rprof(NULL)
summaryRprof()

end_time <- Sys.time()
total_time <- as.numeric(end_time - start_time, unit="secs")
message('total time = ', total_time)
message('parallel total = ', parallel_total)
message('TIME IN PARALLEL: ', (parallel_total / total_time)*100, '%')

quit()
