# 14/08/2019
# testing parallel with st_snap custon function

# usage: Rscript snap_parallel.R

library(dplyr)
library(sf)
library(parallel)

# both versions of the st_parallel function
source("st_parallel.R")

barriers <- 'CO_barriers_ED50.shp'
basinRivers <- 'CO_basin_rivers.shp'
epsg <- 2192
ncores <- 2

message('####################################################')
message('loading rivers')
message('####################################################')

# rivers
basinRivers <- st_read(dsn = basinRivers) %>% 
  st_transform(epsg) %>%
  select(basinID, geometry)

message('####################################################')
message('loading barriers')
message('####################################################')

# barriers
barriers <- st_read(dsn = barriers) %>%
  st_transform(epsg) %>%
  select(LabelAtlas, geometry)

# Snap barriers to river
st_snap_points = function(x, y, max_dist = 1000) {

  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

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

message('####################################################')
message('snapping barriers')
message('####################################################')

start.time <- Sys.time()

# in parallel... 
message('Snapping barriers snapped to the river in parallel...') 
snapped <- st_snap_points_par(barriers, basinRivers, 100) %>%
  st_cast("POINT") %>%
  st_zm(drop = TRUE, what = "ZM")  %>%
  as('Spatial') %>%
  st_as_sf() %>%
  st_transform(epsg)

end.time <- Sys.time()
time.taken <- end.time - start.time
message('time taken in parallel = ', time.taken)

st_write(snapped, './CO_barriers_ED50_parsnapped.shp')

# Snap barriers
##############
## snapping is quick in QGIS so skip this step and use QGIS
## if your dataset is large
##############
start.time <- Sys.time()

message('Snapping barriers snapped to the river in series...') 
snapped <- st_snap_points(barriers, basinRivers, 100) %>%
  st_cast("POINT") %>%
  st_zm(drop = TRUE, what = "ZM")  %>%
  as('Spatial') %>%
  st_as_sf() %>%
  st_transform(epsg)

end.time <- Sys.time()
time.taken <- end.time - start.time
message('time taken in series = ', time.taken)

st_write(snapped, './CO_barriers_ED50_sersnapped.shp')