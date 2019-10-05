library(dplyr)
library(sf)
library(parallel)
library(lwgeom)

source("st_parallel.R")

# inputs
basinRivers_file <- "FR_five_basins.shp"
barriers_file <- "FR_five_basins_barriers.shp"
epsg <- 2192

basinRivers <- st_read(dsn = basinRivers_file) %>% st_transform(epsg)

barriers <- st_read(dsn = barriers_file) %>%
  select(LabelAtlas) %>%
  st_transform(epsg)

## This is much faster than direct computation of distances with lapply (approx 5x)
within_distance <- function (base, df, distance = 0.001) {
  list <- st_is_within_distance(base, df,distance)

  nearest <- df[list[[1]],]
}

distances <- function(base, df) {
  lapply(df[['geometry']], function(compare) st_distance(compare, base))
}

within_distance_by_dist <- function (base, df, distance = 0.001) {
  logical <- distances(base, df) < distance

  nearest <- df[logical,]
}

facets <- function (gg) {
  df['rowname'] <- rownames(df)
  gg + facet_grid(.~rowname)
}

colgplot <- function (df) {
  df['rowname'] <- rownames(df)
  geom_sf(aes(color=rowname), data=df)
}

gbarriers <- function (gg, df) {
  df['rowname'] <- rownames(df)
  gg + geom_sf(aes(color=rowname), data=df)
}

# can also compute this with within_distance_by_dist, but much slower
connected_rivers <- within_distance(basinRivers[1], basinRivers)
barriers <- head(barriers)
rivers_within_distance <- lapply(barriers[['geometry']], function(barrier) {within_distance(barrier, basinRivers)})


ggplot(basinRivers) + geom_sf() + geom_sf(data=barriers)
gplot(connected)

barriers_within_dist <- within_distance(base, barriers, 1000)

barriers <- rownames_to_column(barriers)
barriers <- head(barriers)
gplot(connected_by_dist) + geom_sf(aes(color=rowname, fill="red", size=100), data=barriers)

mbm <- microbenchmark(
  "no" = { within_distance(base, basinRivers) },
  "yes" = { within_distance_by_dist(base, basinRivers) },
  times = 10L)
autoplot(mbm)


# work out distances
bb <- st_transform(buffBarriers[['geometry']], epsg)
rgeom <- river_geoms[[1]]
dist <- lapply(bb, function(x) {
  st_is_within_distance(rgeom, x, 0.001)
})
bb_filt = bb[dist == 1]


# filter data set down
frivers <- within_distance(basinRivers[1,'geometry'], basinRivers, 5000)
fbarriers <- within_distance(basinRivers[1,'geometry'], barriers, 5000)

# find river for barrier
river_for_barrier <- function (barrier) {
  within_distance(barrier, frivers, 0.001)[['ID_DRAIN']]
}

# Add ID_DRAIN to barriers
fbarriers$ID_DRAIN <- unlist(lapply(fbarriers[['geometry']], river_for_barrier))

# can locate nearest points...
split_river_by_barrier <- function(barrier) {
  river_id <- st_nearest_feature(barrier, frivers)
  river <- frivers[river_id,]

  # find nearest point on river - returns a line from barrier to projected point
  point_on_river <- st_nearest_points(barrier, river)

  # barrier seems to always be the first one in the list, so take the other one
  point <- st_cast(point_on_river, "POINT")[2]

  # This is to avoid need to buffering (i.e. get around floating point precision)
  point <- st_set_precision(point, precision = 1e-05)

  # check that river and point intersect
  #if(!st_intersects(point, river)) {
  #  stop("ERROR: point and river do not intersect")
  #}

  # split and cast to individual linestring
  river_linestring <- st_cast(st_split(river, point))
  river_linestring['rowname'] <- rownames(river_linestring)
}

# doesn't seem to work at the moment (although weirdly it has in the past)

# plot the split rivers
ggplot(river_linestring) + geom_sf(aes(color=rowname))

p<-ggplot(frivers) + geom_sf() + geom_sf(data = fbarrier, size=4, color="blue") + geom_sf(data=point, color="red")
ggplotly(p)
# seems to co-incide...!?? how is this possible...? is this always the case?

# some intersting discussion here:
# https://gis.stackexchange.com/questions/288570/find-nearest-point-along-polyline-using-sf-package-in-r

# plot the drain and river to check they're the same
ggplot(frivers) + geom_sf(aes(color=ID_DRAIN), size=2) + geom_sf(data = fbarriers, aes(color=ID_DRAIN), size=10) + scale_color_gradientn(colours = iwanthue(n=20, random=T))


friver['rowname'] <- rownames(friver)
fbarrier['rowname'] <- rownames(fbarrier)

fjoinedriver <- within_distance(basinRivers[1,'geometry'], joinedRivers, 5000)
fjoinedriver['rowname'] <- rownames(fjoinedriver)

library(hues)
p_before <- ggplot(friver) + geom_sf(aes( color=rowname ), size=2) + geom_sf(data=fbarrier, size=5, aes(color=rowname))

p_after <- ggplot(fjoinedriver) + geom_sf(aes( color=iwanthue(n=45, random=T) ), size=2) + geom_sf(data=fbarrier, size=5, aes(color=iwanthue(n=13, random=T)))

# color by basinID
ggplot(basinRivers) + geom_sf(aes( color=basinID ), size=2)

# color by ID_DRAIN
# Looks like this is one per row
basinRivers['SINGLE_DRAIN_TF'] <- 0; basinRivers[basinRivers[[ 'ID_DRAIN' ]] == basinRivers[10,][['ID_DRAIN']], 'SINGLE_DRAIN_TF'] = 1;
ggplot(basinRivers) + geom_sf(aes( color=SINGLE_DRAIN_TF ), size=2) + facet_grid(SINGLE_DRAIN_TF~.)
