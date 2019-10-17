library(dplyr)
library(sf)
library(parallel)
library(lwgeom)
library(ggplot2)

source("st_parallel.R")

# inputs
load('run-image.RData')

## This is much faster than direct computation of distances with lapply (approx 5x)
within_distance <- function (base, df, distance = 0.001) {
  list <- st_is_within_distance(base, df,distance)
  nearest <- df[list[[1]],]
}

# filter data set down
friver <- within_distance(basinRivers[1,'geometry'], basinRivers, 5000)
fbarrier <- within_distance(basinRivers[1,'geometry'], barriers, 5000)


friver['rowname'] <- rownames(friver)
fbarrier['rowname'] <- rownames(fbarrier)

fjoinedriver <- within_distance(basinRivers[1,'geometry'], joinedRivers, 5000)
fjoinedriver['rowname'] <- rownames(fjoinedriver)

library(hues)
ggplot(friver) + geom_sf(aes( color=rowname ), size=2) + geom_sf(data=fbarrier, size=5, aes(color=rowname))

ggplot(fjoinedriver) + geom_sf(aes( color=iwanthue(n=45, random=T) ), size=2) + geom_sf(data=fbarrier, size=5, aes(color=iwanthue(n=13, random=T)))
