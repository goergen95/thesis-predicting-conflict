library(sf)

states = st_read("data/vector/states.gpkg")
states = st_make_valid(states)
basins = st_read("data/raw/hydrosheds/hybas_af_lev05_v1c.shp")
basins = st_make_valid(basins)

# reprojecting to planar
basins = st_transform(basins, st_crs(3006))
states = st_transform(states, st_crs(3006))

# croping basins to states
maskS = st_union(states)
basins = st_intersection(basins, maskS)

# cropping states to basins
maskB = st_union(basins)
states = st_intersection(states, maskB)


clip_buffer <- function(shape, dist, mask){
  buf = st_buffer(shape, dist = dist)
  buf = do.call(rbind, lapply(1:nrow(shape), function(i){
    st_difference(buf[i,], shape[i,])
  }))
  st_transform(st_crop(buf, mask), 4326)
}

basins_50 = clip_buffer(basins, 50000, maskB)
basins_100 = clip_buffer(basins, 100000, maskB)
basins_200 = clip_buffer(basins, 200000, maskB)

states_50 = clip_buffer(states, 50000, maskB)
states_100 = clip_buffer(states, 100000, maskB)
states_200 = clip_buffer(states, 200000, maskB)

basins = st_transform(basins, st_crs(4326))
states = st_transform(states, st_crs(4326))

st_write(basins, "data/vector/basins_mask.gpkg", delete_dsn = TRUE)
st_write(states, "data/vector/states_mask.gpkg", delete_dsn = TRUE)
st_write(basins_50, "data/vector/basins_50_mask.gpkg", delete_dsn = TRUE)
st_write(states_50, "data/vector/states_50_mask.gpkg", delete_dsn = TRUE)
st_write(basins_100, "data/vector/basins_100_mask.gpkg", delete_dsn = TRUE)
st_write(states_100, "data/vector/states_100_mask.gpkg", delete_dsn = TRUE)
st_write(basins_200, "data/vector/basins_200_mask.gpkg", delete_dsn = TRUE)
st_write(states_200, "data/vector/states_200_mask.gpkg", delete_dsn = TRUE)