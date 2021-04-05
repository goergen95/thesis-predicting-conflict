# Spatiotemporal aggregation of conflict data set
# Copyright (C) 2021 Darius A. Görgen
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

source("code/setup.R")
# create response objects per buffer

files = list.files("data/vector", pattern = "mask", full.names = T)
load("data/raw/ged/ged201.RData")

mask = st_read("data/vector/states_mask.gpkg")
# prepare conflict data
ged201$date_start = as.Date(ged201$date_start, "%Y/%m/%d")
ged201 = st_as_sf(ged201, coords =  c("longitude", "latitude"), crs = "EPSG:4326")
ged201 = ged201[which(st_intersects(st_union(mask), ged201, sparse = FALSE)), ]

ged201 %>%
  as_tibble() %>%
  mutate(date_start =  as.Date(date_start, "%Y/%m/%d")) %>%
  filter(date_start > as.Date("1999-12-31"), 
         where_prec %in% c(1,2,3),
         date_prec != 5) %>%
  select(type_of_violence, date_start, starts_with("deaths_"), country, geometry) -> response_raw
# aggregate all types of deaths
response_raw %>% 
  mutate(deaths = deaths_civilians + deaths_a + deaths_b + deaths_unknown) %>%
  select(type_of_violence, date_start, deaths, geometry) -> agg_data 


# prepare space time objects for aggregation
date_vec = seq(as.Date("2000-01-01"), as.Date("2019-12-31"), by = "month")


st_polys = lapply(files, function(x){
  tmp = st_read(x, quiet = T)
  STF(as_Spatial(tmp), date_vec)
})

# declare classes of conflict
classes = c("sb", "ns", "os", "all")


for(p in 1:length(st_polys)){
  poly = st_polys[[p]]
  filename = 
    
    for(i in 1:4){ # loop through types of conflict + all types
      print(i)
      if(i != 4){ # filter if appropriate
        points = as_Spatial(st_as_sf(agg_data[agg_data$type_of_violence == i, ]))
      } else { # else take all conflicts
        points = as_Spatial(st_as_sf(agg_data)) 
      }
      # declaration of a space-time data frame
      points_st = STIDF(points, points$date_start, data = data.frame(casualties = points$deaths))
      # aggregate the sum of deahts
      agg_poly = aggregate(points_st, poly, sum, na.rm = T)
      # convert to sf object
      agg_poly@data[is.na(agg_poly@data)] = 0 # set NA to 0 for no conflict
      agg_poly = as(agg_poly, "Spatial")
      agg_poly = st_as_sf(agg_poly)
      st_crs(agg_poly) = st_crs(4326)
      names(agg_poly)[1:(ncol(agg_poly)-1)] = format(as.Date(names(agg_poly)[1:(ncol(agg_poly)-1)]), "%Y-%m")
      filename = paste0("data/vector/response_", classes[i], "_", paste(str_split(basename(files[p]), "_")[[1]][1:2], collapse = "_"), ".gpkg")
      st_write(agg_poly, dsn = filename, delete_dsn = T)
    }
}

files = list.files("data/vector/response/", pattern = ".gpkg$", full.names = T)

data = lapply(files, function(x){
  filename = str_split(basename(x), "_")[[1]]
  type = filename[2]
  unit = filename[3]
  buffer = str_remove_all(filename[4], ".gpkg")
  if(buffer == "mask") buffer = "0"
  tmp = as_tibble(st_drop_geometry(st_read(x, quiet = T)))
  tmp$id = 1:nrow(tmp) 
  
  tmp %>%
    gather(time, value, -id) %>%
    mutate(time = as.Date(paste0(str_replace_all(str_sub(time, 2,8), "\\." , "-"), "-01"), format = "%Y-%m-%d"),
           unit = unit, buffer = buffer, type = type,
           type = if_else(buffer == "0", type, paste0(type, "_", buffer))) %>%
    select(-buffer) %>%
    relocate(id, time, unit, type, value)
  })

data = do.call(rbind, data)

data %>%
  as.tbl_cube(dim_names = 1:4) -> data_cube

saveRDS(data_cube, "data/vector/response_cube.rds")

