# Zonal Statistics for variable GDP
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>tion, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

source("code/setup.R")

aoi = st_read("data/vector/basins_mask.gpkg")
bbox = st_bbox(aoi)
files = list.files("data/raster/gdp/", full.names = T)
col = create_image_collection(files, date_time = paste(2000:2019, "-01-01", sep = ""), band_names = "gdp")
ext = extent(col)
ext$t1 = c("2019-12-31")
view = cube_view(dx = 0.833, dy = 0.833, dt = "P1M",
                 aggregation = "mean", resampling = "bilinear",
                 srs = "EPSG:4326", extent = ext)

gdalcubes_options(threads = 8)
raster_cube(col, view, chunking = c(240, 1000, 1000)) %>%
  fill_time(method = "locf") %>%
  zonal_statistics(aoi, expr = "mean(gdp)", as_stars = T) -> zonal_gdp

