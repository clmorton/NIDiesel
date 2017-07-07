# Aim: pull down petrol stations near border with Ireland

library(rnaturalearth)
library(osmdata)
library(stplanr)
library(mapview)
library(tmap)
library(sf)

# # Get and process boundary data
# q = add_feature(opq = opq(bbox = "Northern Ireland"), key = "boundary", value = "administrative") %>% 
#   add_feature(key = "boundary_type", value = "nation")
# res = osmdata_sf(q = q)
# res
# plot(res$osm_lines)
# b = res$osm_lines # create separte border object
# b = st_union(b)
# dir.create("Northern Ireland/Outputs")
# write_sf(b, "Northern Ireland/Outputs/border-osm.geojson")
# mapview(b)
# source("https://github.com/cyipt/cyipt/raw/master/R/geobuffer.R")
# b_buff = geo_buffer(b, 10000)
# plot(b_buff$geometry)
# plot(b_buff)
# write_sf(b_buff, "Northern Ireland/Outputs/border-osm-10k.geojson")

# # get buffer in ireland
# ireland = rnaturalearth::ne_countries(scale = 10, country = "Ireland")
# plot(ireland)
# ireland = st_as_sf(ireland)
# b_ireland = st_intersection(x = b_buff, ireland)
# plot(b_ireland)
# write_sf(b_ireland, "Northern Ireland/Outputs/border-osm-10k-in-ireland.geojson")

# # Get petrol stations in the border area
# bbb = bbox(as(object = b_buff, Class = "Spatial"))
# q_petrol = add_feature(opq = opq(bbox = bbb), key = "amenity", value = "fuel")
# res_petrol = osmdata_sf(q = q_petrol)
# b_stations = res_petrol$osm_points[b_ireland,]
# mapview(b_stations)
# write_sf(b_stations, "Northern Ireland/Outputs/b_stations.geojson")

# Load pre-saved files
b_ireland = read_sf("Northern Ireland/Outputs/border-osm-10k-in-ireland.geojson")
b_stations = read_sf("Northern Ireland/Outputs/b_stations.geojson")
b = read_sf("Northern Ireland/Outputs/border-osm.geojson")
z = read_sf("Northern Ireland/Shapefiles/DistMeasure/SOACentroids/SOACentroids.shp")
za = read_sf("Northern Ireland/Shapefiles/SOA2011_Linked.shp")

# Find distance to nearest
z = st_transform(z, st_crs(b_stations))
dists = st_distance(x = z, b_stations)
mindist_1 = which.min(x = dists[1,])
near1 = b_stations[mindist_1,]
mapview(z[1,]) +
  mapview(near1)

# find route distance/time
dist_google(from = st_coordinates(z[1,]), to = st_coordinates(near1))
route_graphhopper(from = st_coordinates(z[1,]), to = st_coordinates(near1), vehicle = "car")

# Do for all zones
i = 7
r_all = NULL
for(i in 502:nrow(z)) {
  mindist_1 = which.min(x = dists[i,])
  near1 = b_stations[mindist_1,]
  r = route_graphhopper(from = st_coordinates(z[i,]), to = st_coordinates(near1), vehicle = "car")
  if(i == 1) {
    r_all = r
  } else {
    r_all = rbind(r_all, r)
  }
}
plot(r_all)

# check result
tmap_mode("view")
tm_shape(r_all) +
  tm_lines(col = "dist", alpha = 0.5, palette = "YlOrRd")
r_all_sf = st_as_sf(x = r_all)
write_sf(r_all_sf, "Northern Ireland/Outputs/r_all.geojson")
za$dist = NA
za$dist[1:nrow(r_all)] = r_all$dist

qtm(za, "dist", palette = "YlOr)
