library(rgdal)
library(sp)

opzones <- readOGR("opportunity_zones/opportunity_zones_4326.shp")
parcels <- readOGR("Parcels.shp")

new_parcels <- spTransform(parcels, CRS(proj4string(opzones)))

writeOGR(obj = new_parcels, dsn = "Parcels_4326", layer = "Parcels_4326", driver = "ESRI Shapefile")