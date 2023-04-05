# split out CA shapefiles

library(sf)
library(terra)

setwd("./ch1_margulis/")

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)

# read in full huc8
huc8_v1 <-vect("./vectors/WBDHU8_Central_Valley_UTMz10/WBDHU8_Central_Valley_UTMz10.shp")
huc8 <-project(huc8_v1, crs(snsr))
huc8[1]
plot(huc8[14])
plot(huc8[15], add = TRUE)

# pull out and save

### american
upper <-huc8[14]
lower <-huc8[15]
american <-union(upper,lower)
american_v2 <-aggregate(american)
plot(american_v2)
writeVector(american_v2, "./vectors/american.gpkg")

### yuba
huc8$NAME[12]
yuba <-huc8[12]
plot(yuba)
writeVector(yuba, "./vectors/yuba.gpkg")

### cosumnes
huc8$NAME[48]
cosumnes <-huc8[48]
plot(cosumnes)
writeVector(cosumnes, "./vectors/cosumnes.gpkg")

### mokelumne
huc8$NAME[47]
mokelumne <-huc8[47]
plot(mokelumne)
writeVector(mokelumne, "./vectors/mokelumne.gpkg")
