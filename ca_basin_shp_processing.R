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
# writeVector(american_v2, "./vectors/american.gpkg")

### yuba
huc8$NAME[12]
yuba <-huc8[12]
plot(yuba)
# writeVector(yuba, "./vectors/yuba.gpkg")

### cosumnes
huc8$NAME[48]
cosumnes <-huc8[48]
plot(cosumnes)
# writeVector(cosumnes, "./vectors/cosumnes.gpkg")

### mokelumne
huc8$NAME[47]
mokelumne <-huc8[47]
plot(mokelumne)
# writeVector(mokelumne, "./vectors/mokelumne.gpkg")

### stanislaus
huc8$NAME[45]
stanislaus <-huc8[45]
plot(stanislaus)
# writeVector(stanislaus, "./vectors/stanislaus.gpkg")

### tuolumne
huc8$NAME[44]
tuolumne <-huc8[44]
plot(tuolumne)
# writeVector(tuolumne, "./vectors/tuolumne.gpkg")

### merced
huc8$NAME[43]
merced <-huc8[43]
plot(merced)
# writeVector(merced, "./vectors/merced.gpkg")

### usj
huc8$NAME[41]
usj <-huc8[41]
plot(usj)
# writeVector(usj, "./vectors/usj.gpkg")

### kings
huc8$NAME[36]
kings <-huc8[36]
plot(kings)
# writeVector(kings, "./vectors/kings.gpkg")

### kaweah
huc8$NAME[34]
kaweah <-huc8[34]
plot(kaweah)
# writeVector(kaweah, "./vectors/kaweah.gpkg")

### tule
huc8$NAME[33]
tule <-huc8[33]
plot(tule)
# writeVector(tule, "./vectors/tule.gpkg")

### kern
upper <-huc8[28]
lower <-huc8[29]
kern <-union(upper,lower)
kern_v2 <-aggregate(kern)
plot(kern_v2)
# writeVector(kern_v2, "./vectors/kern.gpkg")

### feather
north <-huc8[9]
east <-huc8[10]
middle <-huc8[11]
feather <-union(north,middle)
plot(feather)
feather_v2 <-union(feather, east)
feather_v3 <-aggregate(feather_v2, dissolve=TRUE)
# writeVector(feather_v3, "./vectors/feather.gpkg")


## upper sac
# define all 9
u_pit <-huc8[1]
l_pit <-huc8[2]
mcloud <-huc8[3]
sac_head <-huc8[4]
cow <-huc8[16]
cotton <-huc8[17]
battle <-huc8[18]
clear <-huc8[19]
paynes <-huc8[20]

# union
v1 <-union(u_pit, l_pit)
v2 <-union(v1, mcloud)
v3 <-union(v2, sac_head)
v4 <-union(v3, cow)
v5 <-union(v4, cotton)
v6 <-union(v5, battle)
v7 <-union(v6, clear)
v8 <-union(v7, paynes)

upper_sac <-aggregate(v8, dissolve=TRUE)
# writeVector(upper_sac, "./vectors/upper_sac.gpkg")

# bring in conus HUC8 vect
conus_huc8 <-vect("./vectors/HUC8_CONUS/HUC8_US.shp")

# convert to string and find # of CA basins
string <-c(conus_huc8$NAME)

# truckee
truckee_num <-which(string == "Truckee")

# tahe
tahoe <-which(string == "Lake Tahoe")

# carson
carson <-which(string == "Upper Carson")

# walker
west_walker <-which(string == "West Walker")
east_walker <-which(string == "East Walker")

# mono
mono <-which(string == "Mono Lake")

# owens
owens <-which(string == "Owens Lake")
crowley <-which(string == "Crowley Lake")

# pull out
truckee <-conus_huc8[truckee_num]
plot(truckee)
# writeVector(truckee, "./vectors/truckee.gpkg")
