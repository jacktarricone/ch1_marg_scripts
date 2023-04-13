# split out CA shapefiles

library(sf)
library(terra)

setwd("./ch1_margulis/")

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/ca_basins/snsr_shp.gpkg")

# with sf
snsr_v1 <-st_read("./vectors/ca_basins/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)

# read in full huc8
huc8_v1 <-vect("./vectors/ca_basins/WBDHU8_Central_Valley_UTMz10/WBDHU8_Central_Valley_UTMz10.shp")
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
# writeVector(american_v2, "./vectors/ca_basins/american.gpkg")

### yuba
huc8$NAME[12]
yuba <-huc8[12]
plot(yuba)
# writeVector(yuba, "./vectors/ca_basins/yuba.gpkg")

### cosumnes
huc8$NAME[48]
cosumnes <-huc8[48]
plot(cosumnes)
# writeVector(cosumnes, "./vectors/ca_basins/cosumnes.gpkg")

### mokelumne
huc8$NAME[47]
mokelumne <-huc8[47]
plot(mokelumne)
# writeVector(mokelumne, "./vectors/ca_basins/mokelumne.gpkg")

### stanislaus
huc8$NAME[45]
stanislaus <-huc8[45]
plot(stanislaus)
# writeVector(stanislaus, "./vectors/ca_basins/stanislaus.gpkg")

### tuolumne
huc8$NAME[44]
tuolumne <-huc8[44]
plot(tuolumne)
# writeVector(tuolumne, "./vectors/ca_basins/tuolumne.gpkg")

### merced
huc8$NAME[43]
merced <-huc8[43]
plot(merced)
# writeVector(merced, "./vectors/ca_basins/merced.gpkg")

### usj
huc8$NAME[41]
usj <-huc8[41]
plot(usj)
# writeVector(usj, "./vectors/ca_basins/usj.gpkg")

### kings
huc8$NAME[36]
kings <-huc8[36]
plot(kings)
# writeVector(kings, "./vectors/ca_basins/kings.gpkg")

### kaweah
huc8$NAME[34]
kaweah <-huc8[34]
plot(kaweah)
# writeVector(kaweah, "./vectors/ca_basins/kaweah.gpkg")

### tule
huc8$NAME[33]
tule <-huc8[33]
plot(tule)
# writeVector(tule, "./vectors/ca_basins/tule.gpkg")

### kern
upper <-huc8[28]
lower <-huc8[29]
kern <-union(upper,lower)
kern_v2 <-aggregate(kern)
plot(kern_v2)
# writeVector(kern_v2, "./vectors/ca_basins/kern.gpkg")

### feather
huc8$NAME[9]
huc8$NAME[10]
huc8$NAME[11]

north <-huc8[9]
east <-huc8[10]
middle <-huc8[11]
feather <-union(north,middle)
plot(feather)
feather_v2 <-union(feather, east)
feather_v3 <-aggregate(feather_v2, dissolve=TRUE)
plot(feather_v3)
# writeVector(feather_v3, "./vectors/ca_basins/feather.gpkg")


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
# writeVector(upper_sac, "./vectors/ca_basins/upper_sac.gpkg")



# bring in conus HUC8 vect
conus_huc8 <-vect("./vectors/ca_basins/HUC8_CONUS/HUC8_US.shp")

# convert to string and find # of CA basins
string <-c(conus_huc8$NAME)

# truckee
truckee_num <-which(string == "Truckee")

# tahe
tahoe_num <-which(string == "Lake Tahoe")

# carson
carson_num <-which(string == "Upper Carson")

# walker
west_walker_num <-which(string == "West Walker")
east_walker_num <-which(string == "East Walker")

# mono
mono_num <-which(string == "Mono Lake")

# owens
owens_num <-which(string == "Owens Lake")
crowley_num <-which(string == "Crowley Lake")


######## save
truckee <-conus_huc8[truckee_num]
plot(truckee)
# writeVector(truckee, "./vectors/ca_basins/truckee.gpkg")

tahoe <-conus_huc8[tahoe_num]
plot(tahoe)
# writeVector(tahoe, "./vectors/ca_basins/tahoe.gpkg")

carson <-conus_huc8[carson_num]
plot(carson)
# writeVector(carson, "./vectors/ca_basins/carson.gpkg")

west_walker <-conus_huc8[west_walker_num]
east_walker <-conus_huc8[east_walker_num]
walker <-union(west_walker, east_walker)
walker_v2 <-aggregate(walker)
plot(walker_v2)
# writeVector(walker_v2, "./vectors/ca_basins/walker.gpkg")

mono <-conus_huc8[mono_num]
plot(mono)
# writeVector(mono, "./vectors/ca_basins/mono.gpkg")


owens_v1 <-conus_huc8[owens_num]
crowley <-conus_huc8[crowley_num]
owens_v2 <-union(owens_v1, crowley)
owens_v3 <-aggregate(owens_v2)
plot(owens_v3)
# writeVector(owens_v3, "./vectors/ca_basins/owens.gpkg")

## redo feather attempt
feather_sub_basins <-grep('Feather', string, value=TRUE)
feather_sub_basins 

north_feather_num <-which(string == feather_sub_basins[1])
east_feather_num <-which(string == feather_sub_basins[2])
middle_feather_num <-which(string == feather_sub_basins[3])

north_feather <-conus_huc8[north_feather_num]
east_feather <-conus_huc8[east_feather_num]
middle_feather <-conus_huc8[middle_feather_num]

feather_v1 <-union(north_feather, east_feather)
feather_v2 <-union(feather, middle_feather)
plot(feather_v2)
feather_v3 <-aggregate(feather_v2)
plot(feather_v3)
# writeVector(feather_v3, "./vectors/ca_basins/feather.gpkg")
