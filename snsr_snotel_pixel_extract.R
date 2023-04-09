# pull out snotel pixels of SNSR hdf

library(rhdf5)
library(terra)
library(dplyr)

setwd("~/ch1_margulis/")

# bring in real snotel locations
snotel_locs <-read.csv("./csvs/SNOTEL_MASTER_pt_update.csv")
head(snotel_locs)

# read in stack
dem <-rast('./rasters/static/SNSR_DEM.tif')
y_cell <-rast('./rasters/static/y_cell_num.tif')
x_cell <-rast('./rasters/static/x_cell_num.tif')
cell_numbers <-c(x_cell,y_cell)
cn_v1 <-mask(cell_numbers, dem)
cn_v1
plot(cn_v1)

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539, 
                             540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

# filter for CA
snotel_ca <-filter(snotel_locs, Site_ID %in% good_stations)
snotel_ca

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))

# crop down to extent of points test
crop_ext <-ext(-120.79192, -119, 38, 39.8)
cn <-crop(cn_v1, crop_ext)
ca_points_snsr <-crop(ca_points, crop_ext)

# crop 
plot(cn[[2]])
plot(ca_points_snsr, add =TRUE)

# extract pixels
snsr_snotel_pixel_nums <-terra::extract(cn, ca_points_snsr, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'simple')

snsr_snotel_pixel_nums

# bind and save
snsr_snotels <-cbind(snotel_ca, snsr_snotel_pixel_nums)
colnames(snsr_snotels)[14:15] <-c("cell_lon","cell_lat")
# write.csv(snsr_snotels, "./csvs/snsr_snotels.csv")

# read in SNOTEL CSVs using ID number and name from meta data

for (i in seq_along(snsr_snotels)){
  df_list[[i]] <-as.data.frame(snotel_download(snotel_ca$Site_ID[i], path = tempdir(), internal = TRUE))
}




# pull out number of days in given year
test <-h5ls(swe_list) # contains 3 groups: lat, long, and SCA
dims <-test$dim[1]
nday <-as.integer(sub("6601 x 5701 x ","",dims))

# load in borth half of the data cube for RAM purposes
c1 <-h5read(swe_list, "/SWE", index = list(1:3300,1:5701,1:nday))
print("c1 read into memory")

## calculate pixel-wise max
# returns max value in mm per pixel in the given year
max_c1 <-as.matrix(apply(c1, c(1,2), snow_metric_function)) 
print("c1 max calculated")
rm(c1) # clean up