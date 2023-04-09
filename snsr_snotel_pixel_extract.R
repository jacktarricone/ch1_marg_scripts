# pull out snotel pixels of SNSR hdf
# save per station per year

library(rhdf5)
library(terra)
library(dplyr)
library(parallel)
library(pbmcapply)

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

# fine hdf swe files
hdf_paths <-list.files("./swe/hdf", full.names = TRUE) # set paths
# x <-hdf_paths[8]

# define function
snotel_snsr_extract <-function(x){
  
  # this is sloppy but it works
  for(i in seq_len(nrow(snsr_snotels))) {
    
    # file
    file <-basename(x)
    
    # pull out number of days in given year
    test <-h5ls(x) # contains 3 groups: lat, long, and SCA
    dims <-test$dim[1]
    nday <-as.integer(sub("6601 x 5701 x ","",dims))
    
    # use df which has SNOTEL station SNSR cell numbers
    # to read in single pixel where snotel station is
    swe_raw <-h5read(x, "/SWE", 
                          index = list(snsr_snotels$y_cell_num[i], 
                                       snsr_snotels$x_cell_num[i], 
                                       1:nday))
    
    # convert to df
    snsr_swe_mm <-as.data.frame(matrix(swe_raw))
    colnames(snsr_swe_mm)[1] <- "snsr_swe_mm" # change 3rd col name to sca
    
    # extract year
    year_v1 <- gsub(".h5", "", file) # take off .h5
    year <- gsub("SN_SWE_WY", "", year_v1) # take of begining letters
    
    # format names, remove spaces
    snotel_name_v1 <-snsr_snotels$Site_Name[i]
    snotel_name <-gsub(" ", "_", snotel_name_v1)
    
    # pull out other data
    wy <-rep(year,nday)
    site_name <-rep(snotel_name,nday) 
    latitude <-rep(snsr_snotels$cell_lat[i], nday)
    longitude <-rep(snsr_snotels$cell_lon[i], nday)
    ele_m <-rep(snsr_snotels$Elevation_m[i], nday)
    station_id <-rep(snsr_snotels$cell_lat[i], nday)
    cell_number <-rep(snsr_snotels$cell[i], nday)
    x_cell <-rep(snsr_snotels$x_cell_num[i], nday)
    y_cell <-rep(snsr_snotels$y_cell_num[i], nday)
    
    # make df
    final_df <-cbind(site_name, wy, snsr_swe_mm, 
                     latitude, longitude, ele_m,
                     station_id, cell_number, x_cell, y_cell) # bind all 3 cols
    head(final_df)
    
    # create saving information
    saving_location <-file.path("./csvs/snsr_snotel_data/")
    full_saving_name <-paste0(saving_location,"swe_",year,"_",snotel_name,".csv")
    
    # save
    write.csv(final_df, full_saving_name, row.names=FALSE)
  }
}

# apply function to list of hdf files
pbmclapply(hdf_paths, function(x) snotel_snsr_extract(x), mc.cores = 14, mc.cleanup = TRUE)
