# pull out snotel pixels of SNSR hdf
# save per station per year

library(rhdf5)
library(terra)
library(dplyr)
library(parallel)
library(pbmcapply)
library(snotelr)

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

# read in snsr shp
snsr_shp <-vect("./vectors/snsr_shp.gpkg")
plot(snsr_shp)

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539,
                             540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

# filter for CA
snotel_ca <-filter(snotel_locs, Site_ID %in% good_stations)
snotel_ca

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))
snotel_vect <-vect(snotel_locs, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))

# mask points with snsr shp to get all points in shp
snsr_snotels <-mask(snotel_vect, snsr_shp)
plot(snsr_shp)
plot(snsr_snotels, add = TRUE)

### back to df after filtering
new_stations_v2 <-as.data.frame(snsr_snotels)
# write.csv(new_stations_v2, "./csvs/snsr_snotel_stations.csv")
old_stations <-snotel_ca$Site_Name

# filter for stations no in other string aka new stations!
new_stations_v1 <-filter(new_stations_v2, !Site_Name %in% old_stations)

# filter orginal df to add back lat/lon data
new_stations <-filter(snotel_locs, Site_Name %in% new_stations_v1$Site_Name)
new_stations

# creat vector
new_stations_vect <-vect(new_stations, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))
plot(snsr_shp)
plot(new_stations_vect, add = TRUE)

#### extract pixel numbers
snsr_snotel_pixel_nums <-terra::extract(cn_v1, new_stations_vect, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'simple')

snsr_snotel_pixel_nums

# bind and save
snsr_snotels <-cbind(new_stations, snsr_snotel_pixel_nums)
colnames(snsr_snotels)[14:15] <-c("cell_lon","cell_lat")
snsr_snotels
# write.csv(snsr_snotels, "./csvs/snsr_snotels.csv")

# read in data to check the start date on it all
snotel_df_list <-as.list(rep(NA, nrow(snsr_snotels)))

# read in SNOTEL CSVs using ID number and name from meta data
for (i in seq_along(snsr_snotels$Site_Name)){
  snotel_df_list[[i]] <-as.data.frame(snotel_download(snsr_snotels$Site_ID[i], path = tempdir(), internal = TRUE))
}

# convert from list of dfs to one big one
snotel_df <-bind_rows(snotel_df_list, .id = "network")
snotel_df$date <-lubridate::ymd(snotel_df$date)
head(snotel_df)

# add water year
colnames(snotel_df)[12] <-"Date"
colnames(snotel_df)[13] <-"snotel_swe_mm"
snotel_df <-dataRetrieval::addWaterYear(snotel_df)
head(snotel_df)


# calculate minimum year for each station
start_date_df <-as.data.frame(snotel_df %>%
                         group_by(site_name) %>%
                         summarise(first_year = min(waterYear)))

start_date_df 

# new stations first WY: 
# burnside (2004), carson (2005), forestdale (2004)
# horse meadow (2004), leavitt (1990)

# filter for data with the full time series
snsr_snotels_full_ts <-snsr_snotels[-c(1,3,5,6,7),]
snsr_snotels_full_ts

# fine hdf swe files
hdf_paths <-list.files("./swe/hdf", full.names = TRUE) # set paths

# define function
snotel_snsr_extract <-function(x,snsr_snotels){
  
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
    station_id <-rep(snsr_snotels$Site_ID[i], nday)
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
    full_saving_name <-paste0(saving_location,snotel_name,"_swe_",year,".csv")
    
    # save
    write.csv(final_df, full_saving_name, row.names=FALSE)
  }
}

# apply function to list of hdf files
pbmclapply(hdf_paths[2:32], function(x) snotel_snsr_extract(x, snsr_snotels = snsr_snotels_full_ts), 
           mc.cores = 14, mc.cleanup = TRUE)
#lapply(hdf_paths[1], function(x) snotel_snsr_extract(x, snsr_snotels = snsr_snotels_full_ts))



# burnside (2004), carson (2005), forestdale (2004)
# horse meadow (2004), leavitt (1990)

# filter for data with the full time series
burnside <-filter(snsr_snotels, Site_Name == "BURNSIDE LAKE")
carson <-filter(snsr_snotels, Site_Name == "CARSON PASS")
forestdale <-filter(snsr_snotels, Site_Name == "FORESTDALE CREEK")
horse <-filter(snsr_snotels, Site_Name == "HORSE MEADOW")
leavitt <-filter(snsr_snotels, Site_Name == "LEAVITT LAKE")

# define the years to start from for the other 5 stations
start_date_df
start_2004 <-hdf_paths[20:32]
start_2005 <-hdf_paths[21:32]
start_1990 <-hdf_paths[6:32]

# pull out all stations that start in WY 2004 (burnside, forestdale, horse)
pbmclapply(start_2004, function(x) snotel_snsr_extract(x, snsr_snotels = rbind(burnside, forestdale, horse)), 
           mc.cores = 14, mc.cleanup = TRUE)

# pull out all stations that start in WY 2005 (carson)
pbmclapply(start_2005, function(x) snotel_snsr_extract(x, snsr_snotels = carson), 
           mc.cores = 14, mc.cleanup = TRUE)

# pull out all stations that start in WY 1990 (leavitt)
pbmclapply(start_1990, function(x) snotel_snsr_extract(x, snsr_snotels = leavitt), 
           mc.cores = 14, mc.cleanup = TRUE)
