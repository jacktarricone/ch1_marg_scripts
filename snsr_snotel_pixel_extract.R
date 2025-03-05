# pull out snotel pixels of SNSR hdf
# save per station per year

library(rhdf5)
library(terra)
library(dplyr)
library(parallel)
library(pbmcapply)
library(data.table)
library(tidyr)
library(ggplot2)

setwd("~/ch1_margulis/")

# bring in real snotel locations
snotel_locs <-fread("./csvs/SNOTEL_MASTER_pt_update.csv")
head(snotel_locs)

# read in stack
dem <-rast('./rasters/static/SNSR_DEM.tif')
y_cell <-rast('./rasters/static/y_cell_num.tif')
x_cell <-rast('./rasters/static/x_cell_num.tif')
cell_numbers <-c(x_cell,y_cell)
cn_v1 <-mask(cell_numbers, dem)

# read in snsr shp
snsr_shp <-vect("./vectors/snsr_shp.gpkg")

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539, 
                             540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

# filter for CA
snotel_ca <-filter(snotel_locs, Site_ID %in% good_stations)
snotel_ca

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))
snotel_vect <-vect(snotel_locs, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))
snsr_snotels <-mask(snotel_vect, snsr_shp)


# crop down to extent of points test
crop_ext <-ext(-120.79192, -119, 38, 39.8)
cn <-crop(cn_v1, crop_ext)
ca_points_snsr <-crop(ca_points, crop_ext)

# crop 
plot(cn[[2]])
plot(ca_points_snsr, add =TRUE)

# extract pixels
# ??terra::extract
snsr_snotel_pixel_nums <-terra::extract(cn, ca_points_snsr, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'simple')

colnames(snsr_snotel_pixel_nums)[4] <-c("cell1")

# extract 8 surronding cells
test_cells <-adjacent(cn, snsr_snotel_pixel_nums$cell1, direction = 8)

# for five stations
with_cells <-cbind(snsr_snotel_pixel_nums,test_cells)
colnames(with_cells)[7:14] <-c("cell2","cell3","cell4","cell5",
                               "cell6","cell7","cell8","cell9")

cells_long <- with_cells %>%
  pivot_longer(cols = starts_with("cell"), 
               names_to = "cell_type", 
               values_to = "cell")

cells_long_v2 <-cells_long[-c(2:5)]

full_pixel_nums <-terra::extract(cn, cells_long_v2$cell)
colnames(full_pixel_nums)[1:2] <-c("x_cell_num2","y_cell_num2")

# bind and save
snsr_snotels <-cbind(snotel_ca, with_cells)
snsr_snotels
colnames(snsr_snotels)[14:15] <-c("lon","lat")
head(snsr_snotels)
# write.csv(snsr_snotels, "./csvs/snsr_snotels.csv")

df_long <- snsr_snotels %>%
  pivot_longer(cols = starts_with("cell"), 
               names_to = "cell_type", 
               values_to = "cell")

df_long2  <-cbind(df_long,full_pixel_nums)

# create df to define max and min for cells
new_cells_df <-df_long2 %>%
  group_by(Site_Name) %>%
  summarise(
    max_x = max(x_cell_num2, na.rm = TRUE),
    min_x = min(x_cell_num2, na.rm = TRUE),
    max_y = max(y_cell_num2, na.rm = TRUE),
    min_y = min(y_cell_num2, na.rm = TRUE),
  )

new_cells_df
cn_v1

# fine hdf swe files
hdf_paths <-list.files("./swe/hdf", full.names = TRUE) # set paths
i <-2
x <-hdf_paths[9]

# define function
snotel_9cell_snsr_extract <-function(x){
  
  # this is sloppy but it works
  for(i in seq_len(nrow(new_cells_df))) {
    
    # file
    file <-basename(x)
    
    # pull out number of days in given year
    test <-h5ls(x) # contains 3 groups: lat, long, and SCA
    dims <-test$dim[1]
    nday <-as.integer(sub("6601 x 5701 x ","",dims))
    
    # use df which has SNOTEL station SNSR cell numbers
    # to read in single pixel where snotel station is
    swe_raw <-h5read(x, "/SWE", 
                     index = list(new_cells_df$min_y[i]:new_cells_df$max_y[i],
                                  new_cells_df$min_x[i]:new_cells_df$max_x[i],
                                  1:nday))
    
    
    # convert to df
    swe_raw2 <-swe_raw # duplicate
    dim(swe_raw2) <- c(9,nday) # make 9 columns
    t_swe_raw2 <-t(swe_raw2) # transpose
    snsr_swe_mm <-as.data.frame(t_swe_raw2) # convert to df
    head(snsr_swe_mm) # check
    
    # rename cells
    colnames(snsr_swe_mm)[1:9] <- c("cell1","cell2","cell3","cell4",
                                    "cell5","cell6","cell7","cell8","cell9")
    
    # extract year
    year_v1 <- gsub(".h5", "", file) # take off .h5
    year <- gsub("SN_SWE_WY", "", year_v1) # take of begining letters
    
    # format names, remove spaces
    snotel_name_v1 <-snsr_snotels$Site_Name[i]
    snotel_name <-gsub(" ", "_", snotel_name_v1)
    
    # pull out other data
    wy <-rep(year,nday)
    site_name <-rep(snotel_name,nday) 
    latitude <-rep(snsr_snotels$lat[i], nday)
    longitude <-rep(snsr_snotels$lon[i], nday)
    ele_m <-rep(snsr_snotels$Elevation_m[i], nday)
    station_id <-rep(snsr_snotels$Site_ID[i], nday)
    c_number <-rep(snsr_snotels$cell1[i], nday)
    x_cell <-rep(snsr_snotels$x_cell_num[i], nday)
    y_cell <-rep(snsr_snotels$y_cell_num[i], nday)
    dowy <-seq(1,nday,1)
    
    # make df
    final_df <-cbind(site_name, wy, snsr_swe_mm, dowy,
                     latitude, longitude, ele_m,
                     station_id, c_number, x_cell, y_cell) # bind all 3 cols
    
    # make long by cell
    final_df_long <-final_df %>%
      pivot_longer(cols = starts_with("cell"), 
                   names_to = "cell_num", 
                   values_to = "swe_mm")
    
    head(final_df_long)
    
    # create saving information
    saving_location <-file.path("./csvs/snsr_snotel_data2/")
    full_saving_name <-paste0(saving_location,"swe_",year,"_",snotel_name,".csv")
    
    # save
    fwrite(final_df_long, full_saving_name, row.names=FALSE)
  }
}

# apply function to list of hdf files
pbmclapply(hdf_paths, function(x) snotel_9cell_snsr_extract(x), mc.cores = 12, mc.cleanup = TRUE)
# lapply(hdf_paths[14], function(x) snotel_snsr_extract(x))


# test
test_df <-fread("./csvs/snsr_snotel_data2/swe_1993_CSS_LAB.csv")
head(test_df)

ggplot(test_df)+
  geom_line(aes(y = swe_mm, x =  dowy, color = cell_num), size = .1)
