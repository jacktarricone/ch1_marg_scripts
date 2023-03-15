# read in txt file
topo_v1 <-read.delim("./gis/topo_colors_raw.txt")
topo_df <-as.data.frame(topo_v1)
colnames(topo_df)[1] <-'colors'
head(topo_df)

# function for preserving last n figures in string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# chop off all but last 8
v1 <-substrRight(topo_df$colors, 8)

# remove space
v2 <-gsub(" ", "",v1)

# make string
final <-c(v2)
write.table(v2, "./gis/topo_colors.txt", row.names = FALSE, col.names = FALSE)
