library(R.matlab)


# read in matlab
raw_mat <-readMat("~/Downloads/CDEC_SWE_WY1985_2021.mat")

test <-readMat("~/Downloads/CDEC_SWE_peak_reanalysis_best_match.mat")

# extract meta data
names <-raw_mat$CDEC[1] %>% unlist()
lat <-raw_mat$CDEC[2] %>% unlist()
lon <-raw_mat$CDEC[3] %>% unlist()
ele <-raw_mat$CDEC[4] %>% unlist()

# 
test <-raw_mat$Peak.SWE.CDEC

df.import.mat <- data  %>% as_tibble(.name_repair = ~c("allData"))%>% 
  unnest_longer(col = allData, indices_include = FALSE) %>%
  unnest_wider("allData", simplify = TRUE, names_repair = ~key) %>% 
  mutate(eegid = unlist(eegid))
