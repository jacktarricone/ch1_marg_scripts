# jack tarricone
# november 3, 2022

#### snow metric functions

# functions for calculating different snowpack metrics
# applied on a pixel-wise basis to the margulis et al. 2016 dataset
# swe values are in mm


#######################################
############   max_swe   ##############
#######################################

# maximum annual of SWE
# abbr: max_swe

# just max...

#######################################
############     scf      #############
#######################################

# create snow cover frequency function
# number of days there is snow on the ground vs total days in a given year
# set threshold to 5.1 mm or .2 inches because thats the accuracy a SNTOEL site has
# this will also reduce the long tails seen on the data and unrealistically large scf number

# abbr: scf

scf <-function(x){
  
  length(which(x > 5.1))/length(x)
  
} 

#######################################
############     sdd      #############
#######################################

# snow disappearance date: dowy snow is gone
# need 14 days of continuous snow cover to be considered
# abbr: sdd

sdd <-function(x){
  
  # return 0 for values that never reach the 5.1 mm threshold
  if (max(x) < 5.1){
    return(0)
  } else {
    # use which function to index the days that meet condition
    # reverse the vector
    days_vector <-as.numeric(rev(which(x > 5.1)))
    
    # take the difference of each vector point and make positive
    # we do this bc we are looking for the first point that meets condition
    # and does this for 14 consecutive days
    diff_vect <-diff(days_vector*-1)
    
    # use rle function to calc the lengths of continuous number sequences
    # if there isnt a 14 days with continuous snow cover, make pixel 0
    # this fixes the code bc accounts for pixels which may have had 10 days of sc
    
    # which therefore was out of the previous condition set and caused the error
    # found this about by testing different areas, and saw was more in east half
    result <- rle(diff_vect)
    if (max(result$lengths) < 14){
      return(0)
    } else{
      #represents place on rle vector where condition is met
      meets <-as.numeric(min(which(result$lengths >= 14)))
      
      #if its the first rle value, this means day 365 so max of days_vecto0r
      if (meets == 1) {
        sdd1 <- max(days_vector)
        return(sdd1)
      } else {
        #calculate legnth needed to be cut off days vector from rle
        values_to_cut <-result$lengths[1:(meets-1)]
        #sum the lengths
        index_values_until_start <-as.numeric(sum(values_to_cut))
        #subtract that many values off to get your SDD
        sdd2 <-max(days_vector[-c(1:index_values_until_start)])
        return(sdd2)
      }
    }
  }
}
