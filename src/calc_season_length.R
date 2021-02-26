# calc_season_length()

############################################################################################################################# 
# Description:
# Convert the planting and harvesting month values (1-12) into a season length in units of days

# Project: processing_MIRCA2000
# author: Danielle S Grogan
# last update: 2021-02-26
############################################################################################################################# 
### LIBRARIES AND SOURCE CODE ###

library(lubridate)

#############################################################################################################################
# function: turns a numeric month value (e.g., 5) into a character string with 2-digits (e.g., "05")
num_to_2digChar = function(x){
  if(x < 10){
    digChar = paste("0", x, sep="")
  }else{
    digChar = x
  }
  digChar
}

#############################################################################################################################

calc_season_length = function(vec,     # vector of 2 values: planting_month and harvesting_month
                              na.rm=T
){
  
  if(sum(is.na(vec)) == 0){ # both values must be non-NA
    planting_month = vec[1]  # numeric: 1-12 representing months 
    harvest_month  = vec[2]  # numeric: 1-12 representing months 
    
    if(planting_month < harvest_month){     # case: the planting month is earlier in the year than the harvest month
      
      planting.date = ymd(paste("2000-", num_to_2digChar(planting_month), "-15", sep=""))
      harvest.date  = ymd(paste("2000-", num_to_2digChar(harvest_month),  "-15", sep=""))
      season_length = time_length(interval(planting.date, harvest.date), 'day')  # length in days
      
    }else{    # case: the planting month is LATER in the year than the harvest month (crop season goes from one year to the next)
      
      planting.date = ymd(paste("2000-", num_to_2digChar(planting_month), "-15", sep=""))
      harvest.date  = ymd(paste("2001-", num_to_2digChar(harvest_month),  "-15", sep=""))
      season_length = time_length(interval(planting.date, harvest.date), 'day')  # length in days
      
    }
  }else{ # if planting or harvest month is NA, then the season length = NA
    season_length = NA
  }
  season_length
}         

#############################################################################################################################
# function that acts on 2 files: planting_month and harvest_month
# test 
planting_month = raster("data_out/planting_month/Irr_crop_10_sub_1_planting_month.nc")
harvest_month  = raster("data_out/harvest_month/Irr_crop_10_sub_1_harvest_month.nc")

test = stackApply(stack(planting_month, harvest_month), indices = c(1,1), fun = calc_season_length) # WORKED!

planting_month = NA
harvest_month = NA
