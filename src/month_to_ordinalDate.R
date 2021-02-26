# month_to_ordinalDate()

############################################################################################################################# 
# Description:
# Convert the planting or harvesting month value (1-12) to a ordinal date day value (1-365)
# with option to add random +/- 15 days to date to distribute across the month

# Project: processing_MIRCA2000
# author: Danielle S Grogan
# last update: 2021-02-17
############################################################################################################################# 
### LIBRARIES AND SOURCE CODE ###

library(lubridate)

#############################################################################################################################

month_to_ordinalDate = function(month.val,      # numeric: the month number (1 through 12)
                                random.day = 0  # binary: if 1,  +/- random value from -15 to +15
){
  make.date = paste("2000", month.val, "15", sep="-")  # create a date string, setting the day to the 15th of the month
  
  if(random.day == 1){  # choose a random number from -15 to 15
    add.rand = sample(-15:15, 1)
  }else{
    add.rand = 0
  }
  ordinal.date = yday(make.date) + add.rand
  ordinal.date
}
#############################################################################################################################


