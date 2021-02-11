# calendar_to_grid()

############################################################################################################################# 
# Description:
# Convert the MIRCA2000 CELL_SPECIFIC_CROPPING_CALENDAR text files (5 min or 30 min) into 12-month raster bricks
# output bricks (one per crop_subcrop) contain one layer per month
# grid cell values are the physical crop area, in ha

# also outputs gridded planting day and season length files

# Project: processing_MIRCA2000
# author: Danielle S Grogan
# last update: 2021-02-11
############################################################################################################################# 

# ------ DATA FORMAT of the CELL_SPECIFIC_CROPPING_CALENDAR:
# Files list growing periods of irrigated and rainfed crops and their sub-crops including the related growing areas 
# (in hectares) for each 5 arc-minute and 30 arc-minute grid cell with harvested area. Grid cells and crops 
# without areas are not listed.
# 
# The files have a header of one row. The meaning of the columns is as follows:
#   1.) cell_ID = cell-ID (counting from NW to SE beginning with 1 => in 5-minute resolution 4320 X 2160 cells)
#   2.) row = number of row (counting from N to S beginning with 1 => in 5-minute resolution 2160 rows)
#   3.) column = number of column (counting from W to E beginning with 1 => in 5-minute resolution 4320 columns)
#   4.) lat = latitude of cell center (in decimal degrees)
#   5.) lon = longitude of cell center (in decimal degrees)
#   6.) crop = crop class (1-52)
#   7.) subcrop = sub-crop number (in 5 arc-minute resolution up to 5, in 30 arc-minute resolution up to 9 sub-crops per cell)
#   8.) area = sub-crop growing area (in hectares)
#   9.) start = start of cropping period (month of the year, number between 1 (January) and 12 (December))
#   10.) end = end of cropping period (month of the year, number between 1 (January) and 12 (December)).

#############################################################################################################################
### LIBRARIES AND SOURCE CODE ###

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

#############################################################################################################################
### test
crop_cal = data.frame(read.delim("data_in/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS.TXT", header = T))
cell_grid = raster("data_in/cell_area_grid/cell_area_ha_05mn.asc")
#############################################################################################################################
### MAIN calendar_to_grid() ###
calendar_to_grid = function(crop_cal,      # crop_cal = data frame, loaded from the MIRCA2000 CELL_SPECIFIC_CROPPING_CALDENAR (either 5 min or 30 min)
                            cell_grid,     # raster grid matching the spatial resolution of the crop_cal
                            check.file = 0 # binary: if 1, then check if output file exists. If it exists, do not overwrite
                            ){
  
  # list all possible grid cell values
  all_cell_ids = data.frame(seq(1, ncell(cell_grid)))
  colnames(all_cell_ids)[1] = "Cell_ID"
  
  # loop through crops
  for(crp in 1:52){
    
    # subset crop_cal to data with crp
    crop_cal.sub = subset(crop_cal, crop_cal$crop == crp)
    
    # calculate number of subcrops
    n.subcrp = length(unique(crop_cal.sub$subcrop))
    
    # loop through subcrops
    if(n.subcrp > 1){
      for(s in 1:n.subcrp)
        crop_cal.sub = subset(crop_cal.sub, crop_cal.sub$subcrop == s)
    }
    
  
    # merge with all possible cell IDs
    crop_cal.sub.merge = merge(all_cell_ids, crop_cal.sub, by = "Cell_ID", all=T)
    
    test = subset(crop_cal.sub.merge, crop_cal.sub.merge$)
    
    
    
    # subset to area data only
    crop_cal.sub_area = crop_cal.sub.merge[, c('lat', 'long', 'area')]  # subset to only the lat, long, and area data
    
    # rasterize
    empty_grid = cell_grid
    values(empty_grid) = NA
    values(empty_grid) = crop_cal.sub.merge$area
    
    test = rasterFromXYZ(crop_cal.sub_area, res=res(cell_grid))
    
    coordinates(crop_cal.sub_area) = ~ long + lat                 # set coordinates
    gridded(crop_cal.sub_area) = TRUE                             # make the data frame a SpatialPixelsDataFrame (gridded)
    
    
    new_grid = cover(empty_grid, crop_cal.sub_area)
    
  }
}   




# example 2: list of lat, long, data
coordinates(ld) <- ~ lons + lats                                  # identify the coordinates
gridded(ld) <- TRUE                                               # make the data frame a SpatialPixelsDataFrame (gridded)
raster.ld <- raster(ld)                                           # coerce to raster 
projection(raster.ld) <- "+proj=utm +zone=48 +datum=WGS84"        # set the spatial projection (this is the default projection)
values(raster.ld)<-c(ld$irrExtra)                                 # set raster grid cell values


# values(raster) = data: this fills in all grid cells in order from northwest to southeast 



