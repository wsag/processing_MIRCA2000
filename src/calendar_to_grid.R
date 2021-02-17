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
### calendar_to_grid() ###
calendar_to_grid = function(crp,           # number from 1 to 52 (crop category)
                            subcrp,        # number, from 1 5 (subcrop category, must have a matching crp category)
                            crop_cal,      # crop_cal = data frame, loaded from the MIRCA2000 CELL_SPECIFIC_CROPPING_CALDENAR (either 5 min or 30 min),
                                           # ONLY including columns: Cell_ID, crop, subcrop, and the data to be rasterized (e.g., area)
                                           # DATA TO BE RASTERIZED MUST BE IN THE 4TH COLUMN
                            cell_grid,     # raster grid matching the spatial resolution of the crop_cal
                            crop_codes,    # file crop_codes.txt, listing the crop name (e.g., Wheat) that goes with each crop category number
                            varname,       # character string: variable name for netcdf file
                            varunit,       # character string: variable unit for writing to netcdf file (e.g., ha)
                            out.dir,       # character string: directory to write output file
                            check.file = 0 # binary: if 1, then check if output file exists. If it exists, do not overwrite
                            ){
  
  ### If check.file = 1, then first check if file exists before processing
  
  # identify crop type as Irr or Rfd for use in file name
  if(crp < 27){ 
    crop.type = "Irr"
  }else{ 
    crop.type = "Rfd"
  }
  file.nm = paste(out.dir, crop.type, "_crop_", crp, "_sub_", subcrp, "_", varname, ".nc", sep="")
  
  if(check.file == 1 & exists(file.nm)){
    print(paste(file.nm, "already exists. No new file written"))
    
  }else{
    # list all possible grid cell values
    all_cell_ids = data.frame(seq(1, ncell(cell_grid)))
    colnames(all_cell_ids)[1] = "Cell_ID"
    
    
    # subset crop_cal to data with crp and subcrp
    crop_cal.sub = subset(crop_cal, crop_cal$crop == crp)
    crop_cal.sub = subset(crop_cal.sub, crop_cal.sub$subcrop == subcrp)
    
    # merge with all possible cell IDs
    crop_cal.sub.merge = merge(all_cell_ids, crop_cal.sub, by = "Cell_ID", all=T)
    
    # test that crop_cal.sub.merge has the correct number of rows
    check.rows = nrow(crop_cal.sub.merge) == nrow(all_cell_ids)
    if(check.rows == T){
      
      # rasterize
      out_grid = cell_grid
      values(out_grid) = NA
      values(out_grid) = crop_cal.sub.merge[,4]
      projection(out_grid) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  # set the spatial projection 
      
      # identify crop name (e.g., Wheat) that goes with the crop code, include in long name
      crp.name = as.character(crop_codes[(which(crop_codes[,1:2] == crp) %% 26), 3])
      
      ### save raster file
      writeRaster(out_grid, 
                  file.nm,
                  format    ="CDF", 
                  varname   = varname, 
                  longname  = paste("MIRCA2000", varname, "for crop ", crp, "(", crp.name, ") subcrop", subcrp), 
                  varunit   = varunit, 
                  overwrite = T)
      
      print(paste(file.nm, "written"))
      
    }else{
      # if the merge did not result in the correct number of values
      print(paste("merge error in crop", crp, "subcrop", s))
      
    } # end if/else loop on check.rows
  } # end if/else loop on check.file
} # end function


#############################################################################################################################
### MAIN ###
#############################################################################################################################
# load data
crop_cal.full = data.frame(read.delim("data_in/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS.TXT", header = T))
crop_codes = read.delim("data_in/crop_codes.txt", skip = 3, header = T)
cell_grid = raster("data_in/cell_area_grid/cell_area_ha_05mn.asc")

# make a table of all possible crop/subcrop combos
crop_combos = subset(crop_cal.full, select = c("crop", "subcrop"))
crop_combos.u = unique(crop_combos)

### 1. Rasterize cropland area (ha)
# subset crop_cal.full to include only required data (area must be 4th column)
crop_cal.area = subset(crop_cal.full, select = c("Cell_ID", "crop", "subcrop", "area"))

# use mapply to loop through all crp and subcrp combos, applying calendar_to_grid() to each
mapply(calendar_to_grid,
       crp      = crop_combos.u$crop,
       subcrp   = crop_combos.u$subcrop,
       MoreArgs = list(crop_cal   = crop_cal.area,
                       cell_grid  = cell_grid,
                       crop_codes = crop_codes,
                       varname    = "cropland_area",
                       varunit    = "ha",
                       out.dir    = "data_out/crop_physical_area_grids/")
       
)


### 2. Rasterize planting month
# subset crop_cal.full to include only required data (start must be 4th column)
crop_cal.start = subset(crop_cal.full, select = c("Cell_ID", "crop", "subcrop", "start"))

# use mapply to loop through all crp and subcrp combos, applying calendar_to_grid() to each
mapply(calendar_to_grid,
       crp      = crop_combos.u$crop,
       subcrp   = crop_combos.u$subcrop,
       MoreArgs = list(crop_cal   = crop_cal.start,
                       cell_grid  = cell_grid,
                       crop_codes = crop_codes,
                       varname    = "planting_month",
                       varunit    = "month",
                       out.dir    = "data_out/planting_month/")
       
)


### 3. Rasterize harvest month
# subset crop_cal.full to include only required data (end must be 4th column)
crop_cal.end = subset(crop_cal.full, select = c("Cell_ID", "crop", "subcrop", "end"))

# use mapply to loop through all crp and subcrp combos, applying calendar_to_grid() to each
mapply(calendar_to_grid,
       crp      = crop_combos.u$crop,
       subcrp   = crop_combos.u$subcrop,
       MoreArgs = list(crop_cal   = crop_cal.end,
                       cell_grid  = cell_grid,
                       crop_codes = crop_codes,
                       varname    = "harvest_month",
                       varunit    = "month",
                       out.dir    = "data_out/harvest_month/")
       
)
