# Main()

#############################################################################################################################
### LIBRARIES AND SOURCE CODE ###

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

# source functions within this project

#############################################################################################################################
### LOAD DATA ###

crop_cal.full = data.frame(read.delim("data_in/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS.TXT", header = T))
crop_codes = read.delim("data_in/crop_codes.txt", skip = 3, header = T)
cell_grid = raster("data_in/cell_area_grid/cell_area_ha_05mn.asc")

# make a table of all possible crop/subcrop combos
crop_combos = subset(crop_cal.full, select = c("crop", "subcrop"))
crop_combos.u = unique(crop_combos)

#############################################################################################################################
### CONVERT CALENDAR-FORMATTED DATA TO GRIDS ###

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

### 2. Rasterize planting month (output: grid with month values from 1 to 12)
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

### 3. Rasterize harvest month (output: grid with month values from 1 to 12)
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
       
#############################################################################################################################
### CONVERT MONTH VALUES TO ORDINAL DATE VALUES FOR PLANTING AND HARVESTING DATES ###








