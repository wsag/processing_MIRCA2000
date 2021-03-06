# processing_MIRCA2000
Processing of MIRCA2000 cropland data for use by WBM.

All data in the data_in folder of this project were downloaded by D. Grogan from the links on this website: https://www.uni-frankfurt.de/45218031/Data_download_center_for_MIRCA2000

FAQs can be found here: https://www.uni-frankfurt.de/45218032/Frequently_Asked_Questions

Publications to cite when using this data are:

1. Portmann, F.T., S. Siebert, and P. Doell (2010), MIRCA2000--Global monthly irrigated and rainfed crop areas around the year 2000: A new high-resolution data set for agricultural and hydrological modeling, Global Biogeochem. Cycles, 24, GB1011, doi:10.1029/2008GB003425.

2. Portmann, F.T. (2011) Global estiamtion of monthly irrigated and rainfed crop areas on a 5 arc-minute grid. Frankfurt Hydrology Paper 09, Institute of Physical Geography, University of Frankfurt, Frankfurt am Main, Germany, http://publikationen.ub.uni-frankfurt.de/frontdoor/index/index/docId/23013.

Code included in the repo, in order of processing:
1. calendar_to_grid()
This script takes the area, planting month, and harvest month for each crop/subcrop combination from data_in/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS.TXT and converts it to raster format.
Output is written in netCDF format to the folders data_out/crop_physical_area_grids, data_out/planting_month, and data_out/harvest_month.
No unit conversions or other processing are done in this step.

