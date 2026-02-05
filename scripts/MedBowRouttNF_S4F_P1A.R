# Medicine Bow-Routt National Forest

# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 1A: creation of Potential Collection Units (PCUs)

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

# (2) create AOI ----

## MedBow ----
# the Area of Interest (AOI) for this case study is the Arapaho-Roosevelt National Forest (MBRNF)
### load & process ----
NF_CONUS_vect <- vect("S_USA.FSCommonNames.shp")
plot(NF_CONUS_vect)

# see unique names 
names(NF_CONUS_vect)
unique(NF_CONUS_vect$COMMONNAME)

# select for just MBRNF 
MBRNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Medicine Bow-Routt National Forest")
plot(MBRNF_vect)

# project 
MBRNF_vect <- project(MBRNF_vect,"EPSG:5070")

# calc area
expanse(MBRNF_vect) # 11220986892 m^2
11220986892/4046.86 # 4046.86 m/acre = 2772764 acres

#### write & read ----
writeVector(MBRNF_vect, "MBRNF_vect.shp")
MBRNF_vect <- vect("MBRNF_vect.shp")

# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
# already in 5070
plot(QMD_CONUS)

### crop and mask ----
MBRNF_QMD_rast <- crop(QMD_CONUS, MBRNF_vect, mask=TRUE)
plot(MBRNF_QMD_rast)

#### write & read ----
writeRaster(MBRNF_QMD_rast, "MBRNF_QMD_rast.tif")
MBRNF_QMD_rast <- rast("MBRNF_QMD_rast.tif")

global(MBRNF_QMD_rast, fun = "notNA") # 5697616 cells

# reclassify with ifel()
MBRNF_QMD_filt_rast <- ifel(
  QMD_MBRNF_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(MBRNF_QMD_filt_rast, col = "darkgreen")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(MBRNF_QMD_filt_rast, "MBRNF_QMD_filt_rast.tif")
MBRNF_QMD_filt_rast <- rast("MBRNF_QMD_filt_rast.tif")



## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # 5070
res(EVH_CONUS) # 30 30

### crop / mask ----
EVH_MBRNF <- crop(EVH_CONUS, MBRNF_vect, mask=TRUE)
plot(EVH_MBRNF)

levels_EVH <- levels(EVH_MBRNF)
is.factor(EVH_MBRNF) # TRUE

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

MBRNF_EVH_rast <- ifel(
  EVH_MBRNF >= 101 & EVH_MBRNF < 200,
  (EVH_MBRNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(MBRNF_EVH_rast)
global(MBRNF_EVH_rast, fun = "notNA") # 5311714
summary(MBRNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(MBRNF_EVH_rast, "MBRNF_EVH_rast.tif")
MBRNF_EVH_rast <- rast("MBRNF_EVH_rast.tif")

### filter ----
MBRNF_EVH_filt_rast <- ifel(
  MBRNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(MBRNF_EVH_filt_rast, col = "forestgreen")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(MBRNF_EVH_filt_rast, "MBRNF_EVH_filt_rast.tif")
MBRNF_EVH_filt_rast <- rast("MBRNF_EVH_filt_rast.tif")



## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec
# these tiles have GEOGCRS NAD83, but are not yet projected

### load & process DEMs ----
DEM_n41_w106 <- rast("USGS_1_n41w106_20230314.tif")
DEM_n41_w107 <- rast("USGS_1_n41w107_20230314.tif")
DEM_n40_w106 <- rast("USGS_1_n40w106_20230602.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")
# mosaic 4 tiles together
MBRNF_DEM <- mosaic(DEM_n41_w106, DEM_n41_w107, DEM_n40_w106, DEM_n40_w107, fun = "first")
# project
MBRNF_DEM <- project(MBRNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of MBRNF 
MBRNF_DEM_rast <- crop(MBRNF_DEM, MBRNF_vect, mask=TRUE)
plot(MBRNF_DEM_rast) # min = 1470.285 , max = 4393.409 (meters)
plot(is.na(MBRNF_DEM_rast))

#### write & read ----
writeRaster(MBRNF_DEM_rast, "MBRNF_DEM_rast.tif")
MBRNF_DEM_rast <- rast("MBRNF_DEM_rast.tif")

### calc slope ----
MBRNF_slope_rast = terrain(MBRNF_DEM_rast, v="slope", unit="degrees")
plot(MBRNF_slope_rast)

#### write & read ----
writeRaster(MBRNF_slope_rast, "MBRNF_slope_rast.tif")
MBRNF_slope_rast <- rast("MBRNF_slope_rast.tif")

### adjust values ----
minmax(MBRNF_slope_rast) 
# min = 0, max = 72.59397 
# but the max we want to include is 24 degrees
# and we want 0-24 degree slope to become 0-1 score (normalize)

# make all values > 24 degrees NA, make all other values 100
MBRNF_slope_filt_rast <- ifel(MBRNF_slope_rast > 24, NA, 100)

### viz ----
plot(MBRNF_slope_filt_rast)
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=0.5)

plot(is.na(MBRNF_slope_filt_rast))

#### write & read ----
writeRaster(MBRNF_slope_filt_rast, "MBRNF_slope_filt_rast.tif")
MBRNF_slope_filt_rast <- rast("MBRNF_slope_filt_rast.tif")



## road ----

# import CO roads shapefile
# downloaded from The National Map
roads_CONUS <- vect("Trans_RoadSegment_0.shp")
plot(roads_CONUS)
crs(roads_CONUS) # EPSG 4269

road_df <- as.data.frame(roads_CONUS)
# could filter by road type, we did not

# project, crop & mask 
roads_CONUS = project(roads_CONUS, "EPSG:5070")
crs(roads_CONUS) # EPSG 5070

roads_MBRNF = crop(roads_CONUS, MBRNF_vect)
plot(roads_MBRNF)

### rasterize ----
MBRNF_road_rast <- rasterize(roads_MBRNF, MBRNF_risk_score_rast , touches=TRUE)
plot(MBRNF_road_rast, col="blue") # all values = 1
plot(is.na(MBRNF_road_rast)) # values not 1 are NA
# TBH, the raster does not look nearly as contiguous as the road lines from the .shp
# but when I open the .tif in Arc, it looks fine 
# I think it is too much for R studio to render with plot()

#### write & read file ----
writeRaster(MBRNF_road_rast, "MBRNF_road_rast.tif")
MBRNF_road_rast <- rast("MBRNF_road_rast.tif") 

### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
MBRNF_road_dist_rast <- distance(MBRNF_road_rast) 
plot(MBRNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(MBRNF_road_dist_rast, "MBRNF_road_dist_rast.tif")
MBRNF_road_dist_rast <- rast("MBRNF_road_dist_rast.tif")

### adjust values ----
minmax(MBRNF_road_dist_rast) 
# min = 0, max = 37416.17 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if < threshold, make value 500

# make NA all values > 917.3261 meters, make others 500
MBRNF_road_filt_rast <- ifel(MBRNF_road_dist_rast > 917.3261, NA, 500)
plot(MBRNF_road_filt_rast)

### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the MBRNF
MBRNF_road_filt_rast = crop(MBRNF_road_filt_rast, MBRNF_vect, mask = TRUE)

### viz ----
plot(MBRNF_road_filt_rast)
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1)
plot(is.na(MBRNF_road_filt_rast))

#### write & read ----
writeRaster(MBRNF_road_filt_rast, "MBRNF_road_filt_rast.tif")
MBRNF_road_filt_rast <- rast("MBRNF_road_filt_rast.tif")



# (4) combine data ----

## resample ----
# first, the rasters need to be resampled so their extents align,
# and they have matching resolutions and origins

# 3 of the 4 rasters have matching resolutions (QMD, EVH, and road)
# 2 of the 4 rasters have matching extents (QMD, EVH)
# 1 of the 4 rasters has no matching resolution or extent (slope)
# I am choosing EVH to use as the template

slope_resampled <- resample(MBRNF_slope_filt_rast, MBRNF_EVH_filt_rast, method = "near")
road_resampled <- resample(MBRNF_road_filt_rast, MBRNF_EVH_filt_rast, method = "near")

raster_list <- list(MBRNF_EVH_filt_rast,
                    MBRNF_QMD_filt_rast,
                    slope_resampled,
                    road_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
MBRNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615

## viz ----
plot(MBRNF_combined_rast)
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)

plot(is.na(MBRNF_combined_rast))


## stats ----
# we want to know what % of the MBRNF each priority factor (PF) & combo occupies
# need a total # cells in the MBRNF to compare
global(MBRNF_DEM_rast, fun = "notNA") # 6282487 cells (covers all MBRNF)
# but not same resolution as rest of data
DEM_resampled <- resample(MBRNF_DEM_rast, MBRNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 7773990 cells (covers all MBRNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(MBRNF_QMD_rast, fun = "notNA") # 5697616 cells
(5697616/7773990)*100 # 73.29076 % of MBRNF has QMD values

# areas with QMD > 5 inches
global(MBRNF_QMD_filt_rast, fun = "notNA") # 4160703
(4160703/7773990)*100 # 53.52082 % of MBRNF has trees > 5 in QMD


#### EVH ----
# all veg area
global(EVH_MBRNF >= 101, fun = "sum", na.rm = TRUE) # 7001131 cells
(7001131/7773990)*100 # 90.0584 % of MBRNF is vegetated 

# all tree area
global(MBRNF_EVH_rast, fun = "notNA") # 5311714
(5311714/7773990)*100 # 68.32674 % of MBRNF has trees 

# trees > 10 ft area
global(MBRNF_EVH_filt_rast, fun = "notNA") # 5231674
(5231674/7773990)*100 # 67.29715 % of MBRNF has trees > 10 ft


#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 6282487 cells 
(6282487/7773990)*100 # 80.81419 % remaining after 24* filter

#### road ----
# need to use resampled version (above) to get same extent
global(road_resampled, fun = "notNA") # 5213776 cells 
# entire MBRNF = 7773990 cells 
(5213776/7773990)*100 # 67.06692 % remaining 


## combined PFs ----
# we want to know what % of the MBRNF each category falls into after combining

# value 5, QMD only
global(MBRNF_combined_rast == 5, fun = "sum", na.rm = TRUE) # 25120 cells
(25120/7773990)*100 # 0.3231288 % of MBRNF

# value 10, EVH only
global(MBRNF_combined_rast == 10, fun = "sum", na.rm = TRUE) # 90042 cells
(90042/7773990)*100 # 1.158247 % of MBRNF

# value 15, QMD + EVH
global(MBRNF_combined_rast == 15, fun = "sum", na.rm = TRUE) # 188102 cells
(188102/7773990)*100 # 2.419633 % of MBRNF

# value 100, slope only
global(MBRNF_combined_rast == 100, fun = "sum", na.rm = TRUE) # 601784 cells
(601784/7773990)*100 # 7.740993 % of MBRNF

# value 105, slope + QMD
global(MBRNF_combined_rast == 105, fun = "sum", na.rm = TRUE) # 89504 cells
(89504/7773990)*100 # 1.151326 % of MBRNF

# value 110, slope + EVH
global(MBRNF_combined_rast == 110, fun = "sum", na.rm = TRUE) # 411384 cells
(411384/7773990)*100 # 5.2918 % of MBRNF

# value 115, slope + EVH + QMD
global(MBRNF_combined_rast == 115, fun = "sum", na.rm = TRUE) # 835573 cells
(835573/7773990)*100 # 10.74832 % of MBRNF

# value 500, road only
global(MBRNF_combined_rast == 500, fun = "sum", na.rm = TRUE) # 211065 cells
(211065/7773990)*100 # 2.715015 % of MBRNF

# value 505, road + QMD
global(MBRNF_combined_rast == 505, fun = "sum", na.rm = TRUE) # 51904 cells
(51904/7773990)*100 # 0.6676623 % of MBRNF

# value 510, road + EVH
global(MBRNF_combined_rast == 510, fun = "sum", na.rm = TRUE) # 165873 cells
(165873/7773990)*100 # 2.133692 % of MBRNF

# value 515, road + EVH + QMD
global(MBRNF_combined_rast == 515, fun = "sum", na.rm = TRUE) # 440692 cells
(440692/7773990)*100 # 5.668801 % of MBRNF

# value 600, road + slope
global(MBRNF_combined_rast == 600, fun = "sum", na.rm = TRUE) # 990481 cells
(990481/7773990)*100 # 12.74096 % of MBRNF

# value 605, road + slope + QMD
global(MBRNF_combined_rast == 605, fun = "sum", na.rm = TRUE) # 253753 cells
(253753/7773990)*100 # 3.264128 % of MBRNF

# value 610, road + slope + EVH
global(MBRNF_combined_rast == 610, fun = "sum", na.rm = TRUE) # 823953 cells
(823953/7773990)*100 # 10.59884 % of MBRNF

# value 615, road + slope + QMD + EVH
global(MBRNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2276055 cells
(2276055/7773990)*100 # 29.27782 % of MBRNF

# value notNA
global(MBRNF_combined_rast, fun = "notNA") # 7455285 cells
(7455285/7773990)*100 # 95.90037 % of MBRNF (equals the sum of above %s)
100-95.90037 # 4.09963 % is NA (QMD < 5in, EVH < 10ft, slope >24, road >0.57)



## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
MBRNF_priority_rast <- ifel(
  MBRNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(MBRNF_priority_rast, fun = "notNA") # 2276055 cells (same as value=615 above)
(2276055/7773990)*100 # 29.27782 % of MBRNF

## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(MBRNF_priority_rast, transform = FALSE) # 2048449500 m^2
2048449500/4046.86 # 4046.86 m2/acre = 506182.4 acres
# entire MBRNF = 1723619 acres (calculated from MBRNF_vect polygon in Part1A_2)
(506182.4/1723619)*100 # 29.36742 % of MBRNF (same as value=615 above)

## viz ----
plot(MBRNF_priority_rast, col = "darkgreen")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(MBRNF_priority_rast, "MBRNF_priority_rast.tif")
MBRNF_priority_rast <- rast("MBRNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took 20 minutes to run

priority_patches_all <- patches(MBRNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 93608 patches

## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 93608 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 

## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filt out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1414 geoms remain
(1414/134187)*100 # 1.053753 % of polys remain (are >= 20 acres)
# so ~99 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1183 geoms
(1183/1414)*100 # 83.66337 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 231 geoms
# these do need to be divided

## divide ----
# calculate divisions needed for each large poly, ensuring at least 2 parts for large polys
num_all_parts <- pmax(2, round(large_polys$patch_acres / 125))

# use lapply to iterate and divide
divided_polys_list <- lapply(1:nrow(large_polys), function(i) {
  poly <- large_polys[i, ]
  # set a seed to ensure reproducibility for the division process
  set.seed(i)
  # divide by the pre-determined number of parts for that particular poly
  divided_poly <- divide(poly, n = num_all_parts[i])
  # store the original patch_ID and re-calculate the new areas
  divided_poly$patch_ID <- poly$patch_ID
  divided_poly$div_acres <- expanse(divided_poly) * 0.000247105
  
  return(divided_poly)
})

# combine all divided polys into a single SpatVector
divided_polys_vect <- do.call(rbind, divided_polys_list)
# 2847 geoms

# combine the mid-sized polys with the newly divided large polys
MBRNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4030 geoms

## adjust ----

# add new ID col & new final area col
MBRNF_PCUs_1A_vect$PCU_ID <- 1:nrow(MBRNF_PCUs_1A_vect)
MBRNF_PCUs_1A_vect$area_acres <- expanse(MBRNF_PCUs_1A_vect) * 0.000247105

summary(MBRNF_PCUs_1A_vect)
# area_acres min = 16.78, max = 352.67  
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
MBRNF_PCUs_1A_vect <- MBRNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

MBRNF_PCUs_1A_df <- as.data.frame(MBRNF_PCUs_1A_vect)

sum(MBRNF_PCUs_1A_vect$area_acres) # 422214.1 acres
sum(small_polys_removed$patch_acres) # 422214.1 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# MBRNF is 1723619 acres 
(422214.1/1723619)*100 # 24.49579 % of MBRNF are highest priority areas (PCUs)

# for reference, 
(506182.4/1723619)*100 # 29.36742 % of MBRNF meets PFs (MBRNF_priority_rast values = 1)

(422214.1/506182.4)*100 # 83.41145 % of the areas that meet basic priorities
# are continuous PCUs > 20 acres


## viz ----
plot(MBRNF_PCUs_1A_vect)
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(MBRNF_PCUs_1A_vect, "MBRNF_PCUs_1A_vect.shp")
MBRNF_PCUs_1A_vect <- vect("MBRNF_PCUs_1A_vect.shp")


