# Arapaho and Roosevelt National Forests

# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 1A: creation of Potential Collection Units (PCUs)

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)


# (2) create AOI ----
# load & process
NF_CONUS_vect <- vect("S_USA.FSCommonNames.shp")
crs(NF_CONUS_vect) # EPSG: 4269

# see unique names 
names(NF_CONUS_vect)
unique(NF_CONUS_vect$COMMONNAME)

# select for just ARNF 
ARNF <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Arapaho and Roosevelt National Forests")
plot(ARNF)

# project 
ARNF_vect <- project(ARNF_vect,"EPSG:5070")

# calc area
expanse(ARNF_vect) # 6975245280 m^2
6975245280/4046.86 # 4046.86 m/acre = 1723619 acres

## write & read ----
writeVector(ARNF_vect, "ARNF_vect.shp")
ARNF_vect <- vect("ARNF_vect.shp")



## remove?
## state ----
states_all <- vect("tl_2025_us_state.shp")
crs(states_all) # EPSG: 4269

# get all 4 states
states_SRME <- states_all %>% 
  filter(STUSPS %in% c("CO", "WY", "UT", "NM"))
plot(states_SRME)

states_SRME_vect <- project(states_SRME, "EPSG:5070")

states_SRME_vect <- aggregate(states_SRME_vect, dissolve = TRUE)

# get just CO
states_CO <- states_all %>% 
  filter(STUSPS %in% c("CO"))
plot(states_CO)

states_CO_vect <- project(states_CO, "EPSG:5070")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
ARNF_QMD_rast <- crop(QMD_CONUS, ARNF_vect, mask=TRUE)
plot(ARNF_QMD_rast)

#### write & read ----
writeRaster(ARNF_QMD_rast, "ARNF_QMD_rast.tif")
ARNF_QMD_rast <- rast("ARNF_QMD_rast.tif")

global(ARNF_QMD_rast, fun = "notNA") # 5697616 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

ARNF_QMD_filt_rast <- ifel(
  ARNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(ARNF_QMD_filt_rast, col = "darkgreen")
polys(ARNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(ARNF_QMD_filt_rast, "ARNF_QMD_filt_rast.tif")
ARNF_QMD_filt_rast <- rast("ARNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_ARNF <- crop(EVH_CONUS, ARNF_vect, mask=TRUE)
plot(EVH_ARNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

ARNF_EVH_rast <- ifel(
  EVH_ARNF >= 101 & EVH_ARNF < 200,
  (EVH_ARNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(ARNF_EVH_rast)
global(ARNF_EVH_rast, fun = "notNA") # 5311714
summary(ARNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(ARNF_EVH_rast, "ARNF_EVH_rast.tif")
ARNF_EVH_rast <- rast("ARNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

ARNF_EVH_filt_rast <- ifel(
  ARNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(ARNF_EVH_filt_rast, col = "forestgreen")
polys(ARNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(ARNF_EVH_filt_rast, "ARNF_EVH_filt_rast.tif")
ARNF_EVH_filt_rast <- rast("ARNF_EVH_filt_rast.tif")



## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n41_w106 <- rast("USGS_1_n41w106_20230314.tif")
DEM_n41_w107 <- rast("USGS_1_n41w107_20230314.tif")
DEM_n40_w106 <- rast("USGS_1_n40w106_20230602.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")

# mosaic 4 tiles together
ARNF_DEM <- mosaic(DEM_n41_w106, DEM_n41_w107, DEM_n40_w106, DEM_n40_w107, fun = "first")

crs(ARNF_DEM) # EPSG: 4269
res(ARNF_DEM) # 0.0002777778

# project
ARNF_DEM <- project(ARNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of ARNF 
ARNF_DEM_rast <- crop(ARNF_DEM, ARNF_vect, mask=TRUE)
plot(ARNF_DEM_rast) # min = 1514.981 , max = 4343.173  (meters)
plot(is.na(ARNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(ARNF_DEM_rast, "ARNF_DEM_rast.tif")
ARNF_DEM_rast <- rast("ARNF_DEM_rast.tif")

### calc slope ----
ARNF_slope_rast = terrain(ARNF_DEM_rast, v="slope", unit="degrees")
plot(ARNF_slope_rast)

#### write & read ----
writeRaster(ARNF_slope_rast, "ARNF_slope_rast.tif")
ARNF_slope_rast <- rast("ARNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

ARNF_slope_filt_rast <- ifel(ARNF_slope_rast > 24, NA, 100)

### viz ----
plot(ARNF_slope_filt_rast, col = "mediumorchid2")
polys(ARNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(ARNF_slope_filt_rast, "ARNF_slope_filt_rast.tif")
ARNF_slope_filt_rast <- rast("ARNF_slope_filt_rast.tif")



## road ----
### load & process ----
#### USFS roads ----
# S_USA.Trans_RoadCore_FS.shp
# downloaded from the FS Geodata Clearinghouse
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USFS_roads_SRME <- vect("FS_road_SRME_Clip.shp")
crs(USFS_roads_SRME) # EPSG: 4269
nrow(USFS_roads_SRME) # 26985

# project
USFS_roads_SRME_projected <- project(USFS_roads_SRME, "EPSG:5070")

# get just roads in the ARNF
USFS_roads_ARNF <- terra::intersect(USFS_roads_SRME_projected, ARNF_vect)
nrow(USFS_roads_ARNF) # 2825

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_ARNF)
unique(USFS_roads_ARNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_ARNF <- USFS_roads_ARNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
    ))

nrow(USFS_roads_ARNF) # 2399
plot(USFS_roads_ARNF)
(2399/2825)* 100 # = 84.92035 % of FS roads retained
100 - 84.92035 # = 15.07965 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the ARNF
USGS_roads_ARNF <- terra::intersect(USGS_roads_SRME_proj, ARNF_vect)
nrow(USGS_roads_ARNF) # 19423
plot(USGS_roads_ARNF)


### rasterize ----
#### USFS ----
ARNF_USFS_road_rast <- rasterize(USFS_roads_ARNF, ARNF_QMD_rast , touches=TRUE)
plot(ARNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(ARNF_USFS_road_rast)) # values not 1 are NA
global(ARNF_USFS_road_rast, fun = "notNA") # 120022 cells not NA

#### USGS ----
ARNF_USGS_road_rast <- rasterize(USGS_roads_ARNF, ARNF_QMD_rast , touches=TRUE)
plot(ARNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(ARNF_USGS_road_rast)) # values not 1 are NA
global(ARNF_USGS_road_rast, fun = "notNA") # 282401 cells not NA


### combine ----
ARNF_road_rast <- cover(ARNF_USFS_road_rast, ARNF_USGS_road_rast)
plot(ARNF_road_rast)
plot(is.na(ARNF_road_rast))
global(ARNF_road_rast, fun = "notNA") # 295373 cells not NA

##### write & read ----
writeRaster(ARNF_road_rast, "ARNF_road_rast.tif")
ARNF_road_rast <- rast("ARNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
ARNF_road_dist_rast <- distance(ARNF_road_rast) 
plot(ARNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(ARNF_road_dist_rast, "ARNF_road_dist_rast.tif")
ARNF_road_dist_rast <- rast("ARNF_road_dist_rast.tif")


### filter ----
minmax(ARNF_road_dist_rast) 
# min = 0, max = 37416.17 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
ARNF_road_filt_rast <- ifel(ARNF_road_dist_rast > 917.3261, NA, 500)
plot(ARNF_road_filt_rast)

### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the ARNF
ARNF_road_filt_rast = crop(ARNF_road_filt_rast, ARNF_vect, mask = TRUE)

### viz ----
plot(ARNF_road_filt_rast)
polys(ARNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(ARNF_road_filt_rast, "ARNF_road_filt_rast.tif")
ARNF_road_filt_rast <- rast("ARNF_road_filt_rast.tif")



# (4) combine data ----

## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(ARNF_slope_filt_rast, ARNF_EVH_filt_rast, method = "near")

raster_list <- list(ARNF_EVH_filt_rast,
                    ARNF_QMD_filt_rast,
                    ARNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
ARNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615

## viz ----
plot(ARNF_combined_rast)
polys(ARNF_vect, col = "black", alpha=0.01, lwd=1.5)

plot(is.na(ARNF_combined_rast))


## stats ----
# we want to know what % of the ARNF each priority factor (PF) & combo occupies
# need a total # cells in the ARNF to compare
global(ARNF_DEM_rast, fun = "notNA") # 9199894 cells (covers all ARNF)
# but not same resolution as rest of data
DEM_resampled <- resample(ARNF_DEM_rast, ARNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 7773990 cells (covers all ARNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(ARNF_QMD_rast, fun = "notNA") # 5697616 cells
(5697616/7773990)*100 # 73.29076 % of ARNF has QMD values

# areas with QMD > 5 inches
global(ARNF_QMD_filt_rast, fun = "notNA") # 4160703
(4160703/7773990)*100 # 53.52082 % of ARNF has trees > 5 in QMD


#### EVH ----
# all veg area
global(EVH_ARNF >= 101, fun = "sum", na.rm = TRUE) # 7001131 cells
(7001131/7773990)*100 # 90.0584 % of ARNF is vegetated 

# all tree area
global(ARNF_EVH_rast, fun = "notNA") # 5311714
(5311714/7773990)*100 # 68.32674 % of ARNF has trees 

# trees > 10 ft area
global(ARNF_EVH_filt_rast, fun = "notNA") # 5231674
(5231674/7773990)*100 # 67.29715 % of ARNF has trees > 10 ft


#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 6282487 cells 
(6282487/7773990)*100 # 80.81419 % remaining after 24* filter

#### road ----
global(ARNF_road_filt_rast, fun = "notNA") # 5316596 cells
# entire ARNF = 7773990 cells
(5316596/7773990)*100 # 68.38954 % remaining


## combined PFs ----
# we want to know what % of the ARNF each category falls into after combining

# value 5, QMD only
global(ARNF_combined_rast == 5, fun = "sum", na.rm = TRUE) # 24212 cells
(24212/7773990)*100 # 0.3114488 % of ARNF

# value 10, EVH only
global(ARNF_combined_rast == 10, fun = "sum", na.rm = TRUE) # 86352 cells
(86352/7773990)*100 # 1.110781 % of ARNF

# value 15, QMD + EVH
global(ARNF_combined_rast == 15, fun = "sum", na.rm = TRUE) # 176429 cells
(176429/7773990)*100 # 2.269478 % of ARNF

# value 100, slope only
global(ARNF_combined_rast == 100, fun = "sum", na.rm = TRUE) # 589281 cells
(589281/7773990)*100 # 7.580162 % of ARNF

# value 105, slope + QMD
global(ARNF_combined_rast == 105, fun = "sum", na.rm = TRUE) # 84525 cells
(84525/7773990)*100 # 1.08728 % of ARNF

# value 110, slope + EVH
global(ARNF_combined_rast == 110, fun = "sum", na.rm = TRUE) # 391693 cells
(391693/7773990)*100 # 5.038507 % of ARNF

# value 115, slope + EVH + QMD
global(ARNF_combined_rast == 115, fun = "sum", na.rm = TRUE) # 790573 cells
(790573/7773990)*100 # 10.16946 % of ARNF

# value 500, road only
global(ARNF_combined_rast == 500, fun = "sum", na.rm = TRUE) # 215441 cells
(215441/7773990)*100 # 2.771305 % of ARNF

# value 505, road + QMD
global(ARNF_combined_rast == 505, fun = "sum", na.rm = TRUE) # 52812 cells
(52812/7773990)*100 # 0.6793423 % of ARNF

# value 510, road + EVH
global(ARNF_combined_rast == 510, fun = "sum", na.rm = TRUE) # 169563 cells
(169563/7773990)*100 # 2.181158 % of ARNF

# value 515, road + EVH + QMD
global(ARNF_combined_rast == 515, fun = "sum", na.rm = TRUE) # 452365 cells
(452365/7773990)*100 # 5.818955 % of ARNF

# value 600, road + slope
global(ARNF_combined_rast == 600, fun = "sum", na.rm = TRUE) # 1002984 cells
(1002984/7773990)*100 # 12.90179 % of ARNF

# value 605, road + slope + QMD
global(ARNF_combined_rast == 605, fun = "sum", na.rm = TRUE) # 258732 cells
(258732/7773990)*100 # 3.328175 % of ARNF

# value 610, road + slope + EVH
global(ARNF_combined_rast == 610, fun = "sum", na.rm = TRUE) # 843644 cells
(843644/7773990)*100 # 10.85214 % of ARNF

# value 615, road + slope + QMD + EVH
global(ARNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2321055 cells
(2321055/7773990)*100 # 29.85668 % of ARNF

# value notNA
global(ARNF_combined_rast, fun = "notNA") # 7459661 cells
(7459661/7773990)*100 # 95.95666 % of ARNF (equals the sum of above %s)
100-95.95666 # 4.04334 % is NA (QMD < 5in, EVH < 10ft, slope >24, road >0.57)



## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
ARNF_priority_rast <- ifel(
  ARNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(ARNF_priority_rast, fun = "notNA") # 2321055 cells (same as value=615 above)
(2321055/7773990)*100 # 29.85668 % of ARNF

## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(ARNF_priority_rast, transform = FALSE) # 2088949500 m^2
2088949500/4046.86 # 4046.86 m2/acre = 516190.2 acres
# entire ARNF = 1723619 acres (calculated from ARNF_vect polygon in Part1A_2)
(516190.2/1723619)*100 # 29.94805 % of ARNF (same as value=615 above)

## viz ----
plot(ARNF_priority_rast, col = "darkgreen")
polys(ARNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(ARNF_priority_rast, "ARNF_priority_rast.tif")
ARNF_priority_rast <- rast("ARNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took 20 minutes to run

priority_patches_all <- patches(ARNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
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
ARNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4030 geoms

## adjust ----

# add new ID col & new final area col
ARNF_PCUs_1A_vect$PCU_ID <- 1:nrow(ARNF_PCUs_1A_vect)
ARNF_PCUs_1A_vect$area_acres <- expanse(ARNF_PCUs_1A_vect) * 0.000247105

summary(ARNF_PCUs_1A_vect)
# area_acres min = 16.78, max = 352.67  
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
ARNF_PCUs_1A_vect <- ARNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

ARNF_PCUs_1A_df <- as.data.frame(ARNF_PCUs_1A_vect)

sum(ARNF_PCUs_1A_vect$area_acres) # 422214.1 acres
sum(small_polys_removed$patch_acres) # 422214.1 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# ARNF is 1723619 acres 
(422214.1/1723619)*100 # 24.49579 % of ARNF are highest priority areas (PCUs)

# for reference, 
(506182.4/1723619)*100 # 29.36742 % of ARNF meets PFs (ARNF_priority_rast values = 1)

(422214.1/506182.4)*100 # 83.41145 % of the areas that meet basic priorities
# are continuous PCUs > 20 acres


## viz ----
plot(ARNF_PCUs_1A_vect)
polys(ARNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(ARNF_PCUs_1A_vect, "ARNF_PCUs_1A_vect.shp")
ARNF_PCUs_1A_vect <- vect("ARNF_PCUs_1A_vect.shp")


