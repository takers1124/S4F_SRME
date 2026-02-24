# Medicine Bow-Routt National Forest

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

# select for just MBRNF 
MBRNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Medicine Bow-Routt National Forest")
plot(MBRNF_vect)

# project 
MBRNF_vect <- project(MBRNF_vect,"EPSG:5070")

# calc area
expanse(MBRNF_vect) # 11220986892 m^2
11220986892/4046.86 # 4046.86 m/acre = 2772764 acres

## write & read ----
writeVector(MBRNF_vect, "MBRNF_vect.shp")
MBRNF_vect <- vect("MBRNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
MBRNF_QMD_rast <- crop(QMD_CONUS, MBRNF_vect, mask=TRUE)
plot(MBRNF_QMD_rast)

#### write & read ----
writeRaster(MBRNF_QMD_rast, "MBRNF_QMD_rast.tif")
MBRNF_QMD_rast <- rast("MBRNF_QMD_rast.tif")

global(MBRNF_QMD_rast, fun = "notNA") # 9678364 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

MBRNF_QMD_filt_rast <- ifel(
  MBRNF_QMD_rast >= 5, 5, NA 
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
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_MBRNF <- crop(EVH_CONUS, MBRNF_vect, mask=TRUE)
plot(EVH_MBRNF)

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
global(MBRNF_EVH_rast, fun = "notNA") # 9148382
summary(MBRNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(MBRNF_EVH_rast, "MBRNF_EVH_rast.tif")
MBRNF_EVH_rast <- rast("MBRNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

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

### load & process DEMs ----
DEM_n40_w108 <- rast("USGS_1_n40w108_20211208.tif")
DEM_n41_w106 <- rast("USGS_1_n41w106_20230314.tif")
DEM_n41_w107 <- rast("USGS_1_n41w107_20230314.tif")
DEM_n41_w108 <- rast("USGS_1_n41w108_20230314.tif")
DEM_n42_w106 <- rast("USGS_1_n42w106_20230314.tif")
DEM_n42_w107 <- rast("USGS_1_n42w107_20230314.tif")
DEM_n42_w108 <- rast("USGS_1_n42w108_20230314.tif")
DEM_n43_w106 <- rast("USGS_1_n43w106_20240325.tif")
DEM_n43_w107 <- rast("USGS_1_n43w107_20240325.tif")

# mosaic 4 tiles together
MBRNF_DEM <- mosaic(DEM_n40_w108,
                    DEM_n41_w106, DEM_n41_w107, DEM_n41_w108, 
                    DEM_n42_w106, DEM_n42_w107, DEM_n42_w108,
                    DEM_n43_w106, DEM_n43_w107,
                    fun = "first")

crs(MBRNF_DEM) # EPSG: 4269
res(MBRNF_DEM) # 0.0002777778

# project
MBRNF_DEM <- project(MBRNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of MBRNF 
MBRNF_DEM_rast <- crop(MBRNF_DEM, MBRNF_vect, mask=TRUE)
plot(MBRNF_DEM_rast) # min = 1527.020, max = 3945.113 (meters)
plot(is.na(MBRNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(MBRNF_DEM_rast, "MBRNF_DEM_rast.tif")
MBRNF_DEM_rast <- rast("MBRNF_DEM_rast.tif")

### calc slope ----
MBRNF_slope_rast = terrain(MBRNF_DEM_rast, v="slope", unit="degrees")
plot(MBRNF_slope_rast)

#### write & read ----
writeRaster(MBRNF_slope_rast, "MBRNF_slope_rast.tif")
MBRNF_slope_rast <- rast("MBRNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

MBRNF_slope_filt_rast <- ifel(MBRNF_slope_rast > 24, NA, 100)

### viz ----
plot(MBRNF_slope_filt_rast, col = "mediumorchid2")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(MBRNF_slope_filt_rast, "MBRNF_slope_filt_rast.tif")
MBRNF_slope_filt_rast <- rast("MBRNF_slope_filt_rast.tif")


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

# get just roads in the MBRNF
USFS_roads_MBRNF <- terra::intersect(USFS_roads_SRME_projected, MBRNF_vect)
nrow(USFS_roads_MBRNF) # 3290

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_MBRNF)
unique(USFS_roads_MBRNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_MBRNF <- USFS_roads_MBRNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_MBRNF) # 1993
plot(USFS_roads_MBRNF)
(1993/3290)* 100 # = 60.57751 % of FS roads retained
100 - 60.57751 # = 39.42249 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the MBRNF
USGS_roads_MBRNF <- terra::intersect(USGS_roads_SRME_proj, MBRNF_vect)
nrow(USGS_roads_MBRNF) # 14013
plot(USGS_roads_MBRNF)


### rasterize ----
#### USFS ----
MBRNF_USFS_road_rast <- rasterize(USFS_roads_MBRNF, MBRNF_QMD_filt_rast , touches=TRUE)
plot(MBRNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(MBRNF_USFS_road_rast)) # values not 1 are NA
global(MBRNF_USFS_road_rast, fun = "notNA") # 181582 cells not NA

#### USGS ----
MBRNF_USGS_road_rast <- rasterize(USGS_roads_MBRNF, MBRNF_QMD_filt_rast , touches=TRUE)
plot(MBRNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(MBRNF_USGS_road_rast)) # values not 1 are NA
global(MBRNF_USGS_road_rast, fun = "notNA") # 340396 cells not NA


### combine ----
MBRNF_road_rast <- cover(MBRNF_USFS_road_rast, MBRNF_USGS_road_rast)
plot(MBRNF_road_rast)
plot(is.na(MBRNF_road_rast))
global(MBRNF_road_rast, fun = "notNA") # 367668 cells not NA

##### write & read ----
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


### filter ----
minmax(MBRNF_road_dist_rast) 
# min = 0, max = 143650.9 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
MBRNF_road_filt_rast <- ifel(MBRNF_road_dist_rast > 917.3261, NA, 500)
plot(MBRNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the MBRNF
MBRNF_road_filt_rast = crop(MBRNF_road_filt_rast, MBRNF_vect, mask = TRUE)


### viz ----
plot(MBRNF_road_filt_rast, col = "darkorchid2")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(MBRNF_road_filt_rast, "MBRNF_road_filt_rast.tif")
MBRNF_road_filt_rast <- rast("MBRNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(MBRNF_slope_filt_rast, MBRNF_EVH_filt_rast, method = "near")

raster_list <- list(MBRNF_EVH_filt_rast,
                    MBRNF_QMD_filt_rast,
                    MBRNF_road_filt_rast,
                    slope_resampled)

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
global(MBRNF_DEM_rast, fun = "notNA") # 9199894 cells (covers all MBRNF)
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
global(MBRNF_road_filt_rast, fun = "notNA") # 5316596 cells
# entire MBRNF = 7773990 cells
(5316596/7773990)*100 # 68.38954 % remaining


### combined PFs ----
# we want to know what % of the MBRNF each category falls into after combining

# value 5, QMD only
global(MBRNF_combined_rast == 5, fun = "sum", na.rm = TRUE) # 24212 cells
(24212/7773990)*100 # 0.3114488 % of MBRNF

# value 10, EVH only
global(MBRNF_combined_rast == 10, fun = "sum", na.rm = TRUE) # 86352 cells
(86352/7773990)*100 # 1.110781 % of MBRNF

# value 15, QMD + EVH
global(MBRNF_combined_rast == 15, fun = "sum", na.rm = TRUE) # 176429 cells
(176429/7773990)*100 # 2.269478 % of MBRNF

# value 100, slope only
global(MBRNF_combined_rast == 100, fun = "sum", na.rm = TRUE) # 589281 cells
(589281/7773990)*100 # 7.580162 % of MBRNF

# value 105, slope + QMD
global(MBRNF_combined_rast == 105, fun = "sum", na.rm = TRUE) # 84525 cells
(84525/7773990)*100 # 1.08728 % of MBRNF

# value 110, slope + EVH
global(MBRNF_combined_rast == 110, fun = "sum", na.rm = TRUE) # 391693 cells
(391693/7773990)*100 # 5.038507 % of MBRNF

# value 115, slope + EVH + QMD
global(MBRNF_combined_rast == 115, fun = "sum", na.rm = TRUE) # 790573 cells
(790573/7773990)*100 # 10.16946 % of MBRNF

# value 500, road only
global(MBRNF_combined_rast == 500, fun = "sum", na.rm = TRUE) # 215441 cells
(215441/7773990)*100 # 2.771305 % of MBRNF

# value 505, road + QMD
global(MBRNF_combined_rast == 505, fun = "sum", na.rm = TRUE) # 52812 cells
(52812/7773990)*100 # 0.6793423 % of MBRNF

# value 510, road + EVH
global(MBRNF_combined_rast == 510, fun = "sum", na.rm = TRUE) # 169563 cells
(169563/7773990)*100 # 2.181158 % of MBRNF

# value 515, road + EVH + QMD
global(MBRNF_combined_rast == 515, fun = "sum", na.rm = TRUE) # 452365 cells
(452365/7773990)*100 # 5.818955 % of MBRNF

# value 600, road + slope
global(MBRNF_combined_rast == 600, fun = "sum", na.rm = TRUE) # 1002984 cells
(1002984/7773990)*100 # 12.90179 % of MBRNF

# value 605, road + slope + QMD
global(MBRNF_combined_rast == 605, fun = "sum", na.rm = TRUE) # 258732 cells
(258732/7773990)*100 # 3.328175 % of MBRNF

# value 610, road + slope + EVH
global(MBRNF_combined_rast == 610, fun = "sum", na.rm = TRUE) # 843644 cells
(843644/7773990)*100 # 10.85214 % of MBRNF

# value 615, road + slope + QMD + EVH
global(MBRNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2321055 cells
(2321055/7773990)*100 # 29.85668 % of MBRNF

# value notNA
global(MBRNF_combined_rast, fun = "notNA") # 7459661 cells
(7459661/7773990)*100 # 95.95666 % of MBRNF (equals the sum of above %s)
100-95.95666 # 4.04334 % is NA (QMD < 5in, EVH < 10ft, slope >24, road >0.57)


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
MBRNF_priority_rast <- ifel(
  MBRNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(MBRNF_priority_rast, fun = "notNA") # 2321055 cells (same as value=615 above)
(2321055/7773990)*100 # 29.85668 % of MBRNF


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(MBRNF_priority_rast, transform = FALSE) # 2088949500 m^2
2088949500/4046.86 # 4046.86 m2/acre = 516190.2 acres
# entire MBRNF = 1723619 acres (calculated from MBRNF_vect polygon in Part1A_2)
(516190.2/1723619)*100 # 29.94805 % of MBRNF (almost same as value=615 above)

## viz ----
plot(MBRNF_priority_rast, col = "goldenrod1")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(MBRNF_priority_rast, "MBRNF_priority_rast.tif")
MBRNF_priority_rast <- rast("MBRNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(MBRNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 95527 patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 95527 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1429 geoms remain
(1429/134187)*100 # 1.064932 % of polys remain (are >= 20 acres)
# so ~99 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1196 geoms
(1196/1414)*100 # 84.58274 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 233 geoms
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
# 2910 geoms

# combine the mid-sized polys with the newly divided large polys
MBRNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4106 geoms


## adjust ----
# add new ID col & new final area col
MBRNF_PCUs_1A_vect$PCU_ID <- 1:nrow(MBRNF_PCUs_1A_vect)
MBRNF_PCUs_1A_vect$area_acres <- expanse(MBRNF_PCUs_1A_vect) * 0.000247105

summary(MBRNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 265.60  
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
plot(MBRNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(MBRNF_PCUs_1A_vect, "MBRNF_PCUs_1A_vect.shp")
MBRNF_PCUs_1A_vect <- vect("MBRNF_PCUs_1A_vect.shp")


