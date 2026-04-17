# White River National Forest

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

# select for just WRNF 
WRNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "White River National Forest")
plot(WRNF_vect)

# project 
WRNF_vect <- project(WRNF_vect,"EPSG:5070")

# calc area
expanse(WRNF_vect) # 10047184922 m^2
10047184922/4046.86 # 4046.86 m/acre = 2482711 acres

## write & read ----
writeVector(WRNF_vect, "./WhiteRiverNF_S4F/.shp/WRNF_vect.shp")
WRNF_vect <- vect("./WhiteRiverNF_S4F/.shp/WRNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
WRNF_QMD_rast <- crop(QMD_CONUS, WRNF_vect, mask=TRUE)
plot(WRNF_QMD_rast)

#### write & read ----
writeRaster(WRNF_QMD_rast, "./WhiteRiverNF_S4F/.tif/WRNF_QMD_rast.tif")
WRNF_QMD_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_QMD_rast.tif")

global(WRNF_QMD_rast, fun = "notNA") # 7769855 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

WRNF_QMD_filt_rast <- ifel(
  WRNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(WRNF_QMD_filt_rast, col = "darkgreen")
polys(WRNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(WRNF_QMD_filt_rast, "./WhiteRiverNF_S4F/.tif/WRNF_QMD_filt_rast.tif")
WRNF_QMD_filt_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_WRNF <- crop(EVH_CONUS, WRNF_vect, mask=TRUE)
plot(EVH_WRNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

WRNF_EVH_rast <- ifel(
  EVH_WRNF >= 101 & EVH_WRNF < 200,
  (EVH_WRNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(WRNF_EVH_rast)
global(WRNF_EVH_rast, fun = "notNA") # 7847389
summary(WRNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(WRNF_EVH_rast, "./WhiteRiverNF_S4F/.tif/WRNF_EVH_rast.tif")
WRNF_EVH_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

WRNF_EVH_filt_rast <- ifel(
  WRNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(WRNF_EVH_filt_rast, col = "forestgreen")
polys(WRNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(WRNF_EVH_filt_rast, "./WhiteRiverNF_S4F/.tif/WRNF_EVH_filt_rast.tif")
WRNF_EVH_filt_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_EVH_filt_rast.tif")


## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n39_w107 <- rast("USGS_1_n39w107_20220331.tif")
DEM_n39_w108 <- rast("USGS_1_n39w108_20220720.tif")
DEM_n40_w106 <- rast("USGS_1_n40w106_20230602.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")
DEM_n40_w108 <- rast("USGS_1_n40w108_20230602.tif")
DEM_n40_w109 <- rast("USGS_1_n40w109_20180328.tif")
DEM_n41_w107 <- rast("USGS_1_n41w107_20230314.tif")
DEM_n41_w108 <- rast("USGS_1_n41w108_20230314.tif")

# mosaic 8 tiles together
WRNF_DEM <- mosaic(DEM_n39_w107, DEM_n39_w108, 
                   DEM_n40_w106, DEM_n40_w107, 
                   DEM_n40_w108, DEM_n40_w109,
                   DEM_n41_w107, DEM_n41_w108,
                   fun = "first")

crs(WRNF_DEM) # EPSG: 4269
res(WRNF_DEM) # 0.0002777778

# project
WRNF_DEM <- project(WRNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of WRNF 
WRNF_DEM_rast <- crop(WRNF_DEM, WRNF_vect, mask=TRUE)
plot(WRNF_DEM_rast) # min = 1661.947 , max = 4346.247 (meters)
plot(is.na(WRNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(WRNF_DEM_rast, "./WhiteRiverNF_S4F/.tif/WRNF_DEM_rast.tif")
WRNF_DEM_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_DEM_rast.tif")

### calc slope ----
WRNF_slope_rast = terrain(WRNF_DEM_rast, v="slope", unit="degrees")
plot(WRNF_slope_rast)

#### write & read ----
writeRaster(WRNF_slope_rast, "./WhiteRiverNF_S4F/.tif/WRNF_slope_rast.tif")
WRNF_slope_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

WRNF_slope_filt_rast <- ifel(WRNF_slope_rast > 24, NA, 100)

### viz ----
plot(WRNF_slope_filt_rast, col = "mediumorchid2")
polys(WRNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(WRNF_slope_filt_rast, "./WhiteRiverNF_S4F/.tif/WRNF_slope_filt_rast.tif")
WRNF_slope_filt_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_slope_filt_rast.tif")


## road ----
### load & process ----
#### USFS roads ----
# S_USA.Trans_RoadCore_FS.shp
# downloaded from the FS Geodata Clearinghouse
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USFS_roads_SRME <- vect("Trans_RoadCore_USFS_Clip_SRME.shp")
crs(USFS_roads_SRME) # EPSG: 5070
nrow(USFS_roads_SRME) # 26180

# create a buffer around NF
WRNF_buffer1km <- buffer(WRNF_vect, width = 1000)

# get just roads in the WRNF + buffer
USFS_roads_WRNF <- terra::intersect(USFS_roads_SRME, WRNF_buffer1km)
nrow(USFS_roads_WRNF) # 1160

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_WRNF)
unique(USFS_roads_WRNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_WRNF <- USFS_roads_WRNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_WRNF) # 1010
plot(USFS_roads_WRNF)
(1010/1160)* 100 # = 87.06897 % of FS roads retained
100 - 87.06897 # = 12.93103 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_USGS_Clip_SRME.shp")
crs(USGS_roads_SRME) # EPSG: 5070
nrow(USGS_roads_SRME) # 312312

# get just roads in the WRNF
USGS_roads_WRNF <- terra::intersect(USGS_roads_SRME, WRNF_buffer1km)
nrow(USGS_roads_WRNF) # 18007
plot(USGS_roads_WRNF)


### rasterize ----
#### USFS ----
WRNF_USFS_road_rast <- rasterize(USFS_roads_WRNF, WRNF_QMD_rast , touches=TRUE)
plot(WRNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(WRNF_USFS_road_rast)) # values not 1 are NA
global(WRNF_USFS_road_rast, fun = "notNA") # 112458 cells not NA

#### USGS ----
WRNF_USGS_road_rast <- rasterize(USGS_roads_WRNF, WRNF_QMD_rast , touches=TRUE)
plot(WRNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(WRNF_USGS_road_rast)) # values not 1 are NA
global(WRNF_USGS_road_rast, fun = "notNA") # 318016 cells not NA


### combine ----
WRNF_road_rast <- cover(WRNF_USFS_road_rast, WRNF_USGS_road_rast)
plot(WRNF_road_rast)
plot(is.na(WRNF_road_rast))
global(WRNF_road_rast, fun = "notNA") # 327289 cells not NA

##### write & read ----
writeRaster(WRNF_road_rast, "./WhiteRiverNF_S4F/.tif/WRNF_road_rast.tif")
WRNF_road_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
WRNF_road_dist_rast <- distance(WRNF_road_rast) 
plot(WRNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(WRNF_road_dist_rast, "./WhiteRiverNF_S4F/.tif/WRNF_road_dist_rast.tif")
WRNF_road_dist_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_road_dist_rast.tif")


### filter ----
minmax(WRNF_road_dist_rast) 
# min = 0, max = 61128.08 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
WRNF_road_filt_rast <- ifel(WRNF_road_dist_rast > 917.3261, NA, 500)
plot(WRNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the WRNF
WRNF_road_filt_rast = crop(WRNF_road_filt_rast, WRNF_vect, mask = TRUE)


### viz ----
plot(WRNF_road_filt_rast, col = "darkorchid2")
polys(WRNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(WRNF_road_filt_rast, "./WhiteRiverNF_S4F/.tif/WRNF_road_filt_rast.tif")
WRNF_road_filt_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(WRNF_slope_filt_rast, WRNF_EVH_filt_rast, method = "near")

raster_list <- list(WRNF_EVH_filt_rast,
                    WRNF_QMD_filt_rast,
                    WRNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
WRNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(WRNF_combined_rast)
polys(WRNF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(WRNF_combined_rast))


## stats ----
# we want to know what % of the WRNF each priority factor (PF) & combo occupies
# need a total # cells in the WRNF to compare
global(WRNF_DEM_rast, fun = "notNA") # 14235314 cells (covers all WRNF)
# but not same resolution as rest of data
DEM_resampled <- resample(WRNF_DEM_rast, WRNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 11186090 cells (covers all WRNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(WRNF_QMD_rast, fun = "notNA") # 7769855 cells
(7769855/11186090)*100 # 69.45997 % of WRNF has QMD values

# areas with QMD > 5 inches
global(WRNF_QMD_filt_rast, fun = "notNA") # 5806444
(5806444/11186090)*100 # 51.90772 % of WRNF has trees > 5 in QMD

#### EVH ----
# all tree area
global(WRNF_EVH_rast, fun = "notNA") # 7847389
(7847389/11186090)*100 # 70.1531 % of WRNF has trees 

# trees > 10 ft area
global(WRNF_EVH_filt_rast, fun = "notNA") # 7702526
(7702526/11186090)*100 # 68.85807 % of WRNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 8142656 cells 
(8142656/11186090)*100 # 72.79269 % remaining after 24* filter

#### road ----
global(WRNF_road_filt_rast, fun = "notNA") # 5797864 cells
(5797864/11186090)*100 # 51.83102 % remaining


### combined PFs ----
# we want to know what % of the WRNF meets all of the priority factor thresholds, after combining

# value 615 = road + slope + QMD + EVH
global(WRNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2443870 cells
(2443870/11186090)*100 # 21.8474 % of WRNF


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
WRNF_priority_rast <- ifel(
  WRNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(WRNF_priority_rast, fun = "notNA") # 2443870 cells (same as value=615 above)


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(WRNF_priority_rast, transform = FALSE) # 2199483000 m^2
2199483000/4046.86 # 4046.86 m2/acre = 543503.6 acres
# entire WRNF = 2482711 acres (calculated from WRNF_vect polygon in Part1A_2)
(543503.6/2482711)*100 # 21.89154 % of WRNF (almost same as value=615 above)

## viz ----
plot(WRNF_priority_rast, col = "goldenrod1")
polys(WRNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(WRNF_priority_rast, "./WhiteRiverNF_S4F/.tif/WRNF_priority_rast.tif")
WRNF_priority_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(WRNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 112514   patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 112514   geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1790 geoms remain
(1790/110519)*100 # 1.619631 % of polys remain (are >= 20 acres)
# so ~98 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1442 geoms
(1484/1790)*100 # 82.90503 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 306 geoms
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
# 2909 geoms

# combine the mid-sized polys with the newly divided large polys
WRNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4393 geoms


## adjust ----
# add new ID col & new final area col
WRNF_PCUs_1A_vect$PCU_ID <- paste0("WRNF_PCU_", seq_len(nrow(WRNF_PCUs_1A_vect)))
WRNF_PCUs_1A_vect$area_acres <- expanse(WRNF_PCUs_1A_vect) * 0.000247105

summary(WRNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 283.34      
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
WRNF_PCUs_1A_vect <- WRNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

WRNF_PCUs_1A_df <- as.data.frame(WRNF_PCUs_1A_vect)

sum(WRNF_PCUs_1A_vect$area_acres) # 444663 acres
sum(small_polys_removed$patch_acres) # 444663 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# WRNF is 2482711 acres 
(444663/2482711)*100 # 17.91038 % of WRNF are highest priority areas (PCUs)


## viz ----
plot(WRNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(WRNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(WRNF_PCUs_1A_vect, "./WhiteRiverNF_S4F/.shp/WRNF_PCUs_1A_vect.shp")
WRNF_PCUs_1A_vect <- vect("./WhiteRiverNF_S4F/.shp/WRNF_PCUs_1A_vect.shp")


