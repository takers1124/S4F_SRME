# Pike and San Isabel National Forests

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

# select for just PSINF 
PSINF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Pike and San Isabel National Forests")
plot(PSINF_vect)

# project 
PSINF_vect <- project(PSINF_vect,"EPSG:5070")

# calc area
expanse(PSINF_vect) # 10137289879 m^2
10137289879/4046.86 # 4046.86 m/acre = 2504977 acres

## write & read ----
writeVector(PSINF_vect, "PSINF_vect.shp")
PSINF_vect <- vect("PSINF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
PSINF_QMD_rast <- crop(QMD_CONUS, PSINF_vect, mask=TRUE)
plot(PSINF_QMD_rast)

#### write & read ----
writeRaster(PSINF_QMD_rast, "PSINF_QMD_rast.tif")
PSINF_QMD_rast <- rast("PSINF_QMD_rast.tif")

global(PSINF_QMD_rast, fun = "notNA") # 7950133 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

PSINF_QMD_filt_rast <- ifel(
  PSINF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(PSINF_QMD_filt_rast, col = "darkgreen")
polys(PSINF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(PSINF_QMD_filt_rast, "PSINF_QMD_filt_rast.tif")
PSINF_QMD_filt_rast <- rast("PSINF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_PSINF <- crop(EVH_CONUS, PSINF_vect, mask=TRUE)
plot(EVH_PSINF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

PSINF_EVH_rast <- ifel(
  EVH_PSINF >= 101 & EVH_PSINF < 200,
  (EVH_PSINF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(PSINF_EVH_rast)
global(PSINF_EVH_rast, fun = "notNA") # 7939994
summary(PSINF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(PSINF_EVH_rast, "PSINF_EVH_rast.tif")
PSINF_EVH_rast <- rast("PSINF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

PSINF_EVH_filt_rast <- ifel(
  PSINF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(PSINF_EVH_filt_rast, col = "forestgreen")
polys(PSINF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(PSINF_EVH_filt_rast, "PSINF_EVH_filt_rast.tif")
PSINF_EVH_filt_rast <- rast("PSINF_EVH_filt_rast.tif")


## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n38_w105 <- rast("USGS_1_n38w105_20230602.tif")
DEM_n38_w106 <- rast("USGS_1_n38w106_20230602.tif")
DEM_n39_w105 <- rast("USGS_1_n39w105_20230602.tif")
DEM_n39_w106 <- rast("USGS_1_n39w106_20230602.tif")
DEM_n39_w107 <- rast("USGS_1_n39w107_20220331.tif")
DEM_n40_w105 <- rast("USGS_1_n40w105_20230602.tif")
DEM_n40_w106 <- rast("USGS_1_n40w106_20230602.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")

# mosaic 8 tiles together
PSINF_DEM <- mosaic(DEM_n38_w105, DEM_n38_w106,  
                   DEM_n39_w105, DEM_n39_w106, DEM_n39_w107,
                   DEM_n40_w105, DEM_n40_w106, DEM_n40_w107,
                   fun = "first")

crs(PSINF_DEM) # EPSG: 4269
res(PSINF_DEM) # 0.0002777778

# project
PSINF_DEM <- project(PSINF_DEM, "EPSG:5070")
plot(PSINF_DEM)

# crop and mask the DEM to the extent of PSINF 
PSINF_DEM_rast <- crop(PSINF_DEM, PSINF_vect, mask=TRUE)
plot(PSINF_DEM_rast) # min = 1424.028 , max = 4392.732  (meters)
plot(is.na(PSINF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(PSINF_DEM_rast, "PSINF_DEM_rast.tif")
PSINF_DEM_rast <- rast("PSINF_DEM_rast.tif")

### calc slope ----
PSINF_slope_rast = terrain(PSINF_DEM_rast, v="slope", unit="degrees")
plot(PSINF_slope_rast)

#### write & read ----
writeRaster(PSINF_slope_rast, "PSINF_slope_rast.tif")
PSINF_slope_rast <- rast("PSINF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

PSINF_slope_filt_rast <- ifel(PSINF_slope_rast > 24, NA, 100)

### viz ----
plot(PSINF_slope_filt_rast, col = "mediumorchid2")
polys(PSINF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(PSINF_slope_filt_rast, "PSINF_slope_filt_rast.tif")
PSINF_slope_filt_rast <- rast("PSINF_slope_filt_rast.tif")


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

# get just roads in the PSINF
USFS_roads_PSINF <- terra::intersect(USFS_roads_SRME_projected, PSINF_vect)
nrow(USFS_roads_PSINF) # 1871

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_PSINF)
unique(USFS_roads_PSINF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_PSINF <- USFS_roads_PSINF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_PSINF) # 1662
plot(USFS_roads_PSINF)
(1662/1871)* 100 # = 88.8295 % of FS roads retained
100 - 88.8295 # = 11.1705 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the PSINF
USGS_roads_PSINF <- terra::intersect(USGS_roads_SRME_proj, PSINF_vect)
nrow(USGS_roads_PSINF) # 18432
plot(USGS_roads_PSINF)


### rasterize ----
#### USFS ----
PSINF_USFS_road_rast <- rasterize(USFS_roads_PSINF, PSINF_QMD_filt_rast , touches=TRUE)
plot(PSINF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(PSINF_USFS_road_rast)) # values not 1 are NA
global(PSINF_USFS_road_rast, fun = "notNA") # 136768 cells not NA

#### USGS ----
PSINF_USGS_road_rast <- rasterize(USGS_roads_PSINF, PSINF_QMD_filt_rast , touches=TRUE)
plot(PSINF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(PSINF_USGS_road_rast)) # values not 1 are NA
global(PSINF_USGS_road_rast, fun = "notNA") # 307907 cells not NA


### combine ----
PSINF_road_rast <- cover(PSINF_USFS_road_rast, PSINF_USGS_road_rast)
plot(PSINF_road_rast)
plot(is.na(PSINF_road_rast))
global(PSINF_road_rast, fun = "notNA") # 341290 cells not NA

##### write & read ----
writeRaster(PSINF_road_rast, "PSINF_road_rast.tif")
PSINF_road_rast <- rast("PSINF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
PSINF_road_dist_rast <- distance(PSINF_road_rast) 
plot(PSINF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(PSINF_road_dist_rast, "PSINF_road_dist_rast.tif")
PSINF_road_dist_rast <- rast("PSINF_road_dist_rast.tif")


### filter ----
minmax(PSINF_road_dist_rast) 
# min = 0, max = 94286.66 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
PSINF_road_filt_rast <- ifel(PSINF_road_dist_rast > 917.3261, NA, 500)
plot(PSINF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the PSINF
PSINF_road_filt_rast = crop(PSINF_road_filt_rast, PSINF_vect, mask = TRUE)


### viz ----
plot(PSINF_road_filt_rast, col = "darkorchid2")
polys(PSINF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(PSINF_road_filt_rast, "PSINF_road_filt_rast.tif")
PSINF_road_filt_rast <- rast("PSINF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(PSINF_slope_filt_rast, PSINF_EVH_filt_rast, method = "near")

raster_list <- list(PSINF_EVH_filt_rast,
                    PSINF_QMD_filt_rast,
                    PSINF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
PSINF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(PSINF_combined_rast)
polys(PSINF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(PSINF_combined_rast))


## stats ----
# we want to know what % of the PSINF each priority factor (PF) & combo occupies
# need a total # cells in the PSINF to compare
global(PSINF_DEM_rast, fun = "notNA") # 13169666 cells (covers all PSINF)
# but not same resolution as rest of data
DEM_resampled <- resample(PSINF_DEM_rast, PSINF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 11302120 cells (covers all PSINF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(PSINF_QMD_rast, fun = "notNA") # 7950133 cells
(7950133/11302120)*100 # 70.34196 % of PSINF has QMD values

# areas with QMD > 5 inches
global(PSINF_QMD_filt_rast, fun = "notNA") # 6122488
(6122488/11302120)*100 # 54.17115 % of PSINF has trees > 5 in QMD

#### EVH ----
# all tree area
global(PSINF_EVH_rast, fun = "notNA") # 7939994
(7939994/11302120)*100 # 70.25225 % of PSINF has trees 

# trees > 10 ft area
global(PSINF_EVH_filt_rast, fun = "notNA") # 7585335
(7585335/11302120)*100 # 67.11427 % of PSINF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 8599515 cells 
(8599515/11302120)*100 # 76.08763 % remaining after 24* filter

#### road ----
global(PSINF_road_filt_rast, fun = "notNA") # 7012228 cells
(7012228/11302120)*100 # 62.04348 % remaining


### combined PFs ----
# we want to know what % of the PSINF meets all of the priority factor thresholds, after combining

# value 615 = road + slope + QMD + EVH
global(PSINF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 3126082 cells
(3126082/11302120)*100 # 27.65925 % of PSINF


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
PSINF_priority_rast <- ifel(
  PSINF_combined_rast == 615,
  1, NA)

# just confirm filter
global(PSINF_priority_rast, fun = "notNA") # 3126082 cells (same as value=615 above)


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(PSINF_priority_rast, transform = FALSE) # 2813473800 m^2
2813473800/4046.86 # 4046.86 m2/acre = 695223.9 acres
# entire PSINF = 2504977 acres (calculated from PSINF_vect polygon in Part1A_2)
(695223.9/2504977)*100 # 27.7537 % of PSINF (almost same as value=615 above)

## viz ----
plot(PSINF_priority_rast, col = "goldenrod1")
polys(PSINF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(PSINF_priority_rast, "PSINF_priority_rast.tif")
PSINF_priority_rast <- rast("PSINF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(PSINF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 115902 patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 115902 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1881 geoms remain
(1881/115902)*100 # 1.622923 % of polys remain (are >= 20 acres)
# so ~98 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1563 geoms
(1563/1881)*100 # 83.0941 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 318 geoms
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
# 4065 geoms

# combine the mid-sized polys with the newly divided large polys
PSINF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 5628 geoms


## adjust ----
# add new ID col & new final area col
PSINF_PCUs_1A_vect$PCU_ID <- paste0("PSINF_PCU_", seq_len(nrow(PSINF_PCUs_1A_vect)))
PSINF_PCUs_1A_vect$area_acres <- expanse(PSINF_PCUs_1A_vect) * 0.000247105

summary(PSINF_PCUs_1A_vect)
# area_acres min = 17.07, max = 319.66    
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
PSINF_PCUs_1A_vect <- PSINF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

PSINF_PCUs_1A_df <- as.data.frame(PSINF_PCUs_1A_vect)

sum(PSINF_PCUs_1A_vect$area_acres) # 594562.4 acres
sum(small_polys_removed$patch_acres) # 594562.4 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# PSINF is 2504977 acres 
(594562.4/2504977)*100 # 23.73524 % of PSINF are highest priority areas (PCUs)


## viz ----
plot(PSINF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(PSINF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(PSINF_PCUs_1A_vect, "PSINF_PCUs_1A_vect.shp")
PSINF_PCUs_1A_vect <- vect("PSINF_PCUs_1A_vect.shp")


