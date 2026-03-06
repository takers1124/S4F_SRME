# Grand Mesa, Uncompahgre and Gunnison National Forests

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

# select for just GMUGNF 
GMUGNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Grand Mesa, Uncompahgre and Gunnison National Forests")
plot(GMUGNF_vect)

# project 
GMUGNF_vect <- project(GMUGNF_vect,"EPSG:5070")

# calc area
expanse(GMUGNF_vect) # 12760571788 m^2
12760571788/4046.86 # 4046.86 m/acre = 3153203 acres

## write & read
writeVector(GMUGNF_vect, "GMUGNF_vect.shp")
GMUGNF_vect <- vect("GMUGNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
GMUGNF_QMD_rast <- crop(QMD_CONUS, GMUGNF_vect, mask=TRUE)
plot(GMUGNF_QMD_rast)

#### write & read ----
writeRaster(GMUGNF_QMD_rast, "GMUGNF_QMD_rast.tif")
GMUGNF_QMD_rast <- rast("GMUGNF_QMD_rast.tif")

global(GMUGNF_QMD_rast, fun = "notNA") # 10070898 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

GMUGNF_QMD_filt_rast <- ifel(
  GMUGNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(GMUGNF_QMD_filt_rast, col = "darkgreen")
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(GMUGNF_QMD_filt_rast, "GMUGNF_QMD_filt_rast.tif")
GMUGNF_QMD_filt_rast <- rast("GMUGNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_GMUGNF <- crop(EVH_CONUS, GMUGNF_vect, mask=TRUE)
plot(EVH_GMUGNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

GMUGNF_EVH_rast <- ifel(
  EVH_GMUGNF >= 101 & EVH_GMUGNF < 200,
  (EVH_GMUGNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(GMUGNF_EVH_rast)
global(GMUGNF_EVH_rast, fun = "notNA") # 10071238
summary(GMUGNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(GMUGNF_EVH_rast, "GMUGNF_EVH_rast.tif")
GMUGNF_EVH_rast <- rast("GMUGNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

GMUGNF_EVH_filt_rast <- ifel(
  GMUGNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(GMUGNF_EVH_filt_rast, col = "forestgreen")
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(GMUGNF_EVH_filt_rast, "GMUGNF_EVH_filt_rast.tif")
GMUGNF_EVH_filt_rast <- rast("GMUGNF_EVH_filt_rast.tif")


## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n38_w107 <- rast("USGS_1_n38w107_20220720.tif")
DEM_n38_w108 <- rast("USGS_1_n38w108_20220720.tif")
DEM_n38_w109 <- rast("USGS_1_n38w109_20220720.tif")
DEM_n39_w107 <- rast("USGS_1_n39w107_20220331.tif")
DEM_n39_w108 <- rast("USGS_1_n39w108_20220720.tif")
DEM_n39_w109 <- rast("USGS_1_n39w109_20211208.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")
DEM_n40_w108 <- rast("USGS_1_n40w108_20211208.tif")
DEM_n40_w109 <- rast("USGS_1_n40w109_20180328.tif")

# mosaic 9 tiles together
GMUGNF_DEM <- mosaic(DEM_n38_w107, DEM_n38_w108, DEM_n38_w109,
                     DEM_n39_w107, DEM_n39_w108, DEM_n39_w109, 
                     DEM_n40_w107, DEM_n40_w108, DEM_n40_w109,
                     fun = "first")

crs(GMUGNF_DEM) # EPSG: 4269
res(GMUGNF_DEM) # 0.0002777778

# project
GMUGNF_DEM <- project(GMUGNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of GMUGNF 
GMUGNF_DEM_rast <- crop(GMUGNF_DEM, GMUGNF_vect, mask=TRUE)
plot(GMUGNF_DEM_rast) # min = 1442.115 , max = 4354.024  (meters)
plot(is.na(GMUGNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(GMUGNF_DEM_rast, "GMUGNF_DEM_rast.tif")
GMUGNF_DEM_rast <- rast("GMUGNF_DEM_rast.tif")

### calc slope ----
GMUGNF_slope_rast = terrain(GMUGNF_DEM_rast, v="slope", unit="degrees")
plot(GMUGNF_slope_rast)

#### write & read ----
writeRaster(GMUGNF_slope_rast, "GMUGNF_slope_rast.tif")
GMUGNF_slope_rast <- rast("GMUGNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

GMUGNF_slope_filt_rast <- ifel(GMUGNF_slope_rast > 24, NA, 100)

### viz ----
plot(GMUGNF_slope_filt_rast, col = "mediumorchid2")
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(GMUGNF_slope_filt_rast, "GMUGNF_slope_filt_rast.tif")
GMUGNF_slope_filt_rast <- rast("GMUGNF_slope_filt_rast.tif")


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

# get just roads in the GMUGNF
USFS_roads_GMUGNF <- terra::intersect(USFS_roads_SRME_projected, GMUGNF_vect)
nrow(USFS_roads_GMUGNF) # 2240

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_GMUGNF)
unique(USFS_roads_GMUGNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_GMUGNF <- USFS_roads_GMUGNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_GMUGNF) # 2076
plot(USFS_roads_GMUGNF)
(2076/2240)* 100 # = 92.67857 % of FS roads retained
100 - 92.67857 # = 7.32143 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the GMUGNF
USGS_roads_GMUGNF <- terra::intersect(USGS_roads_SRME_proj, GMUGNF_vect)
nrow(USGS_roads_GMUGNF) # 14748
plot(USGS_roads_GMUGNF)


### rasterize ----
#### USFS ----
GMUGNF_USFS_road_rast <- rasterize(USFS_roads_GMUGNF, GMUGNF_QMD_filt_rast , touches=TRUE)
plot(GMUGNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(GMUGNF_USFS_road_rast)) # values not 1 are NA
global(GMUGNF_USFS_road_rast, fun = "notNA") # 209188 cells not NA

#### USGS ----
GMUGNF_USGS_road_rast <- rasterize(USGS_roads_GMUGNF, GMUGNF_QMD_filt_rast , touches=TRUE)
plot(GMUGNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(GMUGNF_USGS_road_rast)) # values not 1 are NA
global(GMUGNF_USGS_road_rast, fun = "notNA") # 329005 cells not NA


### combine ----
GMUGNF_road_rast <- cover(GMUGNF_USFS_road_rast, GMUGNF_USGS_road_rast)
plot(GMUGNF_road_rast)
plot(is.na(GMUGNF_road_rast))
global(GMUGNF_road_rast, fun = "notNA") # 354649 cells not NA

##### write & read ----
writeRaster(GMUGNF_road_rast, "GMUGNF_road_rast.tif")
GMUGNF_road_rast <- rast("GMUGNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
GMUGNF_road_dist_rast <- distance(GMUGNF_road_rast) 
plot(GMUGNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(GMUGNF_road_dist_rast, "GMUGNF_road_dist_rast.tif")
GMUGNF_road_dist_rast <- rast("GMUGNF_road_dist_rast.tif")


### filter ----
minmax(GMUGNF_road_dist_rast) 
# min = 0, max = 58308.8 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
GMUGNF_road_filt_rast <- ifel(GMUGNF_road_dist_rast > 917.3261, NA, 500)
plot(GMUGNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the GMUGNF
GMUGNF_road_filt_rast = crop(GMUGNF_road_filt_rast, GMUGNF_vect, mask = TRUE)


### viz ----
plot(GMUGNF_road_filt_rast, col = "darkorchid2")
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(GMUGNF_road_filt_rast, "GMUGNF_road_filt_rast.tif")
GMUGNF_road_filt_rast <- rast("GMUGNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(GMUGNF_slope_filt_rast, GMUGNF_EVH_filt_rast, method = "near")

raster_list <- list(GMUGNF_EVH_filt_rast,
                    GMUGNF_QMD_filt_rast,
                    GMUGNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
GMUGNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(GMUGNF_combined_rast)
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(GMUGNF_combined_rast))


## stats ----
# we want to know what % of the GMUGNF each priority factor occupies
# need a total # cells in the GMUGNF to compare
global(GMUGNF_DEM_rast, fun = "notNA") # 16570239 cells (covers all GMUGNF)
# but not same resolution as rest of data
DEM_resampled <- resample(GMUGNF_DEM_rast, GMUGNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 14220592 cells (covers all GMUGNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(GMUGNF_QMD_rast, fun = "notNA") # 10070898 cells
(10070898/14220592)*100 # 70.81912 % of GMUGNF has QMD values

# areas with QMD > 5 inches
global(GMUGNF_QMD_filt_rast, fun = "notNA") # 7674672
(7674672/14220592)*100 # 53.96872 % of GMUGNF has trees > 5 in QMD

#### EVH ----
# all tree area
global(GMUGNF_EVH_rast, fun = "notNA") # 10071238
(10071238/14220592)*100 # 70.82151 % of GMUGNF has trees 

# trees > 10 ft area
global(GMUGNF_EVH_filt_rast, fun = "notNA") # 9805068
(9805068/14220592)*100 # 68.94979 % of GMUGNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 11223232 cells 
(11223232/14220592)*100 # 78.9224 % remaining after 24* filter

#### road ----
global(GMUGNF_road_filt_rast, fun = "notNA") # 8129888 cells
(8129888/14220592)*100 # 57.16983 % remaining


### combined PFs ----
# we want to know what % of the GMUGNF meets all of the priority factor thresholds, after combining

# value 615 = road + slope + QMD + EVH
global(GMUGNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 3732527 cells
(3732527/14220592)*100 # 26.24734 % of GMUGNF


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
GMUGNF_priority_rast <- ifel(
  GMUGNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(GMUGNF_priority_rast, fun = "notNA") # 3732527 cells (same as value=615 above)
(3732527/14220592)*100 # 26.24734 % of GMUGNF


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(GMUGNF_priority_rast, transform = FALSE) # 3359274300 m^2
3359274300/4046.86 # 4046.86 m2/acre = 830094 acres

# entire GMUGNF = 3153203 acres (calculated from GMUGNF_vect polygon in Part1A_2)
(830094/3153203)*100 # 26.32542 % of GMUGNF (almost same as value=615 above)


## viz ----
plot(GMUGNF_priority_rast, col = "goldenrod1")
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(GMUGNF_priority_rast, "GMUGNF_priority_rast.tif")
GMUGNF_priority_rast <- rast("GMUGNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(GMUGNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 145492  patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 145492 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 2018 geoms remain
(2018/145492)*100 # 1.387018 % of polys remain (are >= 20 acres)
# so ~98.6 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1630 geoms
(1630/2018)*100 # 80.77304 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 388 geoms
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
# 4948 geoms

# combine the mid-sized polys with the newly divided large polys
GMUGNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 6578 geoms


## adjust ----
# add new ID col & new final area col
GMUGNF_PCUs_1A_vect$PCU_ID <- paste0("GMUGNF_PCU_", seq_len(nrow(GMUGNF_PCUs_1A_vect)))
GMUGNF_PCUs_1A_vect$area_acres <- expanse(GMUGNF_PCUs_1A_vect) * 0.000247105

summary(GMUGNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 329.77    
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
GMUGNF_PCUs_1A_vect <- GMUGNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

GMUGNF_PCUs_1A_df <- as.data.frame(GMUGNF_PCUs_1A_vect)

sum(GMUGNF_PCUs_1A_vect$area_acres) # 709148.9 acres
sum(small_polys_removed$patch_acres) # 709148.9 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# GMUGNF is 3153203 acres 
(709148.9/3153203)*100 # 22.4898 % of GMUGNF are highest priority areas (PCUs)


## viz ----
plot(GMUGNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(GMUGNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(GMUGNF_PCUs_1A_vect, "GMUGNF_PCUs_1A_vect.shp")
GMUGNF_PCUs_1A_vect <- vect("GMUGNF_PCUs_1A_vect.shp")



