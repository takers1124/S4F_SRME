# Rio Grande National Forest

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

# select for just RGNF 
RGNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Rio Grande National Forest")
plot(RGNF_vect)

# project 
RGNF_vect <- project(RGNF_vect,"EPSG:5070")

# calc area
expanse(RGNF_vect) # 7839907991 m^2
7839907991/4046.86 # 4046.86 m/acre = 1937282 acres

#### write & read ----
writeVector(RGNF_vect, "RGNF_vect.shp")
RGNF_vect <- vect("RGNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
RGNF_QMD_rast <- crop(QMD_CONUS, RGNF_vect, mask=TRUE)
plot(RGNF_QMD_rast)

#### write & read ----
writeRaster(RGNF_QMD_rast, "RGNF_QMD_rast.tif")
RGNF_QMD_rast <- rast("RGNF_QMD_rast.tif")

global(RGNF_QMD_rast, fun = "notNA") # 6115825 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

RGNF_QMD_filt_rast <- ifel(
  RGNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(RGNF_QMD_filt_rast, col = "darkgreen")
polys(RGNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(RGNF_QMD_filt_rast, "RGNF_QMD_filt_rast.tif")
RGNF_QMD_filt_rast <- rast("RGNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_RGNF <- crop(EVH_CONUS, RGNF_vect, mask=TRUE)
plot(EVH_RGNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

RGNF_EVH_rast <- ifel(
  EVH_RGNF >= 101 & EVH_RGNF < 200,
  (EVH_RGNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(RGNF_EVH_rast)
global(RGNF_EVH_rast, fun = "notNA") # 6064506
summary(RGNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(RGNF_EVH_rast, "RGNF_EVH_rast.tif")
RGNF_EVH_rast <- rast("RGNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

RGNF_EVH_filt_rast <- ifel(
  RGNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(RGNF_EVH_filt_rast, col = "forestgreen")
polys(RGNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(RGNF_EVH_filt_rast, "RGNF_EVH_filt_rast.tif")
RGNF_EVH_filt_rast <- rast("RGNF_EVH_filt_rast.tif")



## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n38_w106 <- rast("USGS_1_n38w106_20230602.tif")
DEM_n38_w107 <- rast("USGS_1_n38w107_20220720.tif")
DEM_n38_w108 <- rast("USGS_1_n38w108_20220720.tif")
DEM_n39_w106 <- rast("USGS_1_n39w106_20230602.tif")
DEM_n39_w107 <- rast("USGS_1_n39w107_20220331.tif")

# mosaic 4 tiles together
RGNF_DEM <- mosaic(DEM_n38_w106, DEM_n38_w107, DEM_n38_w108, DEM_n39_w106, DEM_n39_w107, fun = "first")

crs(RGNF_DEM) # EPSG: 4269
res(RGNF_DEM) # 0.0002777778

# project
RGNF_DEM <- project(RGNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of RGNF 
RGNF_DEM_rast <- crop(RGNF_DEM, RGNF_vect, mask=TRUE)
plot(RGNF_DEM_rast) # min = 2297.928 , max = 4365.588  (meters)
plot(is.na(RGNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(RGNF_DEM_rast, "RGNF_DEM_rast.tif")
RGNF_DEM_rast <- rast("RGNF_DEM_rast.tif")

### calc slope ----
RGNF_slope_rast = terrain(RGNF_DEM_rast, v="slope", unit="degrees")
plot(RGNF_slope_rast)

#### write & read ----
writeRaster(RGNF_slope_rast, "RGNF_slope_rast.tif")
RGNF_slope_rast <- rast("RGNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

RGNF_slope_filt_rast <- ifel(RGNF_slope_rast > 24, NA, 100)

### viz ----
plot(RGNF_slope_filt_rast, col = "mediumorchid2")
polys(RGNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(RGNF_slope_filt_rast, "RGNF_slope_filt_rast.tif")
RGNF_slope_filt_rast <- rast("RGNF_slope_filt_rast.tif")


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

# get just roads in the RGNF
USFS_roads_RGNF <- terra::intersect(USFS_roads_SRME_projected, RGNF_vect)
nrow(USFS_roads_RGNF) # 1293

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_RGNF)
unique(USFS_roads_RGNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_RGNF <- USFS_roads_RGNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_RGNF) # 778
plot(USFS_roads_RGNF)
(778/1293)* 100 # = 60.17015 % of FS roads retained
100 - 60.17015 # = 39.82985 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the RGNF
USGS_roads_RGNF <- terra::intersect(USGS_roads_SRME_proj, RGNF_vect)
nrow(USGS_roads_RGNF) # 14132
plot(USGS_roads_RGNF)


### rasterize ----
#### USFS ----
RGNF_USFS_road_rast <- rasterize(USFS_roads_RGNF, RGNF_QMD_filt_rast , touches=TRUE)
plot(RGNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(RGNF_USFS_road_rast)) # values not 1 are NA
global(RGNF_USFS_road_rast, fun = "notNA") # 110111 cells not NA

#### USGS ----
RGNF_USGS_road_rast <- rasterize(USGS_roads_RGNF, RGNF_QMD_filt_rast , touches=TRUE)
plot(RGNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(RGNF_USGS_road_rast)) # values not 1 are NA
global(RGNF_USGS_road_rast, fun = "notNA") # 242582 cells not NA


### combine ----
RGNF_road_rast <- cover(RGNF_USFS_road_rast, RGNF_USGS_road_rast)
plot(RGNF_road_rast)
plot(is.na(RGNF_road_rast))
global(RGNF_road_rast, fun = "notNA") # 258315 cells not NA

##### write & read ----
writeRaster(RGNF_road_rast, "RGNF_road_rast.tif")
RGNF_road_rast <- rast("RGNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
RGNF_road_dist_rast <- distance(RGNF_road_rast) 
plot(RGNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(RGNF_road_dist_rast, "RGNF_road_dist_rast.tif")
RGNF_road_dist_rast <- rast("RGNF_road_dist_rast.tif")


### filter ----
minmax(RGNF_road_dist_rast) 
# min = 0, max = 78433.95 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
RGNF_road_filt_rast <- ifel(RGNF_road_dist_rast > 917.3261, NA, 500)
plot(RGNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the RGNF
RGNF_road_filt_rast = crop(RGNF_road_filt_rast, RGNF_vect, mask = TRUE)


### viz ----
plot(RGNF_road_filt_rast, col = "darkorchid2")
polys(RGNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(RGNF_road_filt_rast, "RGNF_road_filt_rast.tif")
RGNF_road_filt_rast <- rast("RGNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(RGNF_slope_filt_rast, RGNF_EVH_filt_rast, method = "near")

raster_list <- list(RGNF_EVH_filt_rast,
                    RGNF_QMD_filt_rast,
                    RGNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
RGNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(RGNF_combined_rast)
polys(RGNF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(RGNF_combined_rast))


## stats ----
# we want to know what % of the RGNF each priority factor (PF) & combo occupies
# need a total # cells in the RGNF to compare
global(RGNF_DEM_rast, fun = "notNA") # 9199894 cells (covers all RGNF)
# but not same resolution as rest of data
DEM_resampled <- resample(RGNF_DEM_rast, RGNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 7773990 cells (covers all RGNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(RGNF_QMD_rast, fun = "notNA") # 5697616 cells
(5697616/7773990)*100 # 73.29076 % of RGNF has QMD values

# areas with QMD > 5 inches
global(RGNF_QMD_filt_rast, fun = "notNA") # 4160703
(4160703/7773990)*100 # 53.52082 % of RGNF has trees > 5 in QMD

#### EVH ----
# all veg area
global(EVH_RGNF >= 101, fun = "sum", na.rm = TRUE) # 7001131 cells
(7001131/7773990)*100 # 90.0584 % of RGNF is vegetated 

# all tree area
global(RGNF_EVH_rast, fun = "notNA") # 5311714
(5311714/7773990)*100 # 68.32674 % of RGNF has trees 

# trees > 10 ft area
global(RGNF_EVH_filt_rast, fun = "notNA") # 5231674
(5231674/7773990)*100 # 67.29715 % of RGNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 6282487 cells 
(6282487/7773990)*100 # 80.81419 % remaining after 24* filter

#### road ----
global(RGNF_road_filt_rast, fun = "notNA") # 5316596 cells
# entire RGNF = 7773990 cells
(5316596/7773990)*100 # 68.38954 % remaining


### combined PFs ----
# we want to know what % of the RGNF each category falls into after combining

# value 5, QMD only
global(RGNF_combined_rast == 5, fun = "sum", na.rm = TRUE) # 24212 cells
(24212/7773990)*100 # 0.3114488 % of RGNF

# value 10, EVH only
global(RGNF_combined_rast == 10, fun = "sum", na.rm = TRUE) # 86352 cells
(86352/7773990)*100 # 1.110781 % of RGNF

# value 15, QMD + EVH
global(RGNF_combined_rast == 15, fun = "sum", na.rm = TRUE) # 176429 cells
(176429/7773990)*100 # 2.269478 % of RGNF

# value 100, slope only
global(RGNF_combined_rast == 100, fun = "sum", na.rm = TRUE) # 589281 cells
(589281/7773990)*100 # 7.580162 % of RGNF

# value 105, slope + QMD
global(RGNF_combined_rast == 105, fun = "sum", na.rm = TRUE) # 84525 cells
(84525/7773990)*100 # 1.08728 % of RGNF

# value 110, slope + EVH
global(RGNF_combined_rast == 110, fun = "sum", na.rm = TRUE) # 391693 cells
(391693/7773990)*100 # 5.038507 % of RGNF

# value 115, slope + EVH + QMD
global(RGNF_combined_rast == 115, fun = "sum", na.rm = TRUE) # 790573 cells
(790573/7773990)*100 # 10.16946 % of RGNF

# value 500, road only
global(RGNF_combined_rast == 500, fun = "sum", na.rm = TRUE) # 215441 cells
(215441/7773990)*100 # 2.771305 % of RGNF

# value 505, road + QMD
global(RGNF_combined_rast == 505, fun = "sum", na.rm = TRUE) # 52812 cells
(52812/7773990)*100 # 0.6793423 % of RGNF

# value 510, road + EVH
global(RGNF_combined_rast == 510, fun = "sum", na.rm = TRUE) # 169563 cells
(169563/7773990)*100 # 2.181158 % of RGNF

# value 515, road + EVH + QMD
global(RGNF_combined_rast == 515, fun = "sum", na.rm = TRUE) # 452365 cells
(452365/7773990)*100 # 5.818955 % of RGNF

# value 600, road + slope
global(RGNF_combined_rast == 600, fun = "sum", na.rm = TRUE) # 1002984 cells
(1002984/7773990)*100 # 12.90179 % of RGNF

# value 605, road + slope + QMD
global(RGNF_combined_rast == 605, fun = "sum", na.rm = TRUE) # 258732 cells
(258732/7773990)*100 # 3.328175 % of RGNF

# value 610, road + slope + EVH
global(RGNF_combined_rast == 610, fun = "sum", na.rm = TRUE) # 843644 cells
(843644/7773990)*100 # 10.85214 % of RGNF

# value 615, road + slope + QMD + EVH
global(RGNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2321055 cells
(2321055/7773990)*100 # 29.85668 % of RGNF

# value notNA
global(RGNF_combined_rast, fun = "notNA") # 7459661 cells
(7459661/7773990)*100 # 95.95666 % of RGNF (equals the sum of above %s)
100-95.95666 # 4.04334 % is NA (QMD < 5in, EVH < 10ft, slope >24, road >0.57)


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
RGNF_priority_rast <- ifel(
  RGNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(RGNF_priority_rast, fun = "notNA") # 2321055 cells (same as value=615 above)
(2321055/7773990)*100 # 29.85668 % of RGNF


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(RGNF_priority_rast, transform = FALSE) # 2088949500 m^2
2088949500/4046.86 # 4046.86 m2/acre = 516190.2 acres
# entire RGNF = 1723619 acres (calculated from RGNF_vect polygon in Part1A_2)
(516190.2/1723619)*100 # 29.94805 % of RGNF (almost same as value=615 above)

## viz ----
plot(RGNF_priority_rast, col = "goldenrod1")
polys(RGNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(RGNF_priority_rast, "RGNF_priority_rast.tif")
RGNF_priority_rast <- rast("RGNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(RGNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
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
RGNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4106 geoms


## adjust ----
# add new ID col & new final area col
RGNF_PCUs_1A_vect$PCU_ID <- 1:nrow(RGNF_PCUs_1A_vect)
RGNF_PCUs_1A_vect$area_acres <- expanse(RGNF_PCUs_1A_vect) * 0.000247105

summary(RGNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 265.60  
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
RGNF_PCUs_1A_vect <- RGNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

RGNF_PCUs_1A_df <- as.data.frame(RGNF_PCUs_1A_vect)

sum(RGNF_PCUs_1A_vect$area_acres) # 422214.1 acres
sum(small_polys_removed$patch_acres) # 422214.1 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# RGNF is 1723619 acres 
(422214.1/1723619)*100 # 24.49579 % of RGNF are highest priority areas (PCUs)

# for reference, 
(506182.4/1723619)*100 # 29.36742 % of RGNF meets PFs (RGNF_priority_rast values = 1)

(422214.1/506182.4)*100 # 83.41145 % of the areas that meet basic priorities
# are continuous PCUs > 20 acres


## viz ----
plot(RGNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(RGNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(RGNF_PCUs_1A_vect, "RGNF_PCUs_1A_vect.shp")
RGNF_PCUs_1A_vect <- vect("RGNF_PCUs_1A_vect.shp")


