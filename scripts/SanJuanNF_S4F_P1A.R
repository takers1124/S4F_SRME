# San Juan National Forest

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

# select for just SJNF 
SJNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "San Juan National Forest")
plot(SJNF_vect)

# project 
SJNF_vect <- project(SJNF_vect,"EPSG:5070")

# calc area
expanse(SJNF_vect) # 8476295999 m^2
8476295999/4046.86 # 4046.86 m/acre = 2094537 acres

## write & read ----
writeVector(SJNF_vect, "SJNF_vect.shp")
SJNF_vect <- vect("SJNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
SJNF_QMD_rast <- crop(QMD_CONUS, SJNF_vect, mask=TRUE)
plot(SJNF_QMD_rast)

#### write & read ----
writeRaster(SJNF_QMD_rast, "SJNF_QMD_rast.tif")
SJNF_QMD_rast <- rast("SJNF_QMD_rast.tif")

global(SJNF_QMD_rast, fun = "notNA") # 7155935 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

SJNF_QMD_filt_rast <- ifel(
  SJNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(SJNF_QMD_filt_rast, col = "darkgreen")
polys(SJNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(SJNF_QMD_filt_rast, "SJNF_QMD_filt_rast.tif")
SJNF_QMD_filt_rast <- rast("SJNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_SJNF <- crop(EVH_CONUS, SJNF_vect, mask=TRUE)
plot(EVH_SJNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

SJNF_EVH_rast <- ifel(
  EVH_SJNF >= 101 & EVH_SJNF < 200,
  (EVH_SJNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(SJNF_EVH_rast)
global(SJNF_EVH_rast, fun = "notNA") # 7061136
summary(SJNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(SJNF_EVH_rast, "SJNF_EVH_rast.tif")
SJNF_EVH_rast <- rast("SJNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

SJNF_EVH_filt_rast <- ifel(
  SJNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(SJNF_EVH_filt_rast, col = "forestgreen")
polys(SJNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(SJNF_EVH_filt_rast, "SJNF_EVH_filt_rast.tif")
SJNF_EVH_filt_rast <- rast("SJNF_EVH_filt_rast.tif")



## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n38_w107 <- rast("USGS_1_n38w107_20220720.tif")
DEM_n38_w108 <- rast("USGS_1_n38w108_20220720.tif")
DEM_n38_w109 <- rast("USGS_1_n38w109_20220720.tif")

# mosaic 8 tiles together
SJNF_DEM <- mosaic(DEM_n38_w107, DEM_n38_w108, DEM_n38_w109, fun = "first")

crs(SJNF_DEM) # EPSG: 4269
res(SJNF_DEM) # 0.0002777778

# project
SJNF_DEM <- project(SJNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of SJNF 
SJNF_DEM_rast <- crop(SJNF_DEM, SJNF_vect, mask=TRUE)
plot(SJNF_DEM_rast) # min = 1914.808 , max = 4323.411  (meters)
plot(is.na(SJNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(SJNF_DEM_rast, "SJNF_DEM_rast.tif")
SJNF_DEM_rast <- rast("SJNF_DEM_rast.tif")

### calc slope ----
SJNF_slope_rast = terrain(SJNF_DEM_rast, v="slope", unit="degrees")
plot(SJNF_slope_rast)

#### write & read ----
writeRaster(SJNF_slope_rast, "SJNF_slope_rast.tif")
SJNF_slope_rast <- rast("SJNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

SJNF_slope_filt_rast <- ifel(SJNF_slope_rast > 24, NA, 100)

### viz ----
plot(SJNF_slope_filt_rast, col = "mediumorchid2")
polys(SJNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(SJNF_slope_filt_rast, "SJNF_slope_filt_rast.tif")
SJNF_slope_filt_rast <- rast("SJNF_slope_filt_rast.tif")


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

# get just roads in the SJNF
USFS_roads_SJNF <- terra::intersect(USFS_roads_SRME_projected, SJNF_vect)
nrow(USFS_roads_SJNF) # 1796

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_SJNF)
unique(USFS_roads_SJNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_SJNF <- USFS_roads_SJNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_SJNF) # 936
plot(USFS_roads_SJNF)
(936/1796)* 100 # = 52.11581 % of FS roads retained
100 - 52.11581 # = 47.88419 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the SJNF
USGS_roads_SJNF <- terra::intersect(USGS_roads_SRME_proj, SJNF_vect)
nrow(USGS_roads_SJNF) # 7595
plot(USGS_roads_SJNF)


### rasterize ----
#### USFS ----
SJNF_USFS_road_rast <- rasterize(USFS_roads_SJNF, SJNF_QMD_filt_rast , touches=TRUE)
plot(SJNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(SJNF_USFS_road_rast)) # values not 1 are NA
global(SJNF_USFS_road_rast, fun = "notNA") # 105141 cells not NA

#### USGS ----
SJNF_USGS_road_rast <- rasterize(USGS_roads_SJNF, SJNF_QMD_filt_rast , touches=TRUE)
plot(SJNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(SJNF_USGS_road_rast)) # values not 1 are NA
global(SJNF_USGS_road_rast, fun = "notNA") # 237704 cells not NA


### combine ----
SJNF_road_rast <- cover(SJNF_USFS_road_rast, SJNF_USGS_road_rast)
plot(SJNF_road_rast)
plot(is.na(SJNF_road_rast))
global(SJNF_road_rast, fun = "notNA") # 243807 cells not NA

##### write & read ----
writeRaster(SJNF_road_rast, "SJNF_road_rast.tif")
SJNF_road_rast <- rast("SJNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
SJNF_road_dist_rast <- distance(SJNF_road_rast) 
plot(SJNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(SJNF_road_dist_rast, "SJNF_road_dist_rast.tif")
SJNF_road_dist_rast <- rast("SJNF_road_dist_rast.tif")


### filter ----
minmax(SJNF_road_dist_rast) 
# min = 0, max = 67549.96 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
SJNF_road_filt_rast <- ifel(SJNF_road_dist_rast > 917.3261, NA, 500)
plot(SJNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the SJNF
SJNF_road_filt_rast = crop(SJNF_road_filt_rast, SJNF_vect, mask = TRUE)


### viz ----
plot(SJNF_road_filt_rast, col = "darkorchid2")
polys(SJNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(SJNF_road_filt_rast, "SJNF_road_filt_rast.tif")
SJNF_road_filt_rast <- rast("SJNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(SJNF_slope_filt_rast, SJNF_EVH_filt_rast, method = "near")

raster_list <- list(SJNF_EVH_filt_rast,
                    SJNF_QMD_filt_rast,
                    SJNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
SJNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(SJNF_combined_rast)
polys(SJNF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(SJNF_combined_rast))


## stats ----
# we want to know what % of the SJNF each priority factor (PF) & combo occupies
# need a total # cells in the SJNF to compare
global(SJNF_DEM_rast, fun = "notNA") # 9199894 cells (covers all SJNF)
# but not same resolution as rest of data
DEM_resampled <- resample(SJNF_DEM_rast, SJNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 7773990 cells (covers all SJNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(SJNF_QMD_rast, fun = "notNA") # 5697616 cells
(5697616/7773990)*100 # 73.29076 % of SJNF has QMD values

# areas with QMD > 5 inches
global(SJNF_QMD_filt_rast, fun = "notNA") # 4160703
(4160703/7773990)*100 # 53.52082 % of SJNF has trees > 5 in QMD

#### EVH ----
# all veg area
global(EVH_SJNF >= 101, fun = "sum", na.rm = TRUE) # 7001131 cells
(7001131/7773990)*100 # 90.0584 % of SJNF is vegetated 

# all tree area
global(SJNF_EVH_rast, fun = "notNA") # 5311714
(5311714/7773990)*100 # 68.32674 % of SJNF has trees 

# trees > 10 ft area
global(SJNF_EVH_filt_rast, fun = "notNA") # 5231674
(5231674/7773990)*100 # 67.29715 % of SJNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 6282487 cells 
(6282487/7773990)*100 # 80.81419 % remaining after 24* filter

#### road ----
global(SJNF_road_filt_rast, fun = "notNA") # 5316596 cells
# entire SJNF = 7773990 cells
(5316596/7773990)*100 # 68.38954 % remaining


### combined PFs ----
# we want to know what % of the SJNF each category falls into after combining

# value 5, QMD only
global(SJNF_combined_rast == 5, fun = "sum", na.rm = TRUE) # 24212 cells
(24212/7773990)*100 # 0.3114488 % of SJNF

# value 10, EVH only
global(SJNF_combined_rast == 10, fun = "sum", na.rm = TRUE) # 86352 cells
(86352/7773990)*100 # 1.110781 % of SJNF

# value 15, QMD + EVH
global(SJNF_combined_rast == 15, fun = "sum", na.rm = TRUE) # 176429 cells
(176429/7773990)*100 # 2.269478 % of SJNF

# value 100, slope only
global(SJNF_combined_rast == 100, fun = "sum", na.rm = TRUE) # 589281 cells
(589281/7773990)*100 # 7.580162 % of SJNF

# value 105, slope + QMD
global(SJNF_combined_rast == 105, fun = "sum", na.rm = TRUE) # 84525 cells
(84525/7773990)*100 # 1.08728 % of SJNF

# value 110, slope + EVH
global(SJNF_combined_rast == 110, fun = "sum", na.rm = TRUE) # 391693 cells
(391693/7773990)*100 # 5.038507 % of SJNF

# value 115, slope + EVH + QMD
global(SJNF_combined_rast == 115, fun = "sum", na.rm = TRUE) # 790573 cells
(790573/7773990)*100 # 10.16946 % of SJNF

# value 500, road only
global(SJNF_combined_rast == 500, fun = "sum", na.rm = TRUE) # 215441 cells
(215441/7773990)*100 # 2.771305 % of SJNF

# value 505, road + QMD
global(SJNF_combined_rast == 505, fun = "sum", na.rm = TRUE) # 52812 cells
(52812/7773990)*100 # 0.6793423 % of SJNF

# value 510, road + EVH
global(SJNF_combined_rast == 510, fun = "sum", na.rm = TRUE) # 169563 cells
(169563/7773990)*100 # 2.181158 % of SJNF

# value 515, road + EVH + QMD
global(SJNF_combined_rast == 515, fun = "sum", na.rm = TRUE) # 452365 cells
(452365/7773990)*100 # 5.818955 % of SJNF

# value 600, road + slope
global(SJNF_combined_rast == 600, fun = "sum", na.rm = TRUE) # 1002984 cells
(1002984/7773990)*100 # 12.90179 % of SJNF

# value 605, road + slope + QMD
global(SJNF_combined_rast == 605, fun = "sum", na.rm = TRUE) # 258732 cells
(258732/7773990)*100 # 3.328175 % of SJNF

# value 610, road + slope + EVH
global(SJNF_combined_rast == 610, fun = "sum", na.rm = TRUE) # 843644 cells
(843644/7773990)*100 # 10.85214 % of SJNF

# value 615, road + slope + QMD + EVH
global(SJNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2321055 cells
(2321055/7773990)*100 # 29.85668 % of SJNF

# value notNA
global(SJNF_combined_rast, fun = "notNA") # 7459661 cells
(7459661/7773990)*100 # 95.95666 % of SJNF (equals the sum of above %s)
100-95.95666 # 4.04334 % is NA (QMD < 5in, EVH < 10ft, slope >24, road >0.57)


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
SJNF_priority_rast <- ifel(
  SJNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(SJNF_priority_rast, fun = "notNA") # 2321055 cells (same as value=615 above)
(2321055/7773990)*100 # 29.85668 % of SJNF


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(SJNF_priority_rast, transform = FALSE) # 2088949500 m^2
2088949500/4046.86 # 4046.86 m2/acre = 516190.2 acres
# entire SJNF = 1723619 acres (calculated from SJNF_vect polygon in Part1A_2)
(516190.2/1723619)*100 # 29.94805 % of SJNF (almost same as value=615 above)

## viz ----
plot(SJNF_priority_rast, col = "goldenrod1")
polys(SJNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(SJNF_priority_rast, "SJNF_priority_rast.tif")
SJNF_priority_rast <- rast("SJNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(SJNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
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
SJNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4106 geoms


## adjust ----
# add new ID col & new final area col
SJNF_PCUs_1A_vect$PCU_ID <- 1:nrow(SJNF_PCUs_1A_vect)
SJNF_PCUs_1A_vect$area_acres <- expanse(SJNF_PCUs_1A_vect) * 0.000247105

summary(SJNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 265.60  
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
SJNF_PCUs_1A_vect <- SJNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

SJNF_PCUs_1A_df <- as.data.frame(SJNF_PCUs_1A_vect)

sum(SJNF_PCUs_1A_vect$area_acres) # 422214.1 acres
sum(small_polys_removed$patch_acres) # 422214.1 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# SJNF is 1723619 acres 
(422214.1/1723619)*100 # 24.49579 % of SJNF are highest priority areas (PCUs)

# for reference, 
(506182.4/1723619)*100 # 29.36742 % of SJNF meets PFs (SJNF_priority_rast values = 1)

(422214.1/506182.4)*100 # 83.41145 % of the areas that meet basic priorities
# are continuous PCUs > 20 acres


## viz ----
plot(SJNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(SJNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(SJNF_PCUs_1A_vect, "SJNF_PCUs_1A_vect.shp")
SJNF_PCUs_1A_vect <- vect("SJNF_PCUs_1A_vect.shp")


