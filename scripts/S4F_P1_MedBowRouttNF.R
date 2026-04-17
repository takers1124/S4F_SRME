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
writeVector(MBRNF_vect, "./MedBowRouttNF_S4F/.shp/MBRNF_vect.shp")
MBRNF_vect <- vect("./MedBowRouttNF_S4F/.shp/MBRNF_vect.shp")


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
writeRaster(MBRNF_QMD_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_QMD_rast.tif")
MBRNF_QMD_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_QMD_rast.tif")

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
writeRaster(MBRNF_QMD_filt_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_QMD_filt_rast.tif")
MBRNF_QMD_filt_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_QMD_filt_rast.tif")


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
writeRaster(MBRNF_EVH_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_EVH_rast.tif")
MBRNF_EVH_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_EVH_rast.tif")

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
writeRaster(MBRNF_EVH_filt_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_EVH_filt_rast.tif")
MBRNF_EVH_filt_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_EVH_filt_rast.tif")


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
writeRaster(MBRNF_DEM_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_DEM_rast.tif")
MBRNF_DEM_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_DEM_rast.tif")

### calc slope ----
MBRNF_slope_rast = terrain(MBRNF_DEM_rast, v="slope", unit="degrees")
plot(MBRNF_slope_rast)

#### write & read ----
writeRaster(MBRNF_slope_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_slope_rast.tif")
MBRNF_slope_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

MBRNF_slope_filt_rast <- ifel(MBRNF_slope_rast > 24, NA, 100)

### viz ----
plot(MBRNF_slope_filt_rast, col = "mediumorchid2")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(MBRNF_slope_filt_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_slope_filt_rast.tif")
MBRNF_slope_filt_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_slope_filt_rast.tif")


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
MBRNF_buffer1km <- buffer(MBRNF_vect, width = 1000)

# get just roads in the MBRNF + buffer
USFS_roads_MBRNF <- terra::intersect(USFS_roads_SRME, MBRNF_buffer1km)
nrow(USFS_roads_MBRNF) # 3326

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

nrow(USFS_roads_MBRNF) # 2024
plot(USFS_roads_MBRNF)
(2024/3326)* 100 # = 60.85388 % of FS roads retained
100 - 60.85388 # = 39.14612 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_USGS_Clip_SRME.shp")
crs(USGS_roads_SRME) # EPSG: 5070
nrow(USGS_roads_SRME) # 312312

# get just roads in the MBRNF
USGS_roads_MBRNF <- terra::intersect(USGS_roads_SRME, MBRNF_buffer1km)
nrow(USGS_roads_MBRNF) # 19410
plot(USGS_roads_MBRNF)


### rasterize ----
#### USFS ----
MBRNF_USFS_road_rast <- rasterize(USFS_roads_MBRNF, MBRNF_QMD_rast , touches=TRUE)
plot(MBRNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(MBRNF_USFS_road_rast)) # values not 1 are NA
global(MBRNF_USFS_road_rast, fun = "notNA") # 185999 cells not NA

#### USGS ----
MBRNF_USGS_road_rast <- rasterize(USGS_roads_MBRNF, MBRNF_QMD_rast , touches=TRUE)
plot(MBRNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(MBRNF_USGS_road_rast)) # values not 1 are NA
global(MBRNF_USGS_road_rast, fun = "notNA") # 423230 cells not NA


### combine ----
MBRNF_road_rast <- cover(MBRNF_USFS_road_rast, MBRNF_USGS_road_rast)
plot(MBRNF_road_rast)
plot(is.na(MBRNF_road_rast))
global(MBRNF_road_rast, fun = "notNA") # 451066 cells not NA

##### write & read ----
writeRaster(MBRNF_road_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_road_rast.tif")
MBRNF_road_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
MBRNF_road_dist_rast <- distance(MBRNF_road_rast) 
plot(MBRNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(MBRNF_road_dist_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_road_dist_rast.tif")
MBRNF_road_dist_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_road_dist_rast.tif")


### filter ----
minmax(MBRNF_road_dist_rast) 
# min = 0, max = 142655.5 
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
writeRaster(MBRNF_road_filt_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_road_filt_rast.tif")
MBRNF_road_filt_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_road_filt_rast.tif")



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
global(MBRNF_DEM_rast, fun = "notNA") # 13858177 cells (covers all MBRNF)
# but not same resolution as rest of data
DEM_resampled <- resample(MBRNF_DEM_rast, MBRNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 12504670 cells (covers all MBRNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(MBRNF_QMD_rast, fun = "notNA") # 9678364 cells
(9678364/12504670)*100 # 77.398 % of MBRNF has QMD values

# areas with QMD > 5 inches
global(MBRNF_QMD_filt_rast, fun = "notNA") # 6702333
(6702333/12504670)*100 # 53.59864 % of MBRNF has trees > 5 in QMD

#### EVH ----
# all tree area
global(MBRNF_EVH_rast, fun = "notNA") # 9148382
(9148382/12504670)*100 # 73.15972 % of MBRNF has trees 

# trees > 10 ft area
global(MBRNF_EVH_filt_rast, fun = "notNA") # 9068667
(9068667/12504670)*100 # 72.52224 % of MBRNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 11391032 cells 
(11391032/12504670)*100 # 91.09422 % remaining after 24* filter

#### road ----
global(MBRNF_road_filt_rast, fun = "notNA") # 8100511 cells
(8100511/12504670)*100 # 64.77989 % remaining


### combined PFs ----
# we want to know what % of the MBRNF meets all of the priority factor thresholds, after combining

# value 615 = road + slope + QMD + EVH
global(MBRNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 3575651 cells
(3575651/12504670)*100 # 28.59453 % of MBRNF


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
MBRNF_priority_rast <- ifel(
  MBRNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(MBRNF_priority_rast, fun = "notNA") # 3575651 cells (same as value=615 above)


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(MBRNF_priority_rast, transform = FALSE) # 3218085900 m^2
3218085900/4046.86 # 4046.86 m2/acre = 795205.6 acres
# entire MBRNF = 2772764 acres (calculated from MBRNF_vect polygon in Part1A_2)
(795205.6/2772764)*100 # 28.67917 % of MBRNF (almost same as value=615 above)

## viz ----
plot(MBRNF_priority_rast, col = "goldenrod1")
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(MBRNF_priority_rast, "./MedBowRouttNF_S4F/.tif/MBRNF_priority_rast.tif")
MBRNF_priority_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(MBRNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 181889  patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 181889  geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 2279 geoms remain
(2279/179093)*100 # 1.272523 % of polys remain (are >= 20 acres)
# so ~98 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1933 geoms
(1933/2279)*100 # 84.8179 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 346 geoms
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
# 4321 geoms

# combine the mid-sized polys with the newly divided large polys
MBRNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 6254 geoms


## adjust ----
# add new ID col & new final area col
MBRNF_PCUs_1A_vect$PCU_ID <- paste0("MBRNF_PCU_", seq_len(nrow(MBRNF_PCUs_1A_vect)))
MBRNF_PCUs_1A_vect$area_acres <- expanse(MBRNF_PCUs_1A_vect) * 0.000247105

summary(MBRNF_PCUs_1A_vect)
# area_acres min = 19.33, max = 286.48      
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
MBRNF_PCUs_1A_vect <- MBRNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

MBRNF_PCUs_1A_df <- as.data.frame(MBRNF_PCUs_1A_vect)

sum(MBRNF_PCUs_1A_vect$area_acres) # 641451.5 acres
sum(small_polys_removed$patch_acres) # 641451.5 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# MBRNF is 2772764 acres 
(641451.5/2772764)*100 # 23.13401 % of MBRNF are highest priority areas (PCUs)


## viz ----
plot(MBRNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(MBRNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(MBRNF_PCUs_1A_vect, "./MedBowRouttNF_S4F/.shp/MBRNF_PCUs_1A_vect.shp")
MBRNF_PCUs_1A_vect <- vect("./MedBowRouttNF_S4F/.shp/MBRNF_PCUs_1A_vect.shp")


