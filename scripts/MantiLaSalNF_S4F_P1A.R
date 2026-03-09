# Manti-La Sal National Forest

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

# select for just MLSNF 
MLSNF_full <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Manti-La Sal National Forest")
plot(MLSNF_full)

# project 
MLSNF_full <- project(MLSNF_full,"EPSG:5070")

# calc area
expanse(MLSNF_full) # 5723820874 m^2
5723820874/4046.86 # 4046.86 m/acre = 1414386 acres

# divide into just 2 parts
# fortunately, this makes an even split between the northern and southern parts of the NF
MLSNF_divided <- divide(MLSNF_full, n = 2)
MLSNF_divided$ID <- 1:nrow(MLSNF_divided)

# filter for just the southern half (which is within the SRME)
MLSNF_vect <- MLSNF_divided %>% 
  filter(ID == "2")
plot(MLSNF_vect)

# calc area
expanse(MLSNF_vect) # 2198348868 m^2
2198348868/4046.86 # 4046.86 m/acre = 543223.4 acres

## write & read ----
writeVector(MLSNF_vect, "MLSNF_vect.shp")
MLSNF_vect <- vect("MLSNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
MLSNF_QMD_rast <- crop(QMD_CONUS, MLSNF_vect, mask=TRUE)
plot(MLSNF_QMD_rast)

#### write & read ----
writeRaster(MLSNF_QMD_rast, "MLSNF_QMD_rast.tif")
MLSNF_QMD_rast <- rast("MLSNF_QMD_rast.tif")

global(MLSNF_QMD_rast, fun = "notNA") # 1975822 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

MLSNF_QMD_filt_rast <- ifel(
  MLSNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(MLSNF_QMD_filt_rast, col = "darkgreen")
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(MLSNF_QMD_filt_rast, "MLSNF_QMD_filt_rast.tif")
MLSNF_QMD_filt_rast <- rast("MLSNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_MLSNF <- crop(EVH_CONUS, MLSNF_vect, mask=TRUE)
plot(EVH_MLSNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

MLSNF_EVH_rast <- ifel(
  EVH_MLSNF >= 101 & EVH_MLSNF < 200,
  (EVH_MLSNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(MLSNF_EVH_rast)
global(MLSNF_EVH_rast, fun = "notNA") # 1930559
summary(MLSNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(MLSNF_EVH_rast, "MLSNF_EVH_rast.tif")
MLSNF_EVH_rast <- rast("MLSNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

MLSNF_EVH_filt_rast <- ifel(
  MLSNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(MLSNF_EVH_filt_rast, col = "forestgreen")
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(MLSNF_EVH_filt_rast, "MLSNF_EVH_filt_rast.tif")
MLSNF_EVH_filt_rast <- rast("MLSNF_EVH_filt_rast.tif")



## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n38_w110 <- rast("USGS_1_n38w110_20241031.tif")
DEM_n39_w109 <- rast("USGS_1_n39w109_20211208.tif")
DEM_n39_w110 <- rast("USGS_1_n39w110_20241031.tif")

# mosaic 4 tiles together
MLSNF_DEM <- mosaic(DEM_n38_w110, DEM_n39_w109, DEM_n39_w110, fun = "first")

crs(MLSNF_DEM) # EPSG: 4269
res(MLSNF_DEM) # 0.0002777778

# project
MLSNF_DEM <- project(MLSNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of MLSNF 
MLSNF_DEM_rast <- crop(MLSNF_DEM, MLSNF_vect, mask=TRUE)
plot(MLSNF_DEM_rast) # min = 1228.455 , max = 3871.930  (meters)
plot(is.na(MLSNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(MLSNF_DEM_rast, "MLSNF_DEM_rast.tif")
MLSNF_DEM_rast <- rast("MLSNF_DEM_rast.tif")

### calc slope ----
MLSNF_slope_rast = terrain(MLSNF_DEM_rast, v="slope", unit="degrees")
plot(MLSNF_slope_rast)

#### write & read ----
writeRaster(MLSNF_slope_rast, "MLSNF_slope_rast.tif")
MLSNF_slope_rast <- rast("MLSNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

MLSNF_slope_filt_rast <- ifel(MLSNF_slope_rast > 24, NA, 100)

### viz ----
plot(MLSNF_slope_filt_rast, col = "mediumorchid2")
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(MLSNF_slope_filt_rast, "MLSNF_slope_filt_rast.tif")
MLSNF_slope_filt_rast <- rast("MLSNF_slope_filt_rast.tif")


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

# get just roads in the MLSNF
USFS_roads_MLSNF <- terra::intersect(USFS_roads_SRME_projected, MLSNF_vect)
nrow(USFS_roads_MLSNF) # 904

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_MLSNF)
unique(USFS_roads_MLSNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_MLSNF <- USFS_roads_MLSNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_MLSNF) # 564
plot(USFS_roads_MLSNF)
(564/904)* 100 # = 62.38938 % of FS roads retained
100 - 62.38938 # = 37.61062 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_Clip.shp")
crs(USGS_roads_SRME) # EPSG: 4269
nrow(USGS_roads_SRME) # 132307

# project
USGS_roads_SRME_proj <- project(USGS_roads_SRME, "EPSG: 5070")

# get just roads in the MLSNF
USGS_roads_MLSNF <- terra::intersect(USGS_roads_SRME_proj, MLSNF_vect)
nrow(USGS_roads_MLSNF) # 2640
plot(USGS_roads_MLSNF)


### rasterize ----
#### USFS ----
MLSNF_USFS_road_rast <- rasterize(USFS_roads_MLSNF, MLSNF_QMD_filt_rast , touches=TRUE)
plot(MLSNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(MLSNF_USFS_road_rast)) # values not 1 are NA
global(MLSNF_USFS_road_rast, fun = "notNA") # 53821 cells not NA

#### USGS ----
MLSNF_USGS_road_rast <- rasterize(USGS_roads_MLSNF, MLSNF_QMD_filt_rast , touches=TRUE)
plot(MLSNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(MLSNF_USGS_road_rast)) # values not 1 are NA
global(MLSNF_USGS_road_rast, fun = "notNA") # 65918 cells not NA


### combine ----
MLSNF_road_rast <- cover(MLSNF_USFS_road_rast, MLSNF_USGS_road_rast)
plot(MLSNF_road_rast)
plot(is.na(MLSNF_road_rast))
global(MLSNF_road_rast, fun = "notNA") # 75120 cells not NA

##### write & read ----
writeRaster(MLSNF_road_rast, "MLSNF_road_rast.tif")
MLSNF_road_rast <- rast("MLSNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
MLSNF_road_dist_rast <- distance(MLSNF_road_rast) 
plot(MLSNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(MLSNF_road_dist_rast, "MLSNF_road_dist_rast.tif")
MLSNF_road_dist_rast <- rast("MLSNF_road_dist_rast.tif")


### filter ----
minmax(MLSNF_road_dist_rast) 
# min = 0, max = 55664.29 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
MLSNF_road_filt_rast <- ifel(MLSNF_road_dist_rast > 917.3261, NA, 500)
plot(MLSNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the MLSNF
MLSNF_road_filt_rast = crop(MLSNF_road_filt_rast, MLSNF_vect, mask = TRUE)


### viz ----
plot(MLSNF_road_filt_rast, col = "darkorchid2")
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(MLSNF_road_filt_rast, "MLSNF_road_filt_rast.tif")
MLSNF_road_filt_rast <- rast("MLSNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(MLSNF_slope_filt_rast, MLSNF_EVH_filt_rast, method = "near")

raster_list <- list(MLSNF_EVH_filt_rast,
                    MLSNF_QMD_filt_rast,
                    MLSNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
MLSNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(MLSNF_combined_rast)
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(MLSNF_combined_rast))


## stats ----
# we want to know what % of the MLSNF each priority factor (PF) & combo occupies
# need a total # cells in the MLSNF to compare
global(MLSNF_DEM_rast, fun = "notNA") # 2834592 cells (covers all MLSNF)
# but not same resolution as rest of data
DEM_resampled <- resample(MLSNF_DEM_rast, MLSNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 2445368 cells (covers all MLSNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(MLSNF_QMD_rast, fun = "notNA") # 1975822 cells
(1975822/2445368)*100 # 80.79855 % of MLSNF has QMD values

# areas with QMD > 5 inches
global(MLSNF_QMD_filt_rast, fun = "notNA") # 1675390
(1675390/2445368)*100 # 68.5128 % of MLSNF has trees > 5 in QMD

#### EVH ----
# all tree area
global(MLSNF_EVH_rast, fun = "notNA") # 1930559
(1930559/2445368)*100 # 78.94759 % of MLSNF has trees 

# trees > 10 ft area
global(MLSNF_EVH_filt_rast, fun = "notNA") # 1827104
(1827104/2445368)*100 # 74.71693 % of MLSNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 1849350 cells 
(1849350/2445368)*100 # 75.62665 % remaining after 24* filter

#### road ----
global(MLSNF_road_filt_rast, fun = "notNA") # 1662751 cells
(1662751/2445368)*100 # 67.99594 % remaining


### combined PFs ----
# we want to know what % of the MLSNF meets all of the priority factor thresholds, after combining

# value 615 = road + slope + QMD + EVH
global(MLSNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 913422 cells
(913422/2445368)*100 # 37.35315 % of MLSNF


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
MLSNF_priority_rast <- ifel(
  MLSNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(MLSNF_priority_rast, fun = "notNA") # 913422 cells (same as value=615 above)


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(MLSNF_priority_rast, transform = FALSE) # 822079800 m^2
822079800/4046.86 # 4046.86 m2/acre = 203140.2 acres
# entire MLSNF = 543223.4 acres (calculated from MLSNF_vect polygon in Part1A_2)
(203140.2/543223.4)*100 # 37.39533 % of MLSNF (almost same as value=615 above)

## viz ----
plot(MLSNF_priority_rast, col = "goldenrod1")
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(MLSNF_priority_rast, "MLSNF_priority_rast.tif")
MLSNF_priority_rast <- rast("MLSNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(MLSNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 21431 patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 21431 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 334 geoms remain
(334/21431)*100 # 1.55849 % of polys remain (are >= 20 acres)
# so ~98 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 259 geoms
(259/334)*100 # 77.54491 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 75 geoms
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
# 1370 geoms

# combine the mid-sized polys with the newly divided large polys
MLSNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 1629 geoms


## adjust ----
# add new ID col & new final area col
MLSNF_PCUs_1A_vect$PCU_ID <- paste0("MLSNF_PCU_", seq_len(nrow(MLSNF_PCUs_1A_vect)))
MLSNF_PCUs_1A_vect$area_acres <- expanse(MLSNF_PCUs_1A_vect) * 0.000247105

summary(MLSNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 306.38    
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
MLSNF_PCUs_1A_vect <- MLSNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

MLSNF_PCUs_1A_df <- as.data.frame(MLSNF_PCUs_1A_vect)

sum(MLSNF_PCUs_1A_vect$area_acres) # 185620.3 acres
sum(small_polys_removed$patch_acres) # 185620.3 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# MLSNF is 543223.4 acres 
(185620.3/543223.4)*100 # 34.17016 % of MLSNF are highest priority areas (PCUs)



## viz ----
plot(MLSNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(MLSNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(MLSNF_PCUs_1A_vect, "MLSNF_PCUs_1A_vect.shp")
MLSNF_PCUs_1A_vect <- vect("MLSNF_PCUs_1A_vect.shp")


