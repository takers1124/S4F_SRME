# Santa Fe National Forest

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

# select for just SFNF 
SFNF_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Santa Fe National Forest")
plot(SFNF_vect)

# project 
SFNF_vect <- project(SFNF_vect,"EPSG:5070")

# calc area
expanse(SFNF_vect) # 6806025928 m^2
6806025928/4046.86 # 4046.86 m/acre = 1681804 acres

## write & read ----
writeVector(SFNF_vect, "./SantaFeNF_S4F/.shp/SFNF_vect.shp")
SFNF_vect <- vect("./SantaFeNF_S4F/.shp/SFNF_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
crs(QMD_CONUS) # EPSG: 5070
res(QMD_CONUS) # 30

### crop and mask ----
SFNF_QMD_rast <- crop(QMD_CONUS, SFNF_vect, mask=TRUE)
plot(SFNF_QMD_rast)

#### write & read ----
writeRaster(SFNF_QMD_rast, "./SantaFeNF_S4F/.tif/SFNF_QMD_rast.tif")
SFNF_QMD_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_QMD_rast.tif")

global(SFNF_QMD_rast, fun = "notNA") # 6090165 cells

### filter ----
# we only want locations with QMD over 5 inches
# make binary values, if > 5 then make 5, else make NA

SFNF_QMD_filt_rast <- ifel(
  SFNF_QMD_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(SFNF_QMD_filt_rast, col = "darkgreen")
polys(SFNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(SFNF_QMD_filt_rast, "./SantaFeNF_S4F/.tif/SFNF_QMD_filt_rast.tif")
SFNF_QMD_filt_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_QMD_filt_rast.tif")


## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
# these values are not continuous
# also the veg height has an offset added
# e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # EPSG: 5070
res(EVH_CONUS) # 30

### crop / mask ----
EVH_SFNF <- crop(EVH_CONUS, SFNF_vect, mask=TRUE)
plot(EVH_SFNF)

### all treed area ----
# EVH value = 101 = tree height 1 meter
# EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

SFNF_EVH_rast <- ifel(
  EVH_SFNF >= 101 & EVH_SFNF < 200,
  (EVH_SFNF - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

plot(SFNF_EVH_rast)
global(SFNF_EVH_rast, fun = "notNA") # 5943311
summary(SFNF_EVH_rast) # min = 3.281, max = 82.021

#### write & read ----
writeRaster(SFNF_EVH_rast, "./SantaFeNF_S4F/.tif/SFNF_EVH_rast.tif")
SFNF_EVH_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_EVH_rast.tif")

### filter ----
# we only want locations with EVH over 10 feet
# make binary values, if > 10 then make 10, else make NA

SFNF_EVH_filt_rast <- ifel(
  SFNF_EVH_rast >= 10, 
  10, # if at least 10 ft tall, make value = 10
  NA) # if not, make NA

### viz ----
plot(SFNF_EVH_filt_rast, col = "forestgreen")
polys(SFNF_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(SFNF_EVH_filt_rast, "./SantaFeNF_S4F/.tif/SFNF_EVH_filt_rast.tif")
SFNF_EVH_filt_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_EVH_filt_rast.tif")


## slope ----
# this slope raster is generated using  
# digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
# they are 1 Arc Sec

### load & process DEMs ----
DEM_n37_w106 <- rast("USGS_1_n37w106_20250311.tif")
DEM_n37_w107 <- rast("USGS_1_n37w107_20220801.tif")
DEM_n37_w108 <- rast("USGS_1_n37w108_20220801.tif")
DEM_n36_w106 <- rast("USGS_1_n36w106_20241210.tif")
DEM_n36_w107 <- rast("USGS_1_n36w107_20240416.tif")

# mosaic 4 tiles together
SFNF_DEM <- mosaic(DEM_n37_w106, DEM_n37_w107, DEM_n37_w108, 
                   DEM_n36_w106, DEM_n36_w107,
                   fun = "first")

crs(SFNF_DEM) # EPSG: 4269
res(SFNF_DEM) # 0.0002777778

# project
SFNF_DEM <- project(SFNF_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of SFNF 
SFNF_DEM_rast <- crop(SFNF_DEM, SFNF_vect, mask=TRUE)
plot(SFNF_DEM_rast) # min = 1630.083 , max = 3986.089  (meters)
plot(is.na(SFNF_DEM_rast)) # covers the entire AOI, will use for stats (see step 4)

#### write & read ----
writeRaster(SFNF_DEM_rast, "./SantaFeNF_S4F/.tif/SFNF_DEM_rast.tif")
SFNF_DEM_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_DEM_rast.tif")

### calc slope ----
SFNF_slope_rast = terrain(SFNF_DEM_rast, v="slope", unit="degrees")
plot(SFNF_slope_rast)

#### write & read ----
writeRaster(SFNF_slope_rast, "./SantaFeNF_S4F/.tif/SFNF_slope_rast.tif")
SFNF_slope_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_slope_rast.tif")

### filter ----
# we only want locations with slope under 24 degrese
# make binary values, if > 24 then make NA, else make 100

SFNF_slope_filt_rast <- ifel(SFNF_slope_rast > 24, NA, 100)

### viz ----
plot(SFNF_slope_filt_rast, col = "mediumorchid2")
polys(SFNF_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeRaster(SFNF_slope_filt_rast, "./SantaFeNF_S4F/.tif/SFNF_slope_filt_rast.tif")
SFNF_slope_filt_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_slope_filt_rast.tif")


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
SFNF_buffer1km <- buffer(SFNF_vect, width = 1000)

# get just roads in the SFNF + buffer
USFS_roads_SFNF <- terra::intersect(USFS_roads_SRME, SFNF_buffer1km)
nrow(USFS_roads_SFNF) # 8539

# filter for specific operational maintenance levels
# see unique names 
names(USFS_roads_SFNF)
unique(USFS_roads_SFNF$OPER_MAINT)

# select for just levels 2-5 
USFS_roads_SFNF <- USFS_roads_SFNF %>%
  filter(OPER_MAINT %in% c(
    "2 - HIGH CLEARANCE VEHICLES",
    "3 - SUITABLE FOR PASSENGER CARS",
    "4 - MODERATE DEGREE OF USER COMFORT",
    "5 - HIGH DEGREE OF USER COMFORT"
  ))

nrow(USFS_roads_SFNF) # 6320
plot(USFS_roads_SFNF)
(6320/8539)* 100 # = 74.01335 % of FS roads retained
100 - 74.01335 # = 25.98665 % dropped


#### USGS roads ----
# Transportation_National_GDB
# downloaded from The National Map transportation dataset
# then pre-processed (Analysis Tools -> Clip) in ArcGIS to only include roads in the SRME NFs

USGS_roads_SRME <- vect("Trans_RoadSegment_USGS_Clip_SRME.shp")
crs(USGS_roads_SRME) # EPSG: 5070
nrow(USGS_roads_SRME) # 312312

# get just roads in the SFNF
USGS_roads_SFNF <- terra::intersect(USGS_roads_SRME, SFNF_buffer1km)
nrow(USGS_roads_SFNF) # 15154
plot(USGS_roads_SFNF)


### rasterize ----
#### USFS ----
SFNF_USFS_road_rast <- rasterize(USFS_roads_SFNF, SFNF_QMD_rast , touches=TRUE)
plot(SFNF_USFS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(SFNF_USFS_road_rast)) # values not 1 are NA
global(SFNF_USFS_road_rast, fun = "notNA") # 324317 cells not NA

#### USGS ----
SFNF_USGS_road_rast <- rasterize(USGS_roads_SFNF, SFNF_QMD_rast , touches=TRUE)
plot(SFNF_USGS_road_rast, col="blue") # all values = 1 (if had a road line)
plot(is.na(SFNF_USGS_road_rast)) # values not 1 are NA
global(SFNF_USGS_road_rast, fun = "notNA") # 245002 cells not NA


### combine ----
SFNF_road_rast <- cover(SFNF_USFS_road_rast, SFNF_USGS_road_rast)
plot(SFNF_road_rast)
plot(is.na(SFNF_road_rast))
global(SFNF_road_rast, fun = "notNA") # 424818 cells not NA

##### write & read ----
writeRaster(SFNF_road_rast, "./SantaFeNF_S4F/.tif/SFNF_road_rast.tif")
SFNF_road_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_road_rast.tif")


### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
SFNF_road_dist_rast <- distance(SFNF_road_rast) 
plot(SFNF_road_dist_rast)
# cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(SFNF_road_dist_rast, "./SantaFeNF_S4F/.tif/SFNF_road_dist_rast.tif")
SFNF_road_dist_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_road_dist_rast.tif")


### filter ----
minmax(SFNF_road_dist_rast) 
# min = 0, max = 67491.69 
# but the max we want to include is 917.3261 meters (0.57 miles)
# if > threshold, make NA; else make value = 500

# make NA all values > 917.3261 meters, make others 500
SFNF_road_filt_rast <- ifel(SFNF_road_dist_rast > 917.3261, NA, 500)
plot(SFNF_road_filt_rast)


### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the SFNF
SFNF_road_filt_rast = crop(SFNF_road_filt_rast, SFNF_vect, mask = TRUE)


### viz ----
plot(SFNF_road_filt_rast, col = "darkorchid2")
polys(SFNF_vect, col = "black", alpha=0.01, lwd=1)

#### write & read ----
writeRaster(SFNF_road_filt_rast, "./SantaFeNF_S4F/.tif/SFNF_road_filt_rast.tif")
SFNF_road_filt_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_road_filt_rast.tif")



# (4) combine data ----
## resample ----
# first, the slope raster need to be resampled so the extent & resolution aligns with others
# I am choosing EVH to use as the template

slope_resampled <- resample(SFNF_slope_filt_rast, SFNF_EVH_filt_rast, method = "near")

raster_list <- list(SFNF_EVH_filt_rast,
                    SFNF_QMD_filt_rast,
                    SFNF_road_filt_rast,
                    slope_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 4 layers, each has same extent and resolution

## sum ----
SFNF_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 5, max = 615


## viz ----
plot(SFNF_combined_rast)
polys(SFNF_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(SFNF_combined_rast))


## stats ----
# we want to know what % of the SFNF each priority factor (PF) & combo occupies
# need a total # cells in the SFNF to compare
global(SFNF_DEM_rast, fun = "notNA") # 9426497 cells (covers all SFNF)
# but not same resolution as rest of data
DEM_resampled <- resample(SFNF_DEM_rast, SFNF_EVH_filt_rast, method = "bilinear")
# now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 7583452 cells (covers all SFNF)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(SFNF_QMD_rast, fun = "notNA") # 6090165 cells
(6090165/7583452)*100 # 80.30861 % of SFNF has QMD values

# areas with QMD > 5 inches
global(SFNF_QMD_filt_rast, fun = "notNA") # 4616580
(4616580/7583452)*100 # 60.87703 % of SFNF has trees > 5 in QMD

#### EVH ----
# all tree area
global(SFNF_EVH_rast, fun = "notNA") # 5943311
(5943311/7583452)*100 # 78.37211 % of SFNF has trees 

# trees > 10 ft area
global(SFNF_EVH_filt_rast, fun = "notNA") # 5389445
(5389445/7583452)*100 # 71.06849 % of SFNF has trees > 10 ft

#### slope ----
# need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 6331391 cells 
(6331391/7583452)*100 # 83.48956 % remaining after 24* filter

#### road ----
global(SFNF_road_filt_rast, fun = "notNA") # 5462661 cells
(5462661/7583452)*100 # 72.03396 % remaining


### combined PFs ----
# we want to know what % of the SFNF meets all of the priority factor thresholds, after combining

# value 615 = road + slope + QMD + EVH
global(SFNF_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2549368 cells
(2549368/7583452)*100 # 33.61751 % of SFNF


## filter & adjust value ----
# make cell value = 1 for all areas that meet our PFs & value = NA if not
SFNF_priority_rast <- ifel(
  SFNF_combined_rast == 615,
  1, NA)

# just confirm filter
global(SFNF_priority_rast, fun = "notNA") # 2549368 cells (same as value=615 above)


## calc area ---- 
# transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
# default units are m^2
expanse(SFNF_priority_rast, transform = FALSE) # 2294431200 m^2
2294431200/4046.86 # 4046.86 m2/acre = 566965.8 acres
# entire SFNF = 1681804 acres (calculated from SFNF_vect polygon in Part1A_2)
(566965.8/1681804)*100 # 33.71176 % of SFNF (almost same as value=615 above)

## viz ----
plot(SFNF_priority_rast, col = "goldenrod1")
polys(SFNF_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(SFNF_priority_rast, "./SantaFeNF_S4F/.tif/SFNF_priority_rast.tif")
SFNF_priority_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took ~20 minutes to run
priority_patches_all <- patches(SFNF_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 105933  patches


## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 105933  geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 


## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filter out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1350 geoms remain
(1350/104422)*100 # 1.292831 % of polys remain (are >= 20 acres)
# so ~99 % of patches/polys were < 20 acres (isolated areas)
# but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1179 geoms
(1179/1350)*100 # 87.33333 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 171 geoms
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
# 3302 geoms

# combine the mid-sized polys with the newly divided large polys
SFNF_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
# 4481 geoms


## adjust ----
# add new ID col & new final area col
SFNF_PCUs_1A_vect$PCU_ID <- paste0("SFNF_PCU_", seq_len(nrow(SFNF_PCUs_1A_vect)))
SFNF_PCUs_1A_vect$area_acres <- expanse(SFNF_PCUs_1A_vect) * 0.000247105

summary(SFNF_PCUs_1A_vect)
# area_acres min = 20.02, max = 331.69    
# not exactly within the desired 20-200 acre range, but close enough
# this is a step in the method that we could refine in the future

# select only new ID and area
SFNF_PCUs_1A_vect <- SFNF_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

SFNF_PCUs_1A_df <- as.data.frame(SFNF_PCUs_1A_vect)

sum(SFNF_PCUs_1A_vect$area_acres) # 475445.5 acres
sum(small_polys_removed$patch_acres) # 475445.5 acres
# bc these are =, we know the divide function worked (retained all area)


## stats ----
# SFNF is 1681804 acres 
(475445.5/1681804)*100 # 28.26997 % of SFNF are highest priority areas (PCUs)


## viz ----
plot(SFNF_PCUs_1A_vect, col = "goldenrod1", alpha=0.01, lwd=0.5)
polys(SFNF_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(SFNF_PCUs_1A_vect, "./SantaFeNF_S4F/.shp/SFNF_PCUs_1A_vect.shp")
SFNF_PCUs_1A_vect <- vect("./SantaFeNF_S4F/.shp/SFNF_PCUs_1A_vect.shp")


