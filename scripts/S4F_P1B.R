# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 1B: adding attributes to Potential Collection Units (PCUs)

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

## create regional boundary ----
### combine NFs within SRME ----
# load & process
NF_CONUS_vect <- vect("S_USA.FSCommonNames.shp")
crs(NF_CONUS_vect) # EPSG: 4269

# see unique names 
names(NF_CONUS_vect)
unique(NF_CONUS_vect$COMMONNAME)

SRME_NFs_list <- c("Arapaho and Roosevelt National Forests", "Carson National Forest", 
              "Grand Mesa, Uncompahgre and Gunnison National Forests", 
              "Medicine Bow-Routt National Forest", "Pike and San Isabel National Forests",
              "Rio Grande National Forest", "San Juan National Forest",
              "Santa Fe National Forest", "White River National Forest")

# select for just NFs in the SRME 
SRME_NFs_unprojected <- NF_CONUS_vect %>%
  filter(COMMONNAME %in% SRME_NFs_list)

# project 
SRME_NFs_projected <- project(SRME_NFs_unprojected,"EPSG:5070")


### add MLSNF ----
# polygon created in MantiLaSalNF_S4F_P1A.R  script (only using part of NF)
MLSNF_vect <- vect("MLSNF_vect.shp")

# add to other NFs
SRME_NFs <- rbind(SRME_NFs_projected, MLSNF_vect)
  # has 10 geometries


### add SRME ----
# some of the NFs fall slightly outside of the SRME
EPA_ecoregions <- vect("us_eco_l3.shp") # 1250 geoms

# see unique names 
names(EPA_ecoregions)
unique(EPA_ecoregions$US_L3NAME)

# select for just SRME
SRME_s_rockies <- EPA_ecoregions %>% 
  filter(US_L3NAME == "Southern Rockies")
  # has 4 separate polygons

# aggregate them together
SRME_aggregated <- terra::aggregate(SRME_s_rockies)
plot(SRME_aggregated)
  # has 1 geom & 0 attributes (they are lost after aggregate)

# project 
SRME_vect <- project(SRME_aggregated, "EPSG:5070")

# add to NFs
SRME_NFs_unagg <- rbind(SRME_NFs, SRME_vect)
  # has 11 geoms


### aggregate ----
SRME_NFs_vect <- aggregate(SRME_NFs_unagg)
plot(SRME_NFs_vect)
  # is the SRME boundary + the extra area just outside but covered by NFs

#### write & read ----
writeVector(SRME_NFs_vect, "SRME_NFs_vect.shp")
SRME_NFs_vect <- vect("SRME_NFs_vect.shp")


## combine PCU vectors ----
# these spatvectors were made individually for each national forest (Part 1A)
  # each has it's own script & directory of files
# here we are combining them together to add attribute data
  # at the end, we will split them up again by forest for sharing 

# read PCUs
ARNF_PCUs_1A_vect <- vect("./ArapahoRooseveltNF_S4F/.shp/ARNF_PCUs_1A_vect.shp")
CNF_PCUs_1A_vect <- vect("./CarsonNF_S4F/.shp/CNF_PCUs_1A_vect.shp")
GMUGNF_PCUs_1A_vect <- vect("./GMUGNF_S4F/.shp/GMUGNF_PCUs_1A_vect.shp")
MLSNF_PCUs_1A_vect <- vect("./MantiLaSalNF_S4F/.shp/MLSNF_PCUs_1A_vect.shp")
MBRNF_PCUs_1A_vect <- vect("./MedBowRouttNF_S4F/.shp/MBRNF_PCUs_1A_vect.shp")
PSINF_PCUs_1A_vect <- vect("./PikeSanIsabelNF_S4F/.shp/PSINF_PCUs_1A_vect.shp")
RGNF_PCUs_1A_vect <- vect("./RioGrandNF_S4F/.shp/RGNF_PCUs_1A_vect.shp")
SJNF_PCUs_1A_vect <- vect("./SanJuanNF_S4F/.shp/SJNF_PCUs_1A_vect.shp")
SFNF_PCUs_1A_vect <- vect("./SantaFeNF_S4F/.shp/SFNF_PCUs_1A_vect.shp")
WRNF_PCUs_1A_vect <- vect("./WhiteRiverNF_S4F/.shp/WRNF_PCUs_1A_vect.shp")

# make list
PCU_list <- list(ARNF_PCUs_1A_vect, CNF_PCUs_1A_vect, GMUGNF_PCUs_1A_vect, 
                 MLSNF_PCUs_1A_vect, MBRNF_PCUs_1A_vect, PSINF_PCUs_1A_vect,
                 RGNF_PCUs_1A_vect, SJNF_PCUs_1A_vect, SFNF_PCUs_1A_vect,
                 WRNF_PCUs_1A_vect)

# combine
SRME_PCUs <- do.call(rbind, PCU_list)
  # has 45640 geometries 


# (2) Pre-process data ----
## (A) crown fire probability ----
# using the crown fire probability (CFP) dataset from Pyrologix 

### read ----
# set path to folder with all match_clim() output rasters
setwd()
# create a list of raster files in folder 
rast_files <- list.files(path = ".", pattern = "\\.tif$", full.names = TRUE)
# make SpatRasterCollection 
SRME_CFP_all2 <- sprc(rast_files)
# mosaic
SRME_CFP_all <- mosaic(SRME_CFP_all2)
# viz
plot(SRME_CFP_all)
crs(SRME_CFP_all) # EPSG: 5070


### crop and mask ---
SRME_CFP_rast <- crop(SRME_CFP_all, SRME_NFs_vect, mask=TRUE)

# viz
plot(SRME_CFP_rast)
polys(SRME_NFs_vect, col = "black", alpha=0.01, lwd=2)

# stats
global(SRME_CFP_rast, fun = "notNA") # 165808312 cells at 30x30 m resolution
# this dataset covers 100% of the ARP  

#### write & read file ----
writeRaster(SRME_CFP_rast, "SRME_CFP_rast.tif")
SRME_CFP_rast <- rast("SRME_CFP_rast.tif")



## (B) nursery inventory ----


## (C) 




# (3) Extract values ----