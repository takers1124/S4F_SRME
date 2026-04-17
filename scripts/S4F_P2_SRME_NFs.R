# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 2: adding attributes to Potential Collection Units (PCUs)

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

sessionInfo()
RStudio.Version()

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
MLSNF_vect <- vect("./MantiLaSalNF_S4F/.shp/MLSNF_vect.shp")

# add to other NFs
SRME_NFs <- rbind(SRME_NFs_projected, MLSNF_vect)
  # has 10 geometries
plot(SRME_NFs)

#### write & read ----
writeVector(SRME_NFs, "./SRME_S4F/.shp/NFs_in_SRME_vect.shp")
SRME_NFs <- vect("./SRME_S4F/.shp/NFs_in_SRME_vect.shp")


### add SRME ----
# some of the NFs fall slightly outside of the SRME
EPA_ecoregions <- vect("us_eco_l3.shp") # 1250 geoms
crs(EPA_ecoregions) # EPSG: 8826

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
writeVector(SRME_NFs_vect, "./SRME_S4F/.shp/SRME_NFs_vect.shp")
SRME_NFs_vect <- vect("./SRME_S4F/.shp/SRME_NFs_vect.shp")
  # this will be used for mapping & some data-preprocessing


### add buffer ----
  # also need a buffer to clip road layers just outside of boundary (which is done in Arc)
SRME_NFs_buffer_vect <- buffer(SRME_NFs_vect, width = 1000) # units in meters

#### write & read ----
writeVector(SRME_NFs_buffer_vect, "./SRME_S4F/.shp/SRME_NFs_buffer_vect.shp")
SRME_NFs_buffer_vect <- vect("./SRME_S4F/.shp/SRME_NFs_buffer_vect.shp")



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
  # has 46179 geometries 
SRME_PCUs_df <- as.data.frame(SRME_PCUs)



# (2) Pre-process data ----
# several of the attributes we will add are coming from datasets that cover the entire continental US (e.g. LANDFIRE data)
    # for these, we simply extract those values for our PCU polygons

# however, for datasets that do not cover our entire study area completely (all NFs in SRME)
  # we need to pre-process the data (e.g. mosaic tiles, combine data frames, etc)
  # we will do that in this section (2) and then use them to create attributes for our PCUs (3) 


## (A) crown fire probability ----
# using the crown fire probability (CFP) dataset from Pyrologix 

### read ----
# set path to folder with all match_clim() output rasters
setwd()
# create a list of raster files in folder 
rast_files <- list.files(path = ".", pattern = "\\.tif$", full.names = TRUE)
# make SpatRasterCollection 
SRME_CFP_sprc <- sprc(rast_files)
# mosaic
SRME_CFP_all <- mosaic(SRME_CFP_sprc)
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
# this dataset covers 100% of the SRME  

#### write & read file ----
writeRaster(SRME_CFP_rast, "./SRME_S4F/.tif/SRME_CFP_rast.tif")
SRME_CFP_rast <- rast("./SRME_S4F/.tif/SRME_CFP_rast.tif")


## (B) elevation & slope ----

# because all of the PCUs are within a NF, we are going to combine all the DEMs & slope rasters that we already created
# for each NF, this was done in the NF_S4F_P1A.R script, step 3 (pre-process data)

### elev ----
ARNF_DEM_rast <- rast("./ArapahoRooseveltNF_S4F/.tif/ARNF_DEM_rast.tif")
CNF_DEM_rast <- rast("./CarsonNF_S4F/.tif/CNF_DEM_rast.tif")
GMUGNF_DEM_rast <- rast("./GMUGNF_S4F/.tif/GMUGNF_DEM_rast.tif")
MLSNF_DEM_rast <- rast("./MantiLaSalNF_S4F/.tif/MLSNF_DEM_rast.tif")
MBRNF_DEM_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_DEM_rast.tif")
PSINF_DEM_rast <- rast("./PikeSanIsabelNF_S4F/.tif/PSINF_DEM_rast.tif")
RGNF_DEM_rast <- rast("./RioGrandNF_S4F/.tif/RGNF_DEM_rast.tif")
SJNF_DEM_rast <- rast("./SanJuanNF_S4F/.tif/SJNF_DEM_rast.tif")
SFNF_DEM_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_DEM_rast.tif")
WRNF_DEM_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_DEM_rast.tif")

#### resample ----
# the resolution needs to be the same in order for mosaic() to accept them 
  # will use Crown Fire Probability as a template for resampling
  # because it covers the entire AOI and also has continuous values

ARNF_DEM_resp <- resample(ARNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
CNF_DEM_resp <- resample(CNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
GMUGNF_DEM_resp <- resample(GMUGNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
MLSNF_DEM_resp <- resample(MLSNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
MBRNF_DEM_resp <- resample(MBRNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
PSINF_DEM_resp <- resample(PSINF_DEM_rast, SRME_CFP_rast, method = "bilinear")
RGNF_DEM_resp <- resample(RGNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
SJNF_DEM_resp <- resample(SJNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
SFNF_DEM_resp <- resample(SFNF_DEM_rast, SRME_CFP_rast, method = "bilinear")
WRNF_DEM_resp <- resample(WRNF_DEM_rast, SRME_CFP_rast, method = "bilinear")

#### combine ----
# make list 
SRME_DEM_list <- list(ARNF_DEM_resp, CNF_DEM_resp, 
                      GMUGNF_DEM_resp, MLSNF_DEM_resp, 
                      MBRNF_DEM_resp, PSINF_DEM_resp,
                      RGNF_DEM_resp, SJNF_DEM_resp,
                      SFNF_DEM_resp, WRNF_DEM_resp)

# make SpatRasterCollection 
SRME_DEM_sprc <- sprc(SRME_DEM_list)
# mosaic
SRME_DEM_rast <- mosaic(SRME_DEM_sprc)
# viz
plot(SRME_DEM_rast)
summary(SRME_DEM_rast) # min = 1607, max = 4255           

##### write & read ----
writeRaster(SRME_DEM_rast, "./SRME_S4F/.tif/SRME_DEM_rast.tif") 
SRME_DEM_rast <- rast("./SRME_S4F/.tif/SRME_DEM_rast.tif")

### slope ----
SRME_slope_rast = terrain(SRME_DEM_rast, v="slope", unit="degrees")
plot(SRME_slope_rast)

#### write & read ----
writeRaster(SRME_slope_rast, "./SRME_S4F/.tif/SRME_slope_rast.tif")
SRME_slope_rast <- rast("./SRME_S4F/.tif/SRME_slope_rast.tif")


## (C) roads ----
# like elevation and slope, the road distance rasters were created independently for each NF
  # here we will mosaic those together 

ARNF_road_dist_rast <- rast("./ArapahoRooseveltNF_S4F/.tif/ARNF_road_dist_rast.tif")
CNF_road_dist_rast <- rast("./CarsonNF_S4F/.tif/CNF_road_dist_rast.tif")
GMUGNF_road_dist_rast <- rast("./GMUGNF_S4F/.tif/GMUGNF_road_dist_rast.tif")
MLSNF_road_dist_rast <- rast("./MantiLaSalNF_S4F/.tif/MLSNF_road_dist_rast.tif")
MBRNF_road_dist_rast <- rast("./MedBowRouttNF_S4F/.tif/MBRNF_road_dist_rast.tif")
PSINF_road_dist_rast <- rast("./PikeSanIsabelNF_S4F/.tif/PSINF_road_dist_rast.tif")
RGNF_road_dist_rast <- rast("./RioGrandNF_S4F/.tif/RGNF_road_dist_rast.tif")
SJNF_road_dist_rast <- rast("./SanJuanNF_S4F/.tif/SJNF_road_dist_rast.tif")
SFNF_road_dist_rast <- rast("./SantaFeNF_S4F/.tif/SFNF_road_dist_rast.tif")
WRNF_road_dist_rast <- rast("./WhiteRiverNF_S4F/.tif/WRNF_road_dist_rast.tif")

### combine ----
# make list 
SRME_road_dist_list <- list(ARNF_road_dist_rast, CNF_road_dist_rast, 
                      GMUGNF_road_dist_rast, MLSNF_road_dist_rast, 
                      MBRNF_road_dist_rast, PSINF_road_dist_rast,
                      RGNF_road_dist_rast, SJNF_road_dist_rast,
                      SFNF_road_dist_rast, WRNF_road_dist_rast)

# make SpatRasterCollection 
SRME_road_dist_sprc <- sprc(SRME_road_dist_list)
# mosaic
SRME_road_dist_rast <- mosaic(SRME_road_dist_sprc)
# crop & mask 
SRME_road_dist_rast <- crop(SRME_road_dist_rast, SRME_NFs_vect, mask = TRUE)
# viz
plot(SRME_road_dist_rast)

#### write & read ----
writeRaster(SRME_road_dist_rast, "./SRME_S4F/.tif/SRME_road_dist_rast.tif") 
SRME_road_dist_rast <- rast("./SRME_S4F/.tif/SRME_road_dist_rast.tif")


## (D) nursery inventory ----
# goal is to see if any of the PCUs that we identified are already represented in the nursery inventory
# nursery inventories are not standardized across the forest service

### import & adjust ----
# read in nursery inventories
# adjust species naming
# add nursery name column
# build cleaned lat/long columns & fix sign errors
b_seed <- read.csv("20230707_besseylots_editedlatlongs.csv") %>% 
  rename(source_code=SOURCE.CODE) %>% 
  mutate(species=sub(".*- ", "", SPECIES)) %>% 
  mutate(nursery = "Bessey") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), LAT, alt_lat), long_2 = ifelse(is.na(alt_long), LONG, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

lp_seed <- read.csv("LuckyPeak_inventory_2022_editedlatlongs.csv") %>% 
  rename(species=SP.Name) %>% 
  mutate(nursery = "LuckyPeak") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), Lat, alt_lat), long_2 = ifelse(is.na(alt_long), Long, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

cda_seed <- read.csv("CdA_inventory_2023_editedlatlongs.csv") %>% 
  rename(species = COMMON.NAME) %>% 
  mutate(species = ifelse(SP=="PSME", "Douglas-fir",
                          ifelse(SP=="PIEN", "Engelmann spruce",
                                 ifelse(SP=="PICO", "lodgepole pine",
                                        ifelse(SP=="PIPO", "ponderosa pine",
                                               ifelse(SP=="PIFL2", "limber pine",
                                                      species)))))) %>% 
  mutate(nursery = "CdA") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), LAT, alt_lat), long_2 = ifelse(is.na(alt_long), LONG, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

### combine ----
# added long, lat, and seed to select()
seed.nums.all <- 
  bind_rows(
    b_seed %>% 
      left_join(b_seed %>% 
                  group_by(species) %>% 
                  summarise(mean.gross.unit=mean(GROSS.UNIT, na.rm=T),mean.germ.pct = mean(GERM.PCT, na.rm=T)), by="species") %>% 
      mutate(n.seeds = BALANCE*mean.gross.unit) %>% 
      mutate(exp.n.germ = n.seeds*GERM.PCT/100, mean.exp.n.germ=n.seeds*mean.germ.pct/100) %>% 
      dplyr::select(species,source_code, DATE.CLCTD, BALANCE, GROSS.UNIT, GERM.PCT, mean.gross.unit, mean.germ.pct, n.seeds, exp.n.germ, mean.exp.n.germ, lat_2, long_2, nursery) %>% 
      dplyr::rename(Lot = source_code, Balance=BALANCE, SPP=GROSS.UNIT, mean.spp=mean.gross.unit, Year = DATE.CLCTD) %>% 
      mutate(Year=as.character(Year)),
    lp_seed %>% 
      left_join(lp_seed %>% 
                  group_by(species) %>% 
                  summarise(mean.spp=mean(SPP, na.rm=T),mean.germ.pct = mean(Germ.., na.rm=T)), by="species") %>% 
      mutate(n.seeds = Balance*mean.spp) %>% 
      mutate(exp.n.germ = n.seeds*Germ../100, mean.exp.n.germ=n.seeds*mean.germ.pct/100, Year=as.character(Year)) %>% 
      dplyr::select(species, Lot, Year, Balance, SPP, Germ.., mean.spp, mean.germ.pct, n.seeds, exp.n.germ, mean.exp.n.germ, lat_2, long_2, nursery) %>% 
      dplyr::rename(GERM.PCT = Germ..),
    cda_seed %>% 
      left_join(cda_seed %>% 
                  group_by(species) %>% 
                  summarise(mean.spp=mean(SEED.LB, na.rm=T),mean.germ.pct = mean(GERM.., na.rm=T)), by="species") %>% 
      mutate(n.seeds = LOT.BAL*mean.spp) %>% 
      mutate(exp.n.germ = n.seeds*GERM../100, mean.exp.n.germ=n.seeds*mean.germ.pct/100, Year=as.character(YR.CLCT)) %>% 
      dplyr::select(species, LOT, Year, LOT.BAL, SEED.LB, GERM.., mean.spp, mean.germ.pct, n.seeds, exp.n.germ, mean.exp.n.germ, lat_2, long_2, nursery) %>% 
      dplyr::rename(GERM.PCT = GERM.., Lot = LOT, Balance = LOT.BAL, SPP=SEED.LB)) %>% 
  mutate(across(everything(), ~replace(., . == "NaN" , NA))) %>% 
  rowwise() %>% 
  mutate(lowest.exp.germ = ifelse(is.na(exp.n.germ) & is.na(mean.exp.n.germ), NA, min(exp.n.germ, mean.exp.n.germ, na.rm=T)))

str(seed.nums.all)

### filter ----
# only for target spp
conifer_seed_all <- seed.nums.all %>% 
  filter(tolower(species) %in% c("Douglas-fir", "white fir",
                                 "Engelmann spruce", "lodgepole pine",
                                 "ponderosa pine", "limber pine",
                                 "southwestern white pine",
                                 "two-needle pinyon pine",
                                 "blue spruce", "bristlecone pine"))
# has 1696 rows (seedlots)

# filter only seedots that have lat/long info
conifer_seed <- conifer_seed_all %>% 
  filter(!is.na(lat_2), !is.na(long_2))
# 1282 rows
(1282/1696)*100 # 75.58962 % of seedlots have coords

# add ID col
conifer_seed$SL_ID <- 1:nrow(conifer_seed)

# fix messed up row... 1082 has lat/long switched...
conifer_seed_coords <- conifer_seed %>% 
  mutate(
    temp_lat = lat_2,# temp column to hold lat values
    lat_2 = if_else(
      SL_ID == "1082", # ID the problem row
      long_2 * -1, # if it's the problem row, swap long_2 value to into the lat_2 col and fix the sign
      lat_2 # if not the problem row, keep original lat_2 value
    ),
    long_2 = if_else(
      SL_ID == "1082",
      temp_lat * -1,
      long_2
    )
  ) %>% 
  select(-temp_lat)

### convert to points ----
# the points are for plotting
seed_points <- vect(x = conifer_seed_coords, geom = c("long_2", "lat_2"), crs = "epsg:4326")
plot(seed_points)

# project
seed_points_vect <- project(seed_points, "epsg:5070")
plot(seed_points_vect)

# but want to filter for only those that are located within the SRME
seed_within <- relate(seed_points_vect, SRME_NFs_vect, relation = "within")
# add as attribute to the spatvector
seed_points_vect$within_SRME <- seed_within

# and we don't need the other attributes
SRME_seed_points_vect <- seed_points_vect %>% 
  filter(within_SRME == "TRUE") %>% 
  select(SL_ID, Lot, species, nursery, Year, Balance)

plot(SRME_seed_points_vect)
# there are 166 points
(166/1282)*100 # = 12.94852 % of seedlots with lat/long are within the SRME

#### write & read ----
writeVector(SRME_seed_points_vect, "./SRME_S4F/.shp/SRME_seed_points_vect.shp")
SRME_seed_points_vect <- vect("./SRME_S4F/.shp/SRME_seed_points_vect.shp")

seed_points_df <- as.data.frame(SRME_seed_points_vect)


### convert to polys ----
# make a circular buffer poly with 564 m width (~ 1km^2 area)
SRME_seed_poly_vect <- buffer(SRME_seed_points_vect, width = 564)
plot(SRME_seed_poly_vect)

#### write & read ----
writeVector(SRME_seed_poly_vect, "./SRME_S4F/.shp/SRME_seed_poly_vect.shp")
SRME_seed_poly_vect <- vect("./SRME_S4F/.shp/SRME_seed_poly_vect.shp")



# (3) attribute creation ----
# in this step, we add attributes (metadata) to our PCU polygons
# this is how we further filter & select PCUs for scouting
# we will divide the attributes into 4 groups (A-D)

## (A) attributes: geographic info ----
# these attributes were not used to create PCUs
# we are adding them to provide basic geographic information about each PCU for reference and filtering


### nursery SL ----
# we want to know if any of the nursery seedlots (SLs) overlap with our PCUs
# and if they do overlap, the PCU will have the SL_ID added as an attribute for looking up later

# compute spatial relationship
SL_relate <- relate(SRME_PCUs, SRME_seed_poly_vect, relation = "intersects", pairs = TRUE, na.rm = TRUE)
str(SL_relate) # a list of vectors (one per PCU) with intersecting pairs

# adjust data
rel_df <- as.data.frame(SL_relate)
colnames(rel_df) <- c("pcu_idx", "seed_idx")
str(rel_df)

# compute per-PCU hit order
rel_df <- rel_df %>%
  group_by(pcu_idx) %>%
  mutate(hit = row_number()) %>%
  ungroup()

# slice the hits
first_hit  <- filter(rel_df, hit == 1) # has 456 rows
second_hit <- filter(rel_df, hit == 2) # has 52 rows
third_hit <- filter(rel_df, hit == 3) # has 1 row
# when expanding PCU creation, the # hits may be >3, so may need to adjust 

# prepare attribute vectors
SL_A <- rep(NA_character_, nrow(SRME_PCUs))
SL_B <- rep(NA_character_, nrow(SRME_PCUs))
SL_C <- rep(NA_character_, nrow(SRME_PCUs))

# assign seedlot names (Lot attribute) by index
SL_A[first_hit$pcu_idx]  <- SRME_seed_poly_vect$Lot[first_hit$seed_idx]
SL_B[second_hit$pcu_idx] <- SRME_seed_poly_vect$Lot[second_hit$seed_idx]
SL_C[third_hit$pcu_idx] <- SRME_seed_poly_vect$Lot[third_hit$seed_idx]

# attach attributes
SRME_PCUs$seedlot_A <- SL_A
SRME_PCUs$seedlot_B <- SL_B
SRME_PCUs$seedlot_C <- SL_C

# stats
count_non_na <- sum(!is.na(SRME_PCUs$seedlot_A))
# 456 PCUs overlap with at least 1 seedlot


### ranger district ----
ranger_districts <- vect("S_USA.BdyAdm_LSRS_RangerDistrict.shp")
crs(ranger_districts) # EPSG:4269
ranger_districts <- project(ranger_districts, "EPSG:5070")

# only need these 2 attributes
RDs <- ranger_districts %>% 
  select(FORESTNAME, DISTRICTNA)

# many PCUs span multiple RDs
# use the centroid (convert polys to points)
# more straight-forward than using relate() (as with the seedlots)
PCU_centroids <- centroids(SRME_PCUs)

# extract RDs at centroids
RD_at_centroid <- extract(RDs, PCU_centroids)

# add attributes 
SRME_PCUs$FORESTNAME <- RD_at_centroid$FORESTNAME 
SRME_PCUs$DISTRICTNA <- RD_at_centroid$DISTRICTNA 


### seed zone ----
# for this attribute, we are using the "historical tree seed zones"
  # this shapefile came from the USFS National Reforestation Data Sharepoint 
  # need USDA login credentials to access
  # does not include Utah (no seed zone data for Manti - La Sal NF)
# note, the USFS is currently working on updating these seed zones

SZ_all <- vect("HistoricalTreeSeedZones.shp")
crs(ranger_districts) # EPSG:4269
SZ_all_projected <- project(SZ_all, "EPSG:5070")

# extract
extract_SZ <- extract(SZ_all_projected, PCU_centroids)
str(extract_SZ)

# add attributes 
SRME_PCUs$seed_zone <- extract_SZ$SeedZone


### elevation ----
# using SRME_DEM_rast created in Part1B_2B (above)
SRME_DEM_rast <- rast("./SRME_S4F/.tif/SRME_DEM_rast.tif")
summary(SRME_DEM_rast) # min = 1607, max = 4255

# the DEM is in meters, but we want feet
# convert m to ft
meters_to_feet_factor <- 3.28084
SRME_DEM_ft <- SRME_DEM_rast * meters_to_feet_factor 
summary(SRME_DEM_ft) # min = 5271, max = 13960            

# extract median
Elv_med_df <- extract(SRME_DEM_ft, SRME_PCUs, fun = median)
str(Elv_med_df)

# rename col
Elv_med_df <- Elv_med_df %>% 
  rename(Elv_med_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)



## (B) attributes: thresholds for PCUs ----
# these attributes were used to create PCUs 
# we used them to set thresholds (e.g. only areas with QMD > 5 inches) in Part1A_3
# here we will just extract the median value for the metric across each PCU polygon

### QMD ----
# read
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif") # already in EPSG: 5070
# crop & mask 
QMD_SRME <- crop(QMD_CONUS, SRME_NFs_vect, mask = TRUE)
# extract value
extract_QMD <- extract(QMD_SRME, SRME_PCUs, fun = median, na.rm = TRUE)
str(extract_QMD)
# adjust
extract_QMD <- extract_QMD %>% 
  rename(QMD_in = TreeMap2022_CONUS_QMD) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)



### EVH ----
EVH_CONUS <- rast("LC24_EVH_250.tif") # already in EPSG: 5070
# crop & mask 
EVH_SRME <- crop(EVH_CONUS, SRME_NFs_vect, mask = TRUE)

# get just tree height and convert to feet
  # EVH value = 101 = tree height 1 meter
  # EVH value = 201 = shrub height 0.01 meter

# define conversion factor
meters_to_feet_factor <- 3.28084

EVH_SRME_rast <- ifel(
  EVH_SRME >= 101 & EVH_SRME < 200,
  (EVH_SRME - 100) * meters_to_feet_factor, # if true, subtract 100 to get meters, then convert to feet
  NA) # if false, make NA

# extract value
extract_EVH <- extract(EVH_SRME_rast, SRME_PCUs, fun = median, na.rm = TRUE)
str(extract_EVH)

# adjust
extract_EVH <- extract_EVH %>% 
  rename(EVH_ft = CLASSNAMES) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### slope ----
# using SRME_slope_rast created in Part1B_2B (above)
SRME_slope_rast <- rast("./SRME_S4F/.tif/SRME_slope_rast.tif")

extract_slope <- extract(SRME_slope_rast, SRME_PCUs, fun = median, na.rm = TRUE)
str(extract_slope)
# adjust
extract_slope <- extract_slope %>% 
  rename(slope_deg = slope) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### road ----
# using SRME_road_dist_rast created in Part1B_2B (above)
SRME_road_dist_rast <- rast("./SRME_S4F/.tif/SRME_road_dist_rast.tif")

extract_road_dist <- extract(SRME_road_dist_rast, SRME_PCUs, fun = median, na.rm = TRUE)
str(extract_road_dist)
# adjust
extract_road_dist <- extract_road_dist %>% 
  mutate(road_mi = layer * 0.000621371, # convert meters to miles
         layer = NULL, # remove distance in meters column
         PCU_ID = SRME_PCUs$PCU_ID)  %>% 
  select(-1)



## (C) attributes: other objectives ----
# these attributes are added to the PCU as a tool for objective-focused filtering

### MCMT ----
# using Mean Coldest Month Temp (MCMT) during the reference period (1961-1990)
MCMT_CONUS <- rast("Normal_1961_1990_MCMT.tif")
crs(MCMT_CONUS) # EPSG: 8806
# project
MCMT_CONUS_projected <- project(MCMT_CONUS, "EPSG:5070")
# crop & mask 
MCMT_SRME <- crop(MCMT_CONUS_projected, SRME_NFs_vect, mask = TRUE)

# extract
extract_MCMT <- extract(MCMT_SRME, SRME_PCUs, fun = median, na.rm = TRUE)
str(extract_MCMT)
# adjust
extract_MCMT <- extract_MCMT %>% 
  rename(MCMT_C = "bigfile[, varname]") %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### CFP ----
# using SRME_CFP_rast created in Part1B_2B (above)
SRME_CFP_rast <- rast("./SRME_S4F/.tif/SRME_CFP_rast.tif")

# extract
extract_CFP <- extract(SRME_CFP_rast, SRME_PCUs, fun = median, na.rm = TRUE)
str(extract_CFP)
# adjust
extract_CFP <- extract_CFP %>% 
  rename(CFP_prob = crown_fire_2025_c00047_r00042) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### BALIVE ----
# read 
BA_CONUS <- rast("TreeMap2022_CONUS_BALIVE.tif") # already in EPSG: 5070
# crop & mask 
BA_SRME <- crop(BA_CONUS, SRME_NFs_vect, mask = TRUE)
# extract
extract_BA <- extract(BA_SRME, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_BA)
# adjust
extract_BA <- extract_BA %>% 
  rename(BA_ft_sq = TreeMap2022_CONUS_BALIVE) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PIPO ----
# read 
PIPO_BigMap <- rast("Hosted_AGB_0122_2018_PONDEROSA_PINE_08142023231656.tif")
# project
crs(PIPO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIPO_projected <- project(PIPO_BigMap, "EPSG:5070")
crs(PIPO_projected) # EPSG 5070
# crop & mask
SRME_PIPO_rast <- crop(PIPO_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIPO_rast, "./SRME_S4F/.tif/SRME_PIPO_rast.tif")
SRME_PIPO_rast <- rast("./SRME_S4F/.tif/SRME_PIPO_rast.tif")

# extract
extract_PIPO <- extract(SRME_PIPO_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIPO)
# adjust
extract_PIPO <- extract_PIPO %>% 
  rename(PIPO_tons = Hosted_AGB_0122_2018_PONDEROSA_PINE_08142023231656) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PICO ----
# read 
PICO_BigMap <- rast("Hosted_AGB_0108_2018_LODGEPOLE_PINE_05272023153302.tif")
# project
crs(PICO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PICO_projected <- project(PICO_BigMap, "EPSG:5070")
crs(PICO_projected) # EPSG 5070
# crop & mask
SRME_PICO_rast <- crop(PICO_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PICO_rast, "./SRME_S4F/.tif/SRME_PICO_rast.tif")
SRME_PICO_rast <- rast("./SRME_S4F/.tif/SRME_PICO_rast.tif")

# extract
extract_PICO <- extract(SRME_PICO_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PICO)
# adjust
extract_PICO <- extract_PICO %>% 
  rename(PICO_tons = Hosted_AGB_0108_2018_LODGEPOLE_PINE_05272023153302) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PIFL ----
# read 
PIFL_BigMap <- rast("Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457.tif")
# project
crs(PIFL_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIFL_projected <- project(PIFL_BigMap, "EPSG:5070")
crs(PIFL_projected) # EPSG 5070
# crop & mask
SRME_PIFL_rast <- crop(PIFL_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIFL_rast, "./SRME_S4F/.tif/SRME_PIFL_rast.tif")
SRME_PIFL_rast <- rast("./SRME_S4F/.tif/SRME_PIFL_rast.tif")

# extract
extract_PIFL <- extract(SRME_PIFL_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIFL)
# adjust
extract_PIFL <- extract_PIFL %>% 
  rename(PIFL_tons = Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)



### PIST ----
# read 
PIST_BigMap <- rast("Hosted_AGB_0114_2018_SOUTHWESTERN_WHITE_PINE__05302023224529.tif")
# project
crs(PIST_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIST_projected <- project(PIST_BigMap, "EPSG:5070")
crs(PIST_projected) # EPSG 5070
# crop & mask
SRME_PIST_rast <- crop(PIST_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIST_rast, "./SRME_S4F/.tif/SRME_PIST_rast.tif")
SRME_PIST_rast <- rast("./SRME_S4F/.tif/SRME_PIST_rast.tif")

# extract
extract_PIST <- extract(SRME_PIST_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIST)
# adjust
extract_PIST <- extract_PIST %>% 
  rename(PIST_tons = Hosted_AGB_0114_2018_SOUTHWESTERN_WHITE_PINE__05302023224529) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PIAR ----
# read 
PIAR_BigMap <- rast("Hosted_AGB_0102_2018_ROCKY_MOUNTAIN_BRISTLECONE_PINE_06142023185256.tif")
# project
crs(PIAR_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIAR_projected <- project(PIAR_BigMap, "EPSG:5070")
crs(PIAR_projected) # EPSG 5070
# crop & mask
SRME_PIAR_rast <- crop(PIAR_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIAR_rast, "./SRME_S4F/.tif/SRME_PIAR_rast.tif")
SRME_PIAR_rast <- rast("./SRME_S4F/.tif/SRME_PIAR_rast.tif")

# extract
extract_PIAR <- extract(SRME_PIAR_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIAR)
# adjust
extract_PIAR <- extract_PIAR %>% 
  rename(PIAR_tons = Hosted_AGB_0102_2018_ROCKY_MOUNTAIN_BRISTLECONE_PINE_06142023185256) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PIED ----
# read 
PIED_BigMap <- rast("Hosted_AGB_0106_2018_COMMON_OR_TWO_NEEDLE_PINYON_08142023230307.tif")
# project
crs(PIED_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIED_projected <- project(PIED_BigMap, "EPSG:5070")
crs(PIED_projected) # EPSG 5070
# crop & mask
SRME_PIED_rast <- crop(PIED_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIED_rast, "./SRME_S4F/.tif/SRME_PIED_rast.tif")
SRME_PIED_rast <- rast("./SRME_S4F/.tif/SRME_PIED_rast.tif")

# extract
extract_PIED <- extract(SRME_PIED_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIED)
# adjust
extract_PIED <- extract_PIED %>% 
  rename(PIED_tons = Hosted_AGB_0106_2018_COMMON_OR_TWO_NEEDLE_PINYON_08142023230307) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PIEN ----
# read 
PIEN_BigMap <- rast("Hosted_AGB_0093_2018_ENGELMANN_SPRUCE_05042023231614.tif")
# project
crs(PIEN_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIEN_projected <- project(PIEN_BigMap, "EPSG:5070")
crs(PIEN_projected) # EPSG 5070
# crop & mask
SRME_PIEN_rast <- crop(PIEN_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIEN_rast, "./SRME_S4F/.tif/SRME_PIEN_rast.tif")
SRME_PIEN_rast <- rast("./SRME_S4F/.tif/SRME_PIEN_rast.tif")

# extract
extract_PIEN <- extract(SRME_PIEN_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIEN)
# adjust
extract_PIEN <- extract_PIEN %>% 
  rename(PIEN_tons = Hosted_AGB_0093_2018_ENGELMANN_SPRUCE_05042023231614) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### PIPU ----
# read 
PIPU_BigMap <- rast("Hosted_AGB_0096_2018_BLUE_SPRUCE_05272023061919.tif")
# project
crs(PIPU_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIPU_projected <- project(PIPU_BigMap, "EPSG:5070")
crs(PIPU_projected) # EPSG 5070
# crop & mask
SRME_PIPU_rast <- crop(PIPU_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PIPU_rast, "./SRME_S4F/.tif/SRME_PIPU_rast.tif")
SRME_PIPU_rast <- rast("./SRME_S4F/.tif/SRME_PIPU_rast.tif")

# extract
extract_PIPU <- extract(SRME_PIPU_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PIPU)
# adjust
extract_PIPU <- extract_PIPU %>% 
  rename(PIPU_tons = Hosted_AGB_0096_2018_BLUE_SPRUCE_05272023061919) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)



### PSME ----
# read 
PSME_BigMap <- rast("Hosted_AGB_0202_2018_DOUGLAS_FIR_06012023172436.tif")
# project
crs(PSME_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PSME_projected <- project(PSME_BigMap, "EPSG:5070")
crs(PSME_projected) # EPSG 5070
# crop & mask
SRME_PSME_rast <- crop(PSME_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_PSME_rast, "./SRME_S4F/.tif/SRME_PSME_rast.tif")
SRME_PSME_rast <- rast("./SRME_S4F/.tif/SRME_PSME_rast.tif")

# extract
extract_PSME <- extract(SRME_PSME_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_PSME)
# adjust
extract_PSME <- extract_PSME %>% 
  rename(PSME_tons = Hosted_AGB_0202_2018_DOUGLAS_FIR_06012023172436) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)


### ABCO ----
# read 
ABCO_BigMap <- rast("Hosted_AGB_0015_2018_WHITE_FIR_08142023164615.tif")
# project
crs(ABCO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
ABCO_projected <- project(ABCO_BigMap, "EPSG:5070")
crs(ABCO_projected) # EPSG 5070
# crop & mask
SRME_ABCO_rast <- crop(ABCO_projected, SRME_NFs_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(SRME_ABCO_rast, "./SRME_S4F/.tif/SRME_ABCO_rast.tif")
SRME_ABCO_rast <- rast("./SRME_S4F/.tif/SRME_ABCO_rast.tif")

# extract
extract_ABCO <- extract(SRME_ABCO_rast, SRME_PCUs, fun = median, na.rm=TRUE)
str(extract_ABCO)
# adjust
extract_ABCO <- extract_ABCO %>% 
  rename(ABCO_tons = Hosted_AGB_0015_2018_WHITE_FIR_08142023164615) %>% 
  mutate(PCU_ID = SRME_PCUs$PCU_ID) %>% 
  select(-1)



# (4) combine & divide ----
SRME_PCUs # has PCU_ID, area_acres, seedlot_A, seedlot_B, seedlot_C, FORESTNAME, DISTRICTNA, seed_zone
# we added these one at a time (because they came from SpatVectors)

# now add the others
match_join_df <- Elv_med_df %>% 
  left_join(extract_EVH, by = "PCU_ID") %>% 
  left_join(extract_QMD, by = "PCU_ID") %>% 
  left_join(extract_slope, by = "PCU_ID") %>% 
  left_join(extract_road_dist, by = "PCU_ID") %>% 
  left_join(extract_MCMT, by = "PCU_ID") %>% 
  left_join(extract_CFP, by = "PCU_ID") %>% 
  left_join(extract_BA, by = "PCU_ID") %>% 
  left_join(extract_PIPO, by = "PCU_ID") %>% 
  left_join(extract_PICO, by = "PCU_ID") %>% 
  left_join(extract_PIFL, by = "PCU_ID") %>% 
  left_join(extract_PIST, by = "PCU_ID") %>% 
  left_join(extract_PIAR, by = "PCU_ID") %>% 
  left_join(extract_PIED, by = "PCU_ID") %>% 
  left_join(extract_PIEN, by = "PCU_ID") %>% 
  left_join(extract_PIPU, by = "PCU_ID") %>% 
  left_join(extract_PSME, by = "PCU_ID") %>% 
  left_join(extract_ABCO, by = "PCU_ID")

# merge with previous PCU spatvector, making new one with full attributes
SRME_PCUs_vect <- SRME_PCUs %>% 
  left_join(match_join_df, by = "PCU_ID")

SRME_PCUs_vect_df <- as.data.frame(SRME_PCUs_vect)

## write & read ----
write.csv(SRME_PCUs_df, "./SRME_S4F/.shp/SRME_PCUs_df.csv", row.names = FALSE)
writeVector(SRME_PCUs_vect, "./SRME_S4F/.shp/SRME_PCUs_vect.shp")
SRME_PCUs_vect <- vect("./SRME_S4F/.shp/SRME_PCUs_vect.shp")

# ** not finished ----
# want to wait until after pub / starting to share with managers

## divide ----
# we want to have a separate .shp for each NF to share with managers 

# ARNF
ARNF_PCUs_vect <- SRME_PCUs_vect %>% 
  filter(startsWith(PCU_ID, "ARNF"))

writeVector(ARNF_PCUs_vect, "ARNF_PCUs_vect.shp")
ARNF_PCUs_vect <- vect("./ArapahoRooseveltNF_S4F/.shp/ARNF_PCUs_vect.shp")

# CNF

CNF_PCUs_vect <- vect("./CarsonNF_S4F/.shp/CNF_PCUs_vect.shp")


# GMUGNF

GMUGNF_PCUs_vect <- vect("./GMUGNF_S4F/.shp/GMUGNF_PCUs_vect.shp")


# MLSNF 

MLSNF_PCUs_vect <- vect("./MantiLaSalNF_S4F/.shp/MLSNF_PCUs_vect.shp")


# MBRNF

MBRNF_PCUs_vect <- vect("./MedBowRouttNF_S4F/.shp/MBRNF_PCUs_vect.shp")


# PSINF

PSINF_PCUs_vect <- vect("./PikeSanIsabelNF_S4F/.shp/PSINF_PCUs_vect.shp")


# RGNF

RGNF_PCUs_vect <- vect("./RioGrandNF_S4F/.shp/RGNF_PCUs_vect.shp")


# SJNF

SJNF_PCUs_vect <- vect("./SanJuanNF_S4F/.shp/SJNF_PCUs_vect.shp")


# SFNF

SFNF_PCUs_vect <- vect("./SantaFeNF_S4F/.shp/SFNF_PCUs_vect.shp")


# WRNF

WRNF_PCUs_vect <- vect("./WhiteRiverNF_S4F/.shp/WRNF_PCUs_vect.shp")


