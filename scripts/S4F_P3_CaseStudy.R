# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 3: creation of Potential Planting Units (PPUs) for the case study

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

# (2) make PPU polygons ----
# we create Potential Planting Units (PPUs)
  # using the FACTS needs polys from within the Cameron Peak (CP) fire boundary
  # last downloaded on August 3rd, 2025
# we divided the FACTS needs into small (50-200 acre) polygons
  # then assigned a 500 ft elevation band (EB) to each
  # and we are just using the 8500-9000 ft EB for this part of the case study
# we will extract the future clim from these needs in Part 2.3B

## import ----
needs_all <- vect("S_USA.Actv_SilvReforest_Needs.shp")
names(needs_all)
unique(needs_all$ACTIVITY_C)

## filter ----
desired_cols <- c("REGION_COD", "ADMIN_FORE", "DISTRICT_C", "FACTS_ID", "ACTIVITY_C")
  # the column ACTIVITY_C has the activity code
  # if we were looking for all planting needs, would use code 4431
desired_ids <- c("RA20CPPLNT")
  # for our case study, we just want this single polygon with FACTS_ID = RA20CPPLNT
  # this is essentially all the CP needs in 1 poly

CP_big_need_poly <- needs_all %>%
  select(all_of(desired_cols)) %>% 
  filter(FACTS_ID %in% desired_ids) 

## project ---- 
CP_big_need_poly <- project(CP_big_need_poly, "EPSG:5070")
plot(CP_big_need_poly)
expanse(CP_big_need_poly)* 0.000247105
# has 1 geometry (one large multipart polygon)

## process ----
### disaggregate ----
CP_disagg_need_poly <- disagg(CP_big_need_poly)
# has 362 geometries

# add an ID col
CP_disagg_need_poly$disagg_ID <- 1:nrow(CP_disagg_need_poly)

### separate sizes ----
# calc area (default in m^2) & convert to acres
CP_disagg_need_poly$area_disagg <- expanse(CP_disagg_need_poly) * 0.000247105

# filt out small poys (< 20 acres)
small_polys_removed <- CP_disagg_need_poly[CP_disagg_need_poly$area_disagg >= 20, ]
# 112 geoms remain
(112/362)*100 # 30.93923 % of polys remain (are >= 20 acres)

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$area_disagg <= 200, ]
# 85 geoms
(85/362)*100 # 23.48066 % of polys >= 20 acres are also <= 200 acres
# these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$area_disagg > 200, ]
# 27 geoms
# these do need to be divided

### divide ----
# calculate divisions needed for each large poly, ensuring at least 2 parts for large polys
num_all_parts <- pmax(2, round(large_polys$area_disagg / 125))

# use lapply to iterate and divide
divided_polys_list <- lapply(1:nrow(large_polys), function(i) {
  poly <- large_polys[i, ]
  # set a seed to ensure reproducibility for the division process
  set.seed(i)
  # divide by the pre-determined number of parts for that particular poly
  divided_poly <- divide(poly, n = num_all_parts[i])
  # store the original ID and re-calculate the new areas
  divided_poly$disagg_ID <- poly$disagg_ID
  divided_poly$area_divided <- expanse(divided_poly) * 0.000247105
  
  return(divided_poly)
})

# combine all divided polys into a single SpatVector
divided_polys_vect <- do.call(rbind, divided_polys_list)
# 349 geoms

# combine the mid-sized polys with the newly divided large polys
CP_PPUs_vect <- rbind(mid_polys, divided_polys_vect)
# 434 geoms

### adjust ----
# add new ID col & new final area col
CP_PPUs_vect$PPU_ID <- 1:nrow(CP_PPUs_vect)
CP_PPUs_vect$area_acres <- expanse(CP_PPUs_vect) * 0.000247105

summary(CP_PPUs_vect$area_acres)
# min = 20.68, max = 203.97
sum(CP_PPUs_vect$area_acres) # 49151.23 acres
sum(small_polys_removed$area_disagg) # 49151.23 acres
# bc these are =, we know the divide function worked (retained all area)

# select only new ID and area
CP_PPUs_vect <- CP_PPUs_vect[, c("PPU_ID", "area_acres")]
plot(CP_PPUs_vect)


## add Elv ----
# using the DEM created in part 1A_3b
ARNF_DEM_rast <- rast("./ArapahoRooseveltNF_S4F/.tif/ARNF_DEM_rast.tif")

# the DEM is in meters --> convert m to ft
meters_to_feet_factor <- 3.28084
ARNF_DEM_ft <- ARNF_DEM_rast * meters_to_feet_factor 
summary(ARNF_DEM_ft) # min = 5374, max = 14030   

# extract median
Elv_med_df <- extract(ARNF_DEM_ft, CP_PPUs_vect, fun=median)
str(Elv_med_df)
# rename col
Elv_med_df <- Elv_med_df %>% 
  rename(Elv_med_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PPU_ID = CP_PPUs_vect$PPU_ID) %>% 
  select(-1)

# add to spatvector
CP_PPUs_vect <- CP_PPUs_vect %>% 
  left_join(Elv_med_df, by = "PPU_ID")

### write & read ----
writeVector(CP_PPUs_vect, "./CaseStudy_S4F/PPUs/CP_PPUs_vect.shp")
CP_PPUs_vect <- vect("./CaseStudy_S4F/PPUs/CP_PPUs_vect.shp")


## filter ----
# for the case study, we are only going to use the planting needs (PPUs)
# that are within the 9000 - 9500 ft EB
PPUs_9000_9500_vect <- CP_PPUs_vect %>% 
  filter(Elv_med_ft >= 9000, Elv_med_ft <= 9500)
# 86 geoms
# too many for the case study - pick a subset of spatially close polys
# using Arc to visualize and choose (this is subjective)

CaseStudy_PPUs <- c("223", "225", "232", "235", "239", "281", "282", "283", "286", "287", "288", # just 11
                    "7", "14", "15", "356", "357", "359", "401") # 18

CaseStudy_PPUs_vect <- CP_PPUs_vect %>% 
  filter(PPU_ID %in% CaseStudy_PPUs)
# 18 geoms

sum(CaseStudy_PPUs_vect$area_acres) # 2272.514 acres

### write & read ----
writeVector(CaseStudy_PPUs_vect, "./CaseStudy_S4F/PPUs/CaseStudy_PPUs_vect.shp")
CaseStudy_PPUs_vect <- vect("./CaseStudy_S4F/PPUs/CaseStudy_PPUs_vect.shp")



# (3) add climate data ----
# data from ClimateNA

# we are only using 1 climate variable: Mean Coldest Month Temperature (MCMT)
# but using 4 climate periods

reference_1961_1990_MCMT <- rast("Normal_1961_1990_MCMT.tif")
current_2011_2040_MCMT <- rast("UKESM10LL_ssp245_2011_2040_MCMT.tif")
ssp2_2041_2070_MCMT <- rast("UKESM10LL_ssp245_2041_2070_MCMT.tif")
ssp5_2041_2070_MCMT <- rast("UKESM10LL_ssp585_2041_2070_MCMT.tif")

crs(reference_1961_1990_MCMT) # EPSG: 8806


## (A) pre-process clim data ----
  # project, crop, & mask 
  # only need MCMT for the ARNF (much smaller file) bc case study PPUs are in ARNF
  # but setting up the code for all NFs in SRME for later expansion

### reference ----
ref_projected <- project(reference_1961_1990_MCMT, "EPSG:5070")

# ARNF
ref_ARNF_rast <- crop(ref_projected, ARNF_vect, mask = TRUE)
summary(ref_ARNF_rast)  # min: -12.839, max: -1.303, mean: -7.612     
plot(ref_ARNF_rast)  
names(ref_ARNF_rast) <- "ref_MCMT"

# SRME_NFs 
ref_SRME_rast <- crop(ref_projected, SRME_NFs_vect, mask = TRUE)
summary(ref_SRME_rast)  # min: -13.878, max: -0.163, mean: -6.608  
plot(ref_SRME_rast) 
polys(ARNF_vect, col = "black", alpha=0.01, lwd=0.5)
names(ref_SRME_rast) <- "ref_MCMT"

#### write & read ----
writeRaster(ref_ARNF_rast, "./CaseStudy_S4F/MCMT_rast/ref_ARNF_rast.tif")
ref_ARNF_rast <- rast("./CaseStudy_S4F/MCMT_rast/ref_ARNF_rast.tif")

writeRaster(ref_SRME_rast, "./CaseStudy_S4F/MCMT_rast/ref_SRME_rast.tif")
ref_SRME_rast <- rast("./CaseStudy_S4F/MCMT_rast/ref_SRME_rast.tif")


### current ----
current_projected <- project(current_2011_2040_MCMT, "EPSG:5070")

# ARNF
curr_ARNF_rast <- crop(current_projected, ARNF_vect, mask = TRUE)
summary(curr_ARNF_rast)  # min: -10.5840, max: 0.8993  , mean: -5.3997       
plot(curr_ARNF_rast)  
names(curr_ARNF_rast) <- "curr_MCMT"

# SRME 
curr_SRME_rast <- crop(current_projected, SRME_NFs_vect, mask = TRUE)
summary(curr_SRME_rast)  # min: -11.478, max: 2.461, mean: -4.307     
plot(curr_SRME_rast) 
polys(ARNF_vect, col = "black", alpha=0.01, lwd=0.5)
names(curr_SRME_rast) <- "curr_MCMT"

#### write & read ----
writeRaster(curr_ARNF_rast, "./CaseStudy_S4F/MCMT_rast/curr_ARNF_rast.tif")
curr_ARNF_rast <- rast("./CaseStudy_S4F/MCMT_rast/curr_ARNF_rast.tif")

writeRaster(curr_SRME_rast, "./CaseStudy_S4F/MCMT_rast/curr_SRME_rast.tif")
curr_SRME_rast <- rast("./CaseStudy_S4F/MCMT_rast/curr_SRME_rast.tif")


### ssp2 ----
ssp2_projected <- project(ssp2_2041_2070_MCMT, "EPSG:5070") # took ~ 4 mins

# ARNF
ssp2_ARNF_rast <- crop(ssp2_projected, ARNF_vect, mask = TRUE)
summary(ssp2_ARNF_rast)  # min: -8.2840, max: 3.3150, mean: -3.0287     
plot(ssp2_ARNF_rast)  
names(ssp2_ARNF_rast) <- "ssp2_MCMT"

# SRME
ssp2_SRME_rast <- crop(ssp2_projected, SRME_NFs_vect, mask = TRUE)
summary(ssp2_SRME_rast) # min: -9.078, max: 5.480, mean: -1.630
plot(ssp2_SRME_rast)    
polys(ARNF_vect, col = "black", alpha=0.01, lwd=0.5)
names(ssp2_SRME_rast) <- "ssp2_MCMT"

#### write & read ----
writeRaster(ssp2_ARNF_rast, "./CaseStudy_S4F/MCMT_rast/ssp2_ARNF_rast.tif")
ssp2_ARNF_rast <- rast("./CaseStudy_S4F/MCMT_rast/ssp2_ARNF_rast.tif")

writeRaster(ssp2_SRME_rast, "./CaseStudy_S4F/MCMT_rast/ssp2_SRME_rast.tif")
ssp2_SRME_rast <- rast("./CaseStudy_S4F/MCMT_rast/ssp2_SRME_rast.tif")


### ssp5 ----
ssp5_projected <- project(ssp5_2041_2070_MCMT, "EPSG:5070") # took ~ 4 mins

# ARNF
ssp5_ARNF_rast <- crop(ssp5_projected, ARNF_vect, mask = TRUE)
summary(ssp5_ARNF_rast)  # min: -7.0859, max: 4.5150, mean: -1.8743   
plot(ssp5_ARNF_rast)  
names(ssp5_ARNF_rast) <- "ssp5_MCMT"

# SRME
ssp5_SRME_rast <- crop(ssp5_projected, SRME_NFs_vect, mask = TRUE)
summary(ssp5_SRME_rast)  # min: -8.078, max: 6.868, mean: -0.363 
plot(ssp5_SRME_rast)  
polys(ARNF_vect, col = "black", alpha=0.01, lwd=0.5)
names(ssp5_SRME_rast) <- "ssp5_MCMT"

#### write & read ----
writeRaster(ssp5_ARNF_rast, "./CaseStudy_S4F/MCMT_rast/ssp5_ARNF_rast.tif")
ssp5_ARNF_rast <- rast("./CaseStudy_S4F/MCMT_rast/ssp5_ARNF_rast.tif")

writeRaster(ssp5_SRME_rast, "./CaseStudy_S4F/MCMT_rast/ssp5_SRME_rast.tif")
ssp5_SRME_rast <- rast("./CaseStudy_S4F/MCMT_rast/ssp5_SRME_rast.tif")



## (B) extract clims ----

### ref ----
ref_MCMT_df <- extract(ref_ARNF_rast, CaseStudy_PPUs_vect, fun = median, na.rm = TRUE)
str(ref_MCMT_df)
# adjust
ref_MCMT_df <- ref_MCMT_df %>% 
  mutate(PPU_ID = CaseStudy_PPUs_vect$PPU_ID) %>% 
  select(-1)

### curr ----
curr_MCMT_df <- extract(curr_ARNF_rast, CaseStudy_PPUs_vect, fun = median, na.rm = TRUE)
str(curr_MCMT_df)
# adjust
curr_MCMT_df <- curr_MCMT_df %>% 
  mutate(PPU_ID = CaseStudy_PPUs_vect$PPU_ID) %>% 
  select(-1)

### ssp2 ----
ssp2_MCMT_df <- extract(ssp2_ARNF_rast, CaseStudy_PPUs_vect, fun = median, na.rm = TRUE)
str(ssp2_MCMT_df)
# adjust
ssp2_MCMT_df <- ssp2_MCMT_df %>% 
  mutate(PPU_ID = CaseStudy_PPUs_vect$PPU_ID) %>% 
  select(-1)

### ssp5 ----
ssp5_MCMT_df <- extract(ssp5_ARNF_rast, CaseStudy_PPUs_vect, fun = median, na.rm = TRUE)
str(ssp5_MCMT_df)
# adjust
ssp5_MCMT_df <- ssp5_MCMT_df %>% 
  mutate(PPU_ID = CaseStudy_PPUs_vect$PPU_ID) %>% 
  select(-1)


### combine ----
PPU_MCMT_df <- ref_MCMT_df %>% 
  left_join(curr_MCMT_df, by = "PPU_ID") %>% 
  left_join(ssp2_MCMT_df, by = "PPU_ID") %>% 
  left_join(ssp5_MCMT_df, by = "PPU_ID")
str(PPU_MCMT_df)
  


# (4) match clims ----

## (A) Pivot PPU MCMT to long format ---- 
PPU_MCMT_long <- PPU_MCMT_df %>% 
  select(PPU_ID, ref_MCMT, curr_MCMT, ssp2_MCMT, ssp5_MCMT) %>% 
  pivot_longer( cols = c(ref_MCMT, curr_MCMT, ssp2_MCMT, ssp5_MCMT), 
                names_to = "climate_period", values_to = "PPU_MCMT" ) %>% 
  # clean up period labels 
  mutate(climate_period = recode(climate_period, 
                                 "ref_MCMT" = "ref_1961_1990", 
                                 "curr_MCMT" = "curr_2011_2040", 
                                 "ssp2_MCMT" = "ssp2_2041_2070", 
                                 "ssp5_MCMT" = "ssp5_2041_2070" )) 
# check 
str(PPU_MCMT_long) 
  # expect: 72 rows (18 PPUs x 4 periods), 3 columns 

## (B) Cross join with PCU MCMT only ---- 
  # PCUs were created in part 1, and attributes were added in part 2
  # there are 46179 total PCUs across all 10 NFs in the SRME

### read ----
SRME_PCUs_vect <- vect("./SRME_S4F/.shp/SRME_PCUs_vect.shp")
SRME_PCUs_df <- as.data.frame(SRME_PCUs_vect)


PCU_MCMT_df <- SRME_PCUs_df %>% 
  select(PCU_ID, MCMT_C) %>% 
  rename(PCU_MCMT = MCMT_C)

str(PCU_MCMT_df)

# match PCUs with PPUs  
match_full_df <- PPU_MCMT_long %>% 
  cross_join(PCU_MCMT_df) 

# check 
nrow(match_full_df) 
  # 3324888 rows (18 PPUs x 4 periods x 46179 PCUs) 


## (C) Calculate MCMT difference and filter to matches ---- 

# this is a long-format match table (relational lookup)
  # and would serve as the backend for a future tool 
  # it would be created fresh for any case study PPUs
# it only includes PCUs that are a match for the case study PPUs

# establish match tolerance
  # we are considering MCMT values within +/- 0.6C to be a match
  # see paper for details
  # this tolerance would be adjusted in a tool
MCMT_tolerance <- 0.6 

match_filt_df <- match_full_df %>% 
  mutate(MCMT_diff = PCU_MCMT - PPU_MCMT) %>% 
  filter(abs(MCMT_diff) <= MCMT_tolerance) %>% 
  select(PPU_ID, PCU_ID, climate_period, PPU_MCMT, PCU_MCMT, MCMT_diff) %>% 
  arrange(PPU_ID, climate_period, PCU_ID) 

# check 
str(match_filt_df) 
nrow(match_filt_df)
  # 373800 rows
  # much less bc only keeping matches
(373800/3324888)*100 # 11.24248% of PCUs across SRME are matches for the case study PPUs

# quick summary: match counts per PPU per climate period (# matching PCUs)
match_summary <- match_filt_df %>%
  group_by(PPU_ID, climate_period) %>%
  summarise(n_matches = n(), .groups = "drop") %>%
  pivot_wider(names_from = climate_period, values_from = n_matches) %>%
  arrange(PPU_ID)

print(match_summary)



# (5) make tables for paper ----

## (A) PPU table ----

# to be included in paper body
# this table will show a summary of median extracted MCMT values for each PPU
  # and also the number of matching PCUs for each PPU

# in addition to climate-match, the case study is also concerned about target species and fire risk
  # we will further filter the PCUs that are ssp2_2041-2070 matches by these additional priorities
    # these priorities would be adjustable in a tool
  # this is a final column for the table, showing the number of PCUs that remain for each PPU after all 3 filters

# check if have NaN
sum(is.nan(SRME_PCUs_df$PIPO_tons))
  # 614
sum(is.nan(SRME_PCUs_df$CFP_prob))
  #0
  
# treat NaN as 0 for PIPO_tons before filtering
SRME_PCUs_df <- SRME_PCUs_df %>%
  mutate(PIPO_tons = ifelse(is.nan(PIPO_tons), 0, PIPO_tons))

# pull relevant PCU attributes for filtered column
PCU_attrs <- SRME_PCUs_df %>%
  select(PCU_ID, PIPO_tons, CFP_prob)

# build filtered match count (ssp2 + PIPO + CFP)
ssp2_PIPO_CFP_filter <- match_filt_df %>%
  filter(climate_period == "ssp2_2041_2070") %>%
  left_join(PCU_attrs, by = "PCU_ID") %>%
  filter(PIPO_tons >= 10,                 # PCU has at least 10 tons/acre of PIPO biomass
         CFP_prob >= 0.5) %>%             # PCU is at least 50% probable to have a crown fire, if a fire were to occur in it (high risk)
  group_by(PPU_ID) %>%
  summarise(n_ssp2_PIPO_CFP = n(), .groups = "drop")

str(ssp2_PIPO_CFP_filter)

# join PPU MCMT values for each period
PPU_MCMT_wide <- PPU_MCMT_df %>%
  select(PPU_ID, ref_MCMT, curr_MCMT, ssp2_MCMT, ssp5_MCMT)

# assemble full summary table
PPU_summary_df <- match_summary %>%
  left_join(ssp2_PIPO_CFP_filter, by = "PPU_ID") %>%
  left_join(PPU_MCMT_wide, by = "PPU_ID") %>%
  select(PPU_ID,
         ref_MCMT, `ref_1961_1990`,
         curr_MCMT, `curr_2011_2040`,
         ssp2_MCMT, `ssp2_2041_2070`,
         ssp5_MCMT, `ssp5_2041_2070`,
         n_ssp2_PIPO_CFP) %>%
  rename(
    n_ref  = `ref_1961_1990`,
    n_curr = `curr_2011_2040`,
    n_ssp2 = `ssp2_2041_2070`,
    n_ssp5 = `ssp5_2041_2070`
  ) %>%
  arrange(PPU_ID)

print(PPU_summary_df)

### write & read ----
write.csv(PPU_summary_df, "./CaseStudy_S4F/PPUs/PPU_summary_df.csv", row.names = FALSE)
PPU_summary_df   <- read.csv("./CaseStudy_S4F/PPUs/PPU_summary_df.csv", check.names = FALSE)


## (B) PCU table ----

# this table/ SpatVector will serve as an intermediate product to show the utility of the relational lookup table
  # bc the relational lookup table is too large (>3 million rows) to show in the paper/ dataset 

# it will be 1) in the supplemental .csv as a tab, and 2) attached as an attribute table to a PCU SpatVector/ shapefile 
  
# it has 8 added match columns for the Case Study (CS) PPUs (in addition to the attributes created in Part 2)
  # 1 column with the number of matching PPUs for each clim period/scenario
  # 1 column with a list of matching PPUs for each clim period/scenario
    # ref, curr, ssp2, and ssp5
  

# build count and list summaries from match_filt_df
PCU_match_wide <- match_filt_df %>%
  group_by(PCU_ID, climate_period) %>%
  summarise(
    n_match  = n_distinct(PPU_ID),
    PPU_list = paste(sort(unique(PPU_ID)), collapse = ", "),
    .groups  = "drop"
  ) %>%
  pivot_wider(
    names_from  = climate_period,
    values_from = c(n_match, PPU_list),
    names_glue  = "{climate_period}_{.value}"
  ) %>%
  rename(
    ref_match  = ref_1961_1990_n_match,
    ref_list   = ref_1961_1990_PPU_list,
    curr_match = curr_2011_2040_n_match,
    curr_list  = curr_2011_2040_PPU_list,
    ssp2_match = ssp2_2041_2070_n_match,
    ssp2_list  = ssp2_2041_2070_PPU_list,
    ssp5_match = ssp5_2041_2070_n_match,
    ssp5_list  = ssp5_2041_2070_PPU_list
  ) %>%
  # replace NAs with 0 for count columns only
  mutate(across(ends_with("_match"), ~ replace_na(., 0))) %>% 
  select(PCU_ID, ref_match, ref_list, curr_match, curr_list, 
         ssp2_match, ssp2_list, ssp5_match, ssp5_list)



### join back to full PCU dataframe ----
SRME_PCUs_CS_df <- SRME_PCUs_df %>%
  left_join(PCU_match_wide, by = "PCU_ID")

# check
str(SRME_PCUs_CS_df)
nrow(SRME_PCUs_CS_df) 
  # should still be 46179


#### write & read ----
write.csv(SRME_PCUs_CS_df, "./CaseStudy_S4F/PCUs/SRME_PCUs_CS_df.csv", row.names = FALSE)
SRME_PCUs_CS_df <- read.csv("SRME_PCUs_CS_df.csv", check.names = FALSE)


### join back to full PCU SpatVector ----
# select only new cols and ID
PCU_matches <- SRME_PCUs_CS_df %>% 
  select(PCU_ID, ref_match, ref_list, curr_match, curr_list, 
         ssp2_match, ssp2_list, ssp5_match, ssp5_list)

SRME_PCUs_CS_vect <- SRME_PCUs_vect %>% 
  left_join(PCU_matches, by = "PCU_ID")

# check
PCU_matches_df <- as.data.frame(SRME_PCUs_CS_vect)
str(PCU_matches_df)


#### write & read ----
writeVector(SRME_PCUs_CS_vect, "./CaseStudy_S4F/PCUs/SRME_PCUs_CS_vect.shp")
SRME_PCUs_CS_vect <- vect("./CaseStudy_S4F/PCUs/SRME_PCUs_CS_vect.shp")



## (C) universal PCU? ----

# find PCUs that match ALL 18 PPUs under ssp2
universal_ssp2 <- match_filt_df %>%
  filter(climate_period == "ssp2_2041_2070") %>%
  group_by(PCU_ID) %>%
  summarise(n_PPUs_matched = n_distinct(PPU_ID), .groups = "drop") %>%
  filter(n_PPUs_matched == 18) %>%
  arrange(PCU_ID)

nrow(universal_ssp2)
# 0 !!!!!!!!!! see notes for interpretation 

# if there were any "universal matches", which would also meet PIPO and CFP thresholds?
universal_ssp2_filtered <- universal_ssp2 %>%
  left_join(PCU_attrs, by = "PCU_ID") %>%
  filter(PIPO_tons >= 10,
         CFP_prob >= 0.5)

nrow(universal_ssp2_filtered)

















