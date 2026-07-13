# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 3: creation of Potential Planting Units (PPUs) for the case study

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)
library(ggplot2)

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
  # 86 geoms is too many for the case study
  # choosing a subset that are geographically near each other (for visual & operational reasons)
    # using Arc to visualize and choose (this is subjective)
  # choosing 14 units that are 
    # also climatically "near" each other
    # they have a median extracted MCMT value for mid-century ssp2
    # that ranges by less than +/- 0.6 (our match tolerance, an objective grouping method)
    # using this threshold bc we want to generate list of "universal" PCUs (see Part 3-5C below)

CaseStudy_PPUs <- c("7", "15", "225", "232", "239", "281", "282", "283", "286", 
                    "287", "288", "356", "357", "359") 

CaseStudy_PPUs_vect <- CP_PPUs_vect %>% 
  filter(PPU_ID %in% CaseStudy_PPUs)
  # 14 geoms

sum(CaseStudy_PPUs_vect$area_acres) # 1782.683 acres

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
  


# (4) match clims with PCUs ----


## (A) prep PPUs  ---- 
  # PPU MCMT to long format
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
  # with 18 PCUs, expect: 72 rows (18 PPUs x 4 periods), 3 columns
  # with 14 PCUs, expect: 56 rows (14 PPUs x 4 periods), 3 columns 


## (B) prep PCUs  ---- 

### read ----
# PCUs were created in part 1, and attributes were added in part 2
# there are 46179 total PCUs across all 10 NFs in the SRME

SRME_PCUs_vect <- vect("./SRME_S4F/.shp/SRME_PCUs_vect.shp")
SRME_PCUs_df <- as.data.frame(SRME_PCUs_vect)
str(SRME_PCUs_df)

### adjust ----

# treat NaN as 0 for PIPO_tons (biologically: no ponderosa detected)
# check if have NaN
sum(is.nan(SRME_PCUs_df$PIPO_tons))
# 614
sum(is.nan(SRME_PCUs_df$CFP_prob))
#0

# treat NaN as 0 for PIPO_tons before filtering
SRME_PCUs_df <- SRME_PCUs_df %>%
  mutate(PIPO_tons = ifelse(is.nan(PIPO_tons), 0, PIPO_tons))

# pull relevant PCU attributes for filtered column (5-B)
PCU_attrs <- SRME_PCUs_df %>%
  select(PCU_ID, PIPO_tons, CFP_prob, seedlot_A, seedlot_B)

# pull MCMT for cross-join (4-C)
PCU_MCMT_df <- SRME_PCUs_df %>% 
  select(PCU_ID, MCMT_C) %>% 
  rename(PCU_MCMT = MCMT_C)

str(PCU_MCMT_df)


## (C) cross join ----  
match_full_df <- PPU_MCMT_long %>% 
  cross_join(PCU_MCMT_df) 

# check 
nrow(match_full_df) 
# with 18 PCUs, expect 3324888 rows (18 PPUs x 4 periods x 46179 PCUs) 
# with 14 PCUs, expect 2586024 rows (14 PPUs x 4 periods x 46179 PCUs) 


## (D) calculate MCMT difference ---- 
  # and filter to matches

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
  # with 18 PCUs, expect 373800 rows
  # with 14 PCUs, expect 291695 rows

  # much less bc only keeping matches
(291695/2586024)*100 # 11.27967% of PCUs across SRME are matches for the case study PPUs

# quick summary: match counts per PPU per climate period (# matching PCUs)
match_summary <- match_filt_df %>%
  group_by(PPU_ID, climate_period) %>%
  summarise(n_matches = n(), .groups = "drop") %>%
  pivot_wider(names_from = climate_period, values_from = n_matches) %>%
  arrange(PPU_ID)

print(match_summary)



# (5) visuals for paper ----


## (A) filter fig ----

### filter 2 ----
# find PCUs that match ALL 14 PPUs under ssp2
n_PPUs_CS <- n_distinct(PPU_MCMT_long$PPU_ID)  # 14, avoids hardcoding

# apply filter
filter_2 <- match_filt_df %>%
  filter(climate_period == "ssp2_2041_2070") %>%
  group_by(PCU_ID) %>%
  summarise(n_PPUs_matched = n_distinct(PPU_ID), .groups = "drop") %>%
  filter(n_PPUs_matched == n_PPUs_CS) %>%
  arrange(PCU_ID)

nrow(filter_2)
# with 18 PPUs, 0 !!!!!!!!!! see notes for interpretation 
# with 14 PPUs, 177 PCUs are universal matches! 


### filter 3 ----
# filter universal matches by all three management objectives:
  # target species (PIPO_tons), fire risk (CFP_prob), nursery inventory (seedlot_A)
  # these objectives would be adjustable in a tool

filter_3 <- filter_2 %>%
  left_join(PCU_attrs, by = "PCU_ID") %>%
  filter(PIPO_tons >= 10,           # at least 10 tons/acre of PIPO biomass
         CFP_prob >= 0.5,           # at least 50% probable to have a crown fire
         is.na(seedlot_A)) %>%      # no overlap with existing nursery seedlot inventory
  arrange(PCU_ID)

nrow(filter_3)
# with 14 PPUs, 50 PCUs are universal matches AND meet all management objectives


## (B) PPU table ----

# to be included in paper body
# this table will show a summary of median extracted MCMT values for each PPU
  # and also the number of matching PCUs for each PPU
  # and also a final filtered count after filter 2 & 3

# build filtered match count (ssp2 + PIPO + CFP + seedlot)
n_filt3_column <- match_filt_df %>%
  filter(climate_period == "ssp2_2041_2070") %>%
  left_join(PCU_attrs, by = "PCU_ID") %>%
  filter(PIPO_tons >= 10,
         CFP_prob >= 0.5,
         is.na(seedlot_A)) %>%
  group_by(PPU_ID) %>%
  summarise(n_filt3 = n(), .groups = "drop")

str(n_filt3_column)


# join PPU MCMT values for each period
PPU_MCMT_wide <- PPU_MCMT_df %>%
  select(PPU_ID, ref_MCMT, curr_MCMT, ssp2_MCMT, ssp5_MCMT)

# assemble full summary table
PPU_summary_df <- match_summary %>%
  left_join(n_filt3_column, by = "PPU_ID") %>%
  left_join(PPU_MCMT_wide, by = "PPU_ID") %>%
  select(PPU_ID,
         ref_MCMT, `ref_1961_1990`,
         curr_MCMT, `curr_2011_2040`,
         ssp2_MCMT, `ssp2_2041_2070`,
         ssp5_MCMT, `ssp5_2041_2070`,
         n_filt3) %>%
  rename(
    n_ref  = `ref_1961_1990`,
    n_curr = `curr_2011_2040`,
    n_ssp2 = `ssp2_2041_2070`,
    n_ssp5 = `ssp5_2041_2070`
  ) %>%
  arrange(PPU_ID)

print(PPU_summary_df)


### write & read ----
write.csv(PPU_summary_df, "./CaseStudy_S4F/paper_tables/PPU_summary_df.csv", row.names = FALSE)
PPU_summary_df   <- read.csv("./CaseStudy_S4F/paper_tables/PPU_summary_df.csv", check.names = FALSE)


## (C) PCU table ----

# this table/ SpatVector will serve as an intermediate product to show the utility 
  # of the relational lookup table (too large; >3 million rows to show in the paper/ dataset)

# it will be 1) in the supplemental .csv as a tab, and 2) attached as an attribute table to a PCU SpatVector/ shapefile 
  
# it has 8 added match columns for the Case Study (CS) PPUs (in addition to the attributes created in Part 2)
  # 1 column with the number of matching PPUs for each clim period/scenario (n_match)
  # 1 column with a list of matching PPUs for each clim period/scenario (PPU_list)
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
write.csv(SRME_PCUs_CS_df, "./CaseStudy_S4F/paper_tables/SRME_PCUs_CS_df.csv", row.names = FALSE)
SRME_PCUs_CS_df <- read.csv("./CaseStudy_S4F/paper_tables/SRME_PCUs_CS_df.csv", check.names = FALSE)


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


## (D) target species fig ----
# this is how we are selecting the target species for filter 3 (above)

### (a) import veg data ----
# LANDFIRE EVT = existing vegetation (PCU side: what's there now)
# LANDFIRE BPS = biophysical setting (PPU side: historic/potential veg for planting site)

# EVT
EVT_CONUS_rast <- rast("LC24_EVT_250.tif")
# already in EPSG:5070 (no need to project)
is.factor(EVT_CONUS_rast) # TRUE
activeCat(EVT_CONUS_rast) <- "EVT_NAME"

# BPS
BPS_CONUS_rast <- rast("LC20_BPS_220.tif")
# already in EPSG:5070 (no need to project)
is.factor(BPS_CONUS_rast) # TRUE
activeCat(BPS_CONUS_rast) <- "BPS_NAME"


### (b) build curr-specific universal match ----
# filter_2 (Part 5-A) is already the ssp2 universal match (177 PCUs, all 14 PPUs)
# here we build the analogous "curr" version, following the same "universal" logic

PCU_curr_match <- match_filt_df %>%
  filter(climate_period == "curr_2011_2040") %>%
  group_by(PCU_ID) %>%
  summarise(n_PPUs_matched = n_distinct(PPU_ID), .groups = "drop") %>%
  filter(n_PPUs_matched == n_PPUs_CS) %>%
  arrange(PCU_ID)

nrow(PCU_curr_match) # 385 PCUs are "universal" matches for all 14 PPUs


### (c) subset PCU spatvector to matched pools ----
# no longer separating ARP vs. SRME - all matching is against the full SRME PCU pool already
PCUs_matched_curr_vect <- SRME_PCUs_CS_vect %>% 
  filter(PCU_ID %in% PCU_curr_match$PCU_ID)

PCUs_matched_ssp2_vect <- SRME_PCUs_CS_vect %>% 
  filter(PCU_ID %in% filter_2$PCU_ID)


### (d) crop/mask veg rasters to relevant polygons ----

# EVT, curr-matched PCUs
EVT_curr_rast <- crop(EVT_CONUS_rast, PCUs_matched_curr_vect, mask = TRUE)
plot(EVT_curr_rast)
# quick count of distinct EVT categories present
freq(EVT_curr_rast) %>% nrow() # 30

# EVT, ssp2-matched PCUs
EVT_ssp2_rast <- crop(EVT_CONUS_rast, PCUs_matched_ssp2_vect, mask = TRUE)
plot(EVT_ssp2_rast)

# BPS, PPUs
BPS_PPU_rast <- crop(BPS_CONUS_rast, CaseStudy_PPUs_vect, mask = TRUE)
plot(BPS_PPU_rast)


### (e) build veg composition tables ----
# helper function: freq table -> % composition, harmonized column names
# note: BPS and EVT category columns share the "EVT_NAME" label here intentionally,
# so the three tables can be bound together for plotting

veg_freq_table <- function(rast, group_label) {
  as.data.frame(freq(rast)) %>%
    mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
           Group = factor(group_label)) %>%
    rename(EVT_NAME = value) %>%
    arrange(desc(REL_PERCENT)) %>%
    select(Group, EVT_NAME, REL_PERCENT)
}

PPU_BpS_df       <- veg_freq_table(BPS_PPU_rast, "PPU_BpS")
PCU_EVT_curr_df  <- veg_freq_table(EVT_curr_rast, "PCU_EVT_curr")
PCU_EVT_ssp2_df  <- veg_freq_table(EVT_ssp2_rast, "PCU_EVT_ssp2")


### (f) combine ----

veg_combined_df <- bind_rows(PPU_BpS_df,
                             PCU_EVT_curr_df,
                             PCU_EVT_ssp2_df)

# lump rare veg types (< 1%) into "Other"
veg_lumped <- veg_combined_df %>%
  mutate(EVT_name_lumped = if_else(REL_PERCENT < 1, "Other", EVT_NAME)) %>%
  group_by(Group, EVT_name_lumped) %>%
  summarise(REL_PERCENT = sum(REL_PERCENT), .groups = "drop")

# compute global totals per lumped name for legend/stack order
lvl_order <- veg_lumped %>%
  group_by(EVT_name_lumped) %>%
  summarise(total_rel = sum(REL_PERCENT), .groups = "drop") %>%
  arrange(desc(total_rel)) %>%
  pull(EVT_name_lumped)

# ensure clean types
veg_lumped <- veg_lumped %>%
  mutate(
    EVT_name_lumped = factor(EVT_name_lumped, levels = lvl_order),
    Group = factor(Group, levels = c("PPU_BpS", "PCU_EVT_curr", "PCU_EVT_ssp2")),
    REL_PERCENT = as.numeric(REL_PERCENT)
  )

# check each group sums to 100
veg_lumped %>%
  group_by(Group) %>%
  summarise(total = sum(REL_PERCENT), .groups = "drop")


### (g) table ----
# wide-format version of veg_lumped for supplemental materials
# rows = veg type (lumped), columns = each group, values = % composition

veg_table_supp <- veg_lumped %>%
  pivot_wider(names_from = Group, values_from = REL_PERCENT, values_fill = 0) %>%
  mutate(`Vegetation Type` = factor(EVT_name_lumped, levels = lvl_order)) %>%
  arrange(`Vegetation Type`) %>%
  select(`Vegetation Type`, PPU_BpS, PCU_EVT_curr, PCU_EVT_ssp2)

print(veg_table_supp)


# check: each column (group) should sum to 100
veg_table_supp %>%
  summarise(across(where(is.numeric), sum))

### write ----
write.csv(veg_table_supp, "./CaseStudy_S4F/paper_tables/veg_table_supp.csv", row.names = FALSE)


### (h) plot ----
veg_plot <- 
  ggplot(veg_lumped, aes(x = Group, y = REL_PERCENT, 
                         fill = EVT_name_lumped)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", width = 0.9) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_d(option = "H", direction = 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = "Relative Percent", fill = "Vegetation Type") +
  theme_minimal()

veg_plot



# (6) match clims with SLs ----
# we will do the same, but matching the PPUs with seedlots (SLs) in the nursery inventory












