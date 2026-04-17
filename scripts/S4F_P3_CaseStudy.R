# Seeds 4 the Future - Prioritizing cone collection for future-focused reforestation 

# Part 3: creation of Potential Planting Units (PPUs) for the case study

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

# (2) make PPU polygons ----
# we create potential planting units (PPUs)
# using the FACTS needs polys from within the Cameron Peak fire boundary
# last downloaded on August 3rd, 2025
# we divided the FACTS needs into small (50-200 acre) polygons
# then assigned a 500 ft elevation band (EB) to each
# and we are just using the 8500-9000 ft EB for this part of the case study
# we will extract the future clim from these needs in Part 2

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
# this is essentially all the Cameron Peak (CP) fire needs in 1 poly

Cameron_big_need_poly <- needs_all %>%
  select(all_of(desired_cols)) %>% 
  filter(FACTS_ID %in% desired_ids) 

## project ---- 
Cameron_big_need_poly <- project(Cameron_big_need_poly, "EPSG:5070")
plot(Cameron_big_need_poly)
expanse(Cameron_big_need_poly)* 0.000247105
# has 1 geometry (one large multipart polygon)

## process ----
### disaggregate ----
Cameron_disagg_need_poly <- disagg(Cameron_big_need_poly)
# has 362 geometries

# add an ID col
Cameron_disagg_need_poly$disagg_ID <- 1:nrow(Cameron_disagg_need_poly)

### separate sizes ----
# calc area (default in m^2) & convert to acres
Cameron_disagg_need_poly$area_disagg <- expanse(Cameron_disagg_need_poly) * 0.000247105

# filt out small poys (< 20 acres)
small_polys_removed <- Cameron_disagg_need_poly[Cameron_disagg_need_poly$area_disagg >= 20, ]
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
Cameron_PPUs_vect <- rbind(mid_polys, divided_polys_vect)
# 434 geoms

### adjust ----
# add new ID col & new final area col
Cameron_PPUs_vect$PPU_ID <- 1:nrow(Cameron_PPUs_vect)
Cameron_PPUs_vect$area_acres <- expanse(Cameron_PPUs_vect) * 0.000247105

summary(Cameron_PPUs_vect$area_acres)
# min = 20.68, max = 203.97
sum(Cameron_PPUs_vect$area_acres) # 49151.23 acres
sum(small_polys_removed$area_disagg) # 49151.23 acres
# bc these are =, we know the divide function worked (retained all area)

# select only new ID and area
Cameron_PPUs_vect <- Cameron_PPUs_vect[, c("PPU_ID", "area_acres")]
plot(Cameron_PPUs_vect)


## add Elv ----
# using the DEM created in part 1A_3b
ARNF_DEM_rast <- rast("./ArapahoRooseveltNF_S4F/.tif/ARNF_DEM_rast.tif")

# the DEM is in meters --> convert m to ft
meters_to_feet_factor <- 3.28084
ARNF_DEM_ft <- ARNF_DEM_rast * meters_to_feet_factor 
summary(ARNF_DEM_ft) # min = 5374, max = 14030   

# extract median
Elv_med_df <- extract(ARNF_DEM_ft, Cameron_PPUs_vect, fun=median)
str(Elv_med_df)
# rename col
Elv_med_df <- Elv_med_df %>% 
  rename(Elv_med_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PPU_ID = Cameron_PPUs_vect$PPU_ID) %>% 
  select(-1)

# add to spatvector
Cameron_PPUs_vect <- Cameron_PPUs_vect %>% 
  left_join(Elv_med_df, by = "PPU_ID")

### write & read ----
writeVector(Cameron_PPUs_vect, "./CaseStudy_S4F/PPUs_shp/Cameron_PPUs_vect.shp")
Cameron_PPUs_vect <- vect("./CaseStudy_S4F/PPUs_shp/Cameron_PPUs_vect.shp")


## filter ----
# for the case study, we are only going to use the planting needs (PPUs)
# that are within the 9000 - 9500 ft EB
PPUs_9000_9500_vect <- Cameron_PPUs_vect %>% 
  filter(Elv_med_ft >= 9000, Elv_med_ft <= 9500)
# 86 geoms
# too many for the case study - pick a subset of spatially close polys
# using Arc to visualize and choose (this is subjective)

CaseStudy_PPUs <- c("223", "225", "232", "235", "239", "281", "282", "283", "286", "287", "288", # just 11
                    "7", "14", "15", "356", "357", "359", "401") # 18

CaseStudy_PPUs_vect <- Cameron_PPUs_vect %>% 
  filter(PPU_ID %in% CaseStudy_PPUs)
# 18 geoms

sum(CaseStudy_PPUs_vect$area_acres) # 2272.514 acres

### write & read ----
writeVector(CaseStudy_PPUs_vect, "./CaseStudy_S4F/PPUs_shp/CaseStudy_PPUs_vect.shp")
CaseStudy_PPUs_vect <- vect("./CaseStudy_S4F/PPUs_shp/CaseStudy_PPUs_vect.shp")



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
  
# merge with previous PPU spatvector, to have previous attributes
CS_PPUs_vect <- CaseStudy_PPUs_vect %>% 
  left_join(PPU_MCMT_df, by = "PPU_ID")

CS_PPUs_df <- as.data.frame(CS_PPUs_vect)
str(CS_PPUs_df)

## write & read ----
write.csv(CS_PPUs_df, "./CaseStudy_S4F/PPUs_shp/CS_PPUs_df.csv", row.names = FALSE)
# writeVector(CS_PPUs_vect, "./CaseStudy_S4F/PPUs_shp/CS_PPUs_vect.shp")
# CS_PPUs_vect <- vect("./CaseStudy_S4F/PPUs_shp/CS_PPUs_vect.shp")




# (4) match clims ----
# bc we are only using 1 climate variable (MCMT), we're calling this temperature-matching

# the general concept is that we will use the extracted median MCMT across each PPU (this script Part 3, step 3B)
# and see if that value "matches" the extracted median MCMT across each PCU (script for Part 2, step 3C)

# to incorporate variation, but stay within a biologically-relevant range (see paper for details),
# we consider any values for a given polygon pair to be "matched" if their median MCMT values are within +/- 0.6 degree C of each other
# match if abs(PPU_MCMT - PCU_MCMT) ≤ 0.6 (°C)

# for PCUs, the median MCMT is only for the reference period (1961-1990) - representing conditions which parent trees of collected seed evolved under
# for PPUs, we use median MCMT for all 4 periods/scenarios (see above) - representing different possible conditions at the planting site 

library(dplyr)
library(tidyr)
library(purrr)

# --- Parameters ---
tol <- 0.6

# --- Build a single long PPU table with period labels ---
# If area_acres and Elv_med_ft exist only in one PPU df, take them from ref_MCMT_PPU_df.
PPU_meta <- ref_MCMT_PPU_df %>%
  select(PPU_ID, area_acres, Elv_med_ft) %>%
  distinct()

PPU_long <- list(
  ref  = ref_MCMT_PPU_df,
  curr = curr_MCMT_PPU_df,
  ssp2 = ssp2_MCMT_PPU_df,
  ssp5 = ssp5_MCMT_PPU_df
) %>%
  imap_dfr(~ .x %>%
             transmute(
               PPU_ID,
               PPU_MCMT = PPU_median,
               clim = .y
             )) %>%
  mutate(clim = factor(clim, levels = c("ref","curr","ssp2","ssp5")))

# --- Minimal PCU table ---
PCU_min <- ref_MCMT_PCU_df %>%
  transmute(
    PCU_ID,
    PCU_MCMT = PCU_median
  )

# --- Lookup table: all (PPU × PCU × period) with match = 0/1 ---
lookup_df <- tidyr::crossing(
  PPU_long %>% select(PPU_ID, clim, PPU_MCMT),
  PCU_min     %>% select(PCU_ID, PCU_MCMT)
) %>%
  mutate(
    match = as.integer(abs(PPU_MCMT - PCU_MCMT) <= tol)
  ) %>%
  select(PPU_ID, PCU_ID, clim, match)

# Optional sanity check for row count:
# stopifnot(nrow(lookup_df) == length(levels(PPU_long$clim)) * nrow(PPU_meta) * nrow(PCU_min))

# --- Summary table: per PPU, counts of matching PCUs by period + requested metadata ---
match_summary <- lookup_df %>%
  group_by(PPU_ID, clim) %>%
  summarise(n_PCUs = sum(match), .groups = "drop") %>%
  pivot_wider(
    names_from = clim,
    values_from = n_PCUs,
    values_fill = 0
  ) %>%
  left_join(PPU_meta, by = "PPU_ID") %>%
  select(PPU_ID, area_acres, Elv_med_ft, ref, curr, ssp2, ssp5)

# --- Quick verification examples (optional) ---
nrow(lookup_df)              # should be 18 * 4148 * 4 = 298,656 in your case
lookup_df %>%
  filter(PPU_ID == 7, clim == "ref", match == 1) %>%
  nrow()                     # should reproduce your check (e.g., 1250)

# Final outputs:
# lookup_df   # big "lookup" table with IDs, period, and 0/1 match
# match_summary  # compact table: one row per PPU with counts in ref/curr/ssp2/ssp5 + area_acres + Elv_med_ft




  


  






