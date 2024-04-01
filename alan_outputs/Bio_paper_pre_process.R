###### Bio_paper_pre_process.R
# apply specified protected area from a raster file to moirai land distribution
#    based on 2015 moirai land distribution
# create a gcamdata file that specifies the desired protection (e.g., L120.LC_prot_land_frac_GLU_default.csv)
#    this technically is gcam unavailable land for conversion (protected plus unsuitable unprotected)
#    all years will have the same protected and non-protected fractions?
#    or the other years can be scaled accordingly?
#    only 6 gcam land types have protection info: forest, grassland, pasture, rockicedesert, shrubland, tundra
# do this for several scenarios:
#    allan scen 1 - miniumum area for protecting biodiversity
#    allan scen 2 - protecting biodiversity away from humans
#    30% uniform protection
#    default gcam unavailable - current protected (suitable and unsuitable) plus unsuitable unprotected
# these are applied as protection only and also overlayed with the default gcam unavailable



library(dplyr)
library(tidyr)
library(raster)
library(ggplot2)
library(viridis)

setwd("./")

# can make plots of existing output files by reading them in and using the plot code at the end of this script

# Read in total hyde land area (without water or antarctica)

# this is a moirai diagnostic output
# no-data values are -9999; these correspond to water pixels, effectively incorporating a land mask
# set these pixels to zero for processing purposes
# units are km^2
land_hyde_raster = raster("mapping/land_area_hyde.bil")
land_hyde <- as.data.frame(rasterToPoints(land_hyde_raster))
colnames(land_hyde)<- c("x","y","land_area_km2")
land_hyde$land_area_km2[land_hyde$land_area_km2 == -9999] = 0

# this is currently 129930555 km^2
total_hyde_land_area = sum(land_hyde$land_area_km2)
cat(paste0("Global hyde land area in km^2 (no antarctica) = ", total_hyde_land_area))

# read in hyde cell area - this is also an effective land mask
# antarctica is not included

# this is a moirai diagnostic output
# no-data values are -9999; these correspond to water pixels, effectively incorporating a land mask
# set these pixels to zero for processing purposes
# create a land mask first
# units are km^2
cell_hyde_raster = raster("mapping/cell_area_hyde.bil")
cell_hyde <- as.data.frame(rasterToPoints(cell_hyde_raster))
colnames(cell_hyde)<- c("x","y","cell_area_km2")
cell_hyde$land_mask = 0
cell_hyde$land_mask[which(cell_hyde$cell_area_km2 > -9999)] = 1
cell_hyde$cell_area_km2[cell_hyde$cell_area_km2 == -9999] = 0
cell_hyde_rast = cell_hyde_raster
cell_hyde_rast[Which(cell_hyde_raster <= 0 ,cells=TRUE)] = 0

# this is currently 129930555 km^2
total_hyde_cell_area_masked = sum(cell_hyde$cell_area_km2)
cat(paste0("Global hyde cell area for land pixels in km^2 (no antarctica) = ", total_hyde_cell_area_masked))

# merge the hyde land and cell area data
hyde = left_join(land_hyde, cell_hyde, by=c("x", "y"))

# round x and y to 3 digits for consistency with all data for joining/merging
hyde = mutate(hyde, x=round(x,3),y=round(y,3))

##### use hyde cell and land area (no antarctica)!!!
# WDPA does not include antarctica in terrestrial protected area or total global land area
#    see: https://www.protectedplanet.net/en/resources/calculating-protected-area-coverage
# allan data include antarctica area km^2 from paper SM:
antarctica_area = 12078023
# there are some high latitide allan protected cells that are not in the hyde land mask
# use 60 deg south as the cutoff for antarctica
antarctica_threshold = -60

land_mask_rast = cell_hyde_raster
land_mask_rast[] = 0
land_mask_rast[Which(cell_hyde_raster > -9999,cells=TRUE)] = 1


##### read in the gcamdata file to replace
# this provides the output format for the final csv files

# this is a gcamdata file with the default land availability
gcamdata_default <- read.csv("L120.LC_prot_land_frac_GLU_default.csv", skip=1)
gcamdata_cols = names(gcamdata_default)
gcamdata_years = sort(unique(gcamdata_default$year))
gcamdata_regions = sort(unique(gcamdata_default$GCAM_region_ID))
gcamdata_glus = sort(unique(gcamdata_default$GLU))
gcamdata_landtypes = sort(unique(gcamdata_default$Land_Type))


##### note that the allan data have some additional cells compared to hyde/moirai

## Allan scenario 1 data - minimun area required
Allan_sce1_rast  <- raster("rasters_Allan_et_al/Allan_cat_sce_data_5arcmin.tif")

# look at difference in land cells
md_land_sce1 = land_mask_rast - Allan_sce1_rast
plot(md_land_sce1)

Allan_sce1_data <- as.data.frame(rasterToPoints(Allan_sce1_rast))
colnames(Allan_sce1_data)<- c("x","y","protected_cell")
# all cell area
Allan_sce1_area <- as.data.frame(rasterToPoints(area(Allan_sce1_rast)))
colnames(Allan_sce1_area)<- c("x","y","all_cell_area_km2")
Allan_sce1_data$protected_all_cell_area_km2 = Allan_sce1_data$protected_cell
Allan_sce1_data %>% 
  left_join(Allan_sce1_area) %>% 
  mutate(protected_all_cell_area_km2 = protected_all_cell_area_km2 * all_cell_area_km2)->Allan_sce1_data

# round x and y to 3 digits
Allan_sce1_data = mutate(Allan_sce1_data, x=round(x,3),y=round(y,3))

# hyde cell and land area
Allan_sce1_data$protected_hyde_cell_area_km2 = Allan_sce1_data$protected_cell
Allan_sce1_data$protected_hyde_land_area_km2 = Allan_sce1_data$protected_cell
Allan_sce1_data %>% 
  left_join(hyde) %>% 
  mutate(protected_hyde_cell_area_km2 = protected_hyde_cell_area_km2 * cell_area_km2)->Allan_sce1_data
Allan_sce1_data$protected_hyde_land_area_km2 = Allan_sce1_data$protected_hyde_land_area_km2 * Allan_sce1_data$land_area_km2

# cell area stats
total_allan_scen1_protected_all_cell_area = sum(Allan_sce1_data$protected_all_cell_area_km2)
total_allan_scen1_protected_hyde_cell_area = sum(Allan_sce1_data$protected_hyde_cell_area_km2)
cat(paste0("For comparison:\nallan scen 1 protected all cell area in km^2 (with antarctica) = ", total_allan_scen1_protected_all_cell_area))
cat(paste0("allan scen 1 protected all cell percent area (est. with hyde cell area and antarctica) = ",
            100 * total_allan_scen1_protected_all_cell_area / (sum(Allan_sce1_data$cell_area_km2) + antarctica_area) ))
cat(paste0("allan scen 1 protected hyde cell area in km^2 (no antarctica) = ", total_allan_scen1_protected_hyde_cell_area))
cat(paste0("allan scen 1 protected hyde cell percent area (no antarctica) = ", 100 * total_allan_scen1_protected_hyde_cell_area / sum(Allan_sce1_data$cell_area_km2)))

# subtract the protected cell area in antarctica
antarctica_allan_scen1_protected_all_cell_area = sum(Allan_sce1_data$protected_all_cell_area_km2[Allan_sce1_data$y < antarctica_threshold])
total_no_ant_allan_scen1_protected_all_cell_area = total_allan_scen1_protected_all_cell_area - antarctica_allan_scen1_protected_all_cell_area
cat(paste0("allan scen 1 protected all cell area in km^2 (no antarctica) = ", total_no_ant_allan_scen1_protected_all_cell_area))
cat(paste0("allan scen 1 protected all cell percent area (est. with hyde cell area and no antarctica) = ",
            100 * total_no_ant_allan_scen1_protected_all_cell_area / (sum(Allan_sce1_data$cell_area_km2) ) ))
            
# estimate the protected cell area not in the hyde land mask and not in antarctica
lost_allan_scen1_protected_all_cell_area = total_allan_scen1_protected_all_cell_area - total_no_ant_allan_scen1_protected_all_cell_area
cat(paste0("allan scen 1 lost protected all cell area in km^2 (no antarctica) = ", lost_allan_scen1_protected_all_cell_area))
cat(paste0("allan scen 1 lost protected all cell percent of non-antarctic protected all cell area  = ",
            100 * lost_allan_scen1_protected_all_cell_area / total_no_ant_allan_scen1_protected_all_cell_area ) )

# land area stats
total_allan_scen1_protected_hyde_land_area = sum(Allan_sce1_data$protected_hyde_land_area_km2)
total_allan_scen1_unprotected_hyde_land_area = total_hyde_land_area - total_allan_scen1_protected_hyde_land_area
cat(paste0("\nTotal allan scen 1 unprotected land area in km^2 (no antarctica) = ", total_allan_scen1_unprotected_hyde_land_area))
cat(paste0("Total allan scen 1 unprotected percent land area (no antarctica) = ", 100 * total_allan_scen1_unprotected_hyde_land_area / total_hyde_land_area))
cat(paste0("Total allan scen 1 protected land area in km^2 (no antarctica) = ", total_allan_scen1_protected_hyde_land_area))
cat(paste0("Total allan scen 1 protected percent land area (no antarctica) = ", 100 * total_allan_scen1_protected_hyde_land_area / total_hyde_land_area))



## Allan scenario 2 data - minimizing nearness to human land use (costs of protection are higher near current use)
Allan_sce2_rast <- raster("rasters_Allan_et_al/Allan_cat_sce2_5arcmin.tif")

Allan_sce2_data <- as.data.frame(rasterToPoints(Allan_sce2_rast))
colnames(Allan_sce2_data)<- c("x","y","protected_cell")

# all cell area
Allan_sce2_area <- as.data.frame(rasterToPoints(area(Allan_sce2_rast)))
colnames(Allan_sce2_area)<- c("x","y","all_cell_area_km2")
Allan_sce2_data$protected_all_cell_area_km2 = Allan_sce2_data$protected_cell
Allan_sce2_data %>% 
  left_join(Allan_sce2_area) %>% 
  mutate(protected_all_cell_area_km2 = protected_all_cell_area_km2 * all_cell_area_km2)->Allan_sce2_data

# round x and y to 3 digits
Allan_sce2_data = mutate(Allan_sce2_data, x=round(x,3),y=round(y,3))

# hyde cell and land area
Allan_sce2_data$protected_hyde_cell_area_km2 = Allan_sce2_data$protected_cell
Allan_sce2_data$protected_hyde_land_area_km2 = Allan_sce2_data$protected_cell
Allan_sce2_data %>% 
  left_join(hyde) %>% 
  mutate(protected_hyde_cell_area_km2 = protected_hyde_cell_area_km2 * cell_area_km2)->Allan_sce2_data
Allan_sce2_data$protected_hyde_land_area_km2 = Allan_sce2_data$protected_hyde_land_area_km2 * Allan_sce2_data$land_area_km2

# cell area stats
total_allan_scen2_protected_all_cell_area = sum(Allan_sce2_data$protected_all_cell_area_km2)
total_allan_scen2_protected_hyde_cell_area = sum(Allan_sce2_data$protected_hyde_cell_area_km2)
cat(paste0("For comparison:\nallan scen 2 protected all cell area in km^2 (no antarctica) = ", total_allan_scen2_protected_all_cell_area))
cat(paste0("allan scen 2 protected all cell percent area (est. with hyde cell area and antarctica) = ",
            100 * total_allan_scen2_protected_all_cell_area / (sum(Allan_sce2_data$cell_area_km2) + antarctica_area) ))
cat(paste0("allan scen 2 protected hyde cell area in km^2 (no antarctica) = ", total_allan_scen2_protected_hyde_cell_area))
cat(paste0("allan scen 2 protected hyde cell percent area (no antarctica) = ", 100 * total_allan_scen2_protected_hyde_cell_area / sum(Allan_sce2_data$cell_area_km2)))

# subtract the protected cell area in antarctica
antarctica_allan_scen2_protected_all_cell_area = sum(Allan_sce2_data$protected_all_cell_area_km2[Allan_sce2_data$y < antarctica_threshold])
total_no_ant_allan_scen2_protected_all_cell_area = total_allan_scen2_protected_all_cell_area - antarctica_allan_scen2_protected_all_cell_area
cat(paste0("allan scen 2 protected all cell area in km^2 (no antarctica) = ", total_no_ant_allan_scen2_protected_all_cell_area))
cat(paste0("allan scen 2 protected all cell percent area (est. with hyde cell area and no antarctica) = ",
            100 * total_no_ant_allan_scen2_protected_all_cell_area / (sum(Allan_sce2_data$cell_area_km2) ) ))
            
# estimate the protected cell area not in the hyde land mask and not in antarctica
lost_allan_scen2_protected_all_cell_area = total_allan_scen2_protected_all_cell_area - total_no_ant_allan_scen2_protected_all_cell_area
cat(paste0("allan scen 2 lost protected all cell area in km^2 (no antarctica) = ", lost_allan_scen2_protected_all_cell_area))
cat(paste0("allan scen 2 lost protected all cell percent of non-antarctic protected all cell area  = ",
            100 * lost_allan_scen2_protected_all_cell_area / total_no_ant_allan_scen2_protected_all_cell_area ) )

total_allan_scen2_protected_hyde_land_area = sum(Allan_sce2_data$protected_hyde_land_area_km2)
total_allan_scen2_unprotected_hyde_land_area = total_hyde_land_area - total_allan_scen2_protected_hyde_land_area
cat(paste0("Total allan scen 2 unprotected land area in km^2 (no antarctica) = ", total_allan_scen2_unprotected_hyde_land_area))
cat(paste0("Total allan scen 2 unprotected percent land area (no antarctica) = ", 100 * total_allan_scen2_unprotected_hyde_land_area / total_hyde_land_area))
cat(paste0("Total allan scen 2 protected land area in km^2 (no antarctica) = ", total_allan_scen2_protected_hyde_land_area))
cat(paste0("Total allan scen 2 protected percent land area (no antarctica) = ", 100 * total_allan_scen2_protected_hyde_land_area / total_hyde_land_area))

########## fix scen 3 raster!
# y lat values are not correct

if(FALSE){

## Allan scenario 3 data - minimizing conflict with agricultural expansion (costs of protection area higher where ag rents are higher)
Allan_sce3_rast <- raster("rasters_Allan_et_al/Allan_cat_sce3_5arcmin.tif")

Allan_sce3_data <- as.data.frame(rasterToPoints(Allan_sce3_rast))
colnames(Allan_sce3_data)<- c("x","y","protected_cell")

# all cell area
Allan_sce3_area <- as.data.frame(rasterToPoints(area(Allan_sce3_rast)))
colnames(Allan_sce3_area)<- c("x","y","all_cell_area_km2")
Allan_sce3_data$protected_all_cell_area_km2 = Allan_sce3_data$protected_cell
Allan_sce3_data %>% 
  left_join(Allan_sce3_area) %>% 
  mutate(protected_all_cell_area_km2 = protected_all_cell_area_km2 * all_cell_area_km2)->Allan_sce3_data

# round x and y to 3 digits
Allan_sce3_data = mutate(Allan_sce3_data, x=round(x,3),y=round(y,3))

# hyde cell and land area
Allan_sce3_data$protected_hyde_cell_area_km2 = Allan_sce3_data$protected_cell
Allan_sce3_data$protected_hyde_land_area_km2 = Allan_sce3_data$protected_cell
Allan_sce3_data %>% 
  left_join(hyde) %>% 
  mutate(protected_hyde_cell_area_km2 = protected_hyde_cell_area_km2 * cell_area_km2)->Allan_sce3_data
Allan_sce3_data$protected_hyde_land_area_km2 = Allan_sce3_data$protected_hyde_land_area_km2 * Allan_sce3_data$land_area_km2

total_allan_scen3_protected_all_cell_area = sum(Allan_sce3_data$protected_all_cell_area_km2)
total_allan_scen3_protected_hyde_cell_area = sum(Allan_sce3_data$protected_hyde_cell_area_km2)
cat(paste0("For comparison:\nallan scen 3 protected all cell area in km^2 (no antarctica) = ", total_allan_scen3_protected_all_cell_area))
cat(paste0("allan scen 3 protected hyde cell area in km^2 (no antarctica) = ", total_allan_scen3_protected_hyde_cell_area))

total_allan_scen3_protected_hyde_land_area = sum(Allan_sce3_data$protected_hyde_land_area_km2)
total_allan_scen3_unprotected_hyde_land_area = total_hyde_land_area - total_allan_scen3_protected_hyde_land_area
cat(paste0("Total allan scen 3 unprotected land area in km^2 (no antarctica) = ", total_allan_scen3_unprotected_hyde_land_area))
cat(paste0("Total allan scen 3 unprotected percent land area (no antarctica) = ", 100 * total_allan_scen3_unprotected_hyde_land_area / total_hyde_land_area))
cat(paste0("Total allan scen 3 protected land area in km^2 (no antarctica) = ", total_allan_scen3_protected_hyde_land_area))
cat(paste0("Total allan scen 3 protected percent land area (no antarctica) = ", 100 * total_allan_scen3_protected_hyde_land_area / total_hyde_land_area))

} # end if(FALSE) for scen 3


####### getting gcam default unavailable land rasters

# moirai suitable unprotected land - GCAM default

# this is a moirai diagnostic output - it is the fraction of a cell
# convert it to corresponding land area
# no data cells are -9999, so convert them to zero
suit_unprot_rast <- raster("moirai_default_protection/SuitableUnprotected.bil")
suit_unprot_rast[Which(suit_unprot_rast <= 0,cells=TRUE)] = 0
GCAM_default <- as.data.frame(rasterToPoints(suit_unprot_rast))
colnames(GCAM_default)<- c("x","y","suit_unprot_area_frac")

GCAM_default %>% 
  mutate(x=round(x,3),
         y=round(y,3)) %>% 
  left_join(hyde) -> GCAM_default
GCAM_default$suit_unprot_area_km2 = GCAM_default$suit_unprot_area_frac * GCAM_default$land_area_km2
GCAM_default$suit_unprot_area_frac = GCAM_default$suit_unprot_area_frac * GCAM_default$land_mask     # just in case
  
#   %>%
#  mutate(suit_unprot_area_km2 = suit_unprot_area_km2 * land_area_km2)->GCAM_default

default_avail_area = sum(GCAM_default$suit_unprot_area_km2)
default_unavail_area = total_hyde_land_area - default_avail_area
cat(paste0("Total suitable unprotected land area (default available) in km^2 (no antarctica) (GCAM default) = ", default_avail_area))
cat(paste0("Total suitable unprotected percent land area (default available) (no antarctica) (GCAM default) = ", 100 * default_avail_area / total_hyde_land_area))
cat(paste0("Total unsuitable plus suitable protected land area (default unavailable) in km^2 (no antarctica) under GCAM default = ", default_unavail_area))
cat(paste0("Total unsuitable plus suitable protected percent land area (default unavailable) (no antarctica) under GCAM default = ",
            100 * default_unavail_area / total_hyde_land_area))


# moirai unsuitable unprotected land

# this is a moirai diagnostic output - it is the fraction of a cell
# convert it to corresponding land area
# no data cells are -9999, so convert them to zero
unsuit_unprot_rast <- raster("moirai_default_protection/UnsuitableUnprotected.bil")
unsuit_unprot_rast[Which(unsuit_unprot_rast <= 0,cells=TRUE)] = 0
GCAM_Unsuit_Unprot <- as.data.frame(rasterToPoints(unsuit_unprot_rast)) 
colnames(GCAM_Unsuit_Unprot)<- c("x","y","unsuit_unprot_area_frac")

GCAM_Unsuit_Unprot %>% 
  mutate(x=round(x,3),
         y=round(y,3)) %>% 
  left_join(GCAM_default) -> GCAM_Unsuit_Unprot
GCAM_Unsuit_Unprot$unsuit_unprot_area_km2 = GCAM_Unsuit_Unprot$unsuit_unprot_area_frac * GCAM_Unsuit_Unprot$land_area_km2
GCAM_Unsuit_Unprot$unsuit_unprot_area_frac = GCAM_Unsuit_Unprot$unsuit_unprot_area_frac * GCAM_Unsuit_Unprot$land_mask     # just in case
  
 #  %>% 
#  mutate(unsuit_unprot_area_km2 = unsuit_unprot_area_km2 * land_area_km2)-> GCAM_Unsuit_Unprot

total_unsuit_unprot_area = sum(GCAM_Unsuit_Unprot$unsuit_unprot_area_km2)
cat(paste0("Total unsuitable unprotected land area in km^2 (no antarctica) = ", total_unsuit_unprot_area))
cat(paste0("Total unsuitable unprotected percent land area (no antarctica) = ", 100 * total_unsuit_unprot_area / total_hyde_land_area)) 
  
  
# moirai unprotected and protected land
  
GCAM_Availability = GCAM_Unsuit_Unprot
GCAM_Availability$unprot_area_km2 = GCAM_Availability$suit_unprot_area_km2 + GCAM_Availability$unsuit_unprot_area_km2
GCAM_Availability$unprot_area_frac = GCAM_Availability$suit_unprot_area_frac + GCAM_Availability$unsuit_unprot_area_frac
GCAM_Availability$prot_area_km2 = GCAM_Availability$land_area_km2 - GCAM_Availability$unprot_area_km2
GCAM_Availability$prot_area_frac = (1 - GCAM_Availability$unprot_area_frac) * GCAM_Availability$land_mask

total_unprotected_area = sum(GCAM_Availability$unprot_area_km2)
cat(paste0("Total unprotected land area in km^2 (no antarctica) = ", total_unprotected_area))
cat(paste0("Total unprotected percent land area (no antarctica) = ", 100 * total_unprotected_area / total_hyde_land_area))
total_protected_area = sum(GCAM_Availability$prot_area_km2)
cat(paste0("Total protected land area in km^2 (no antarctica) = ", total_protected_area))
cat(paste0("Total protected percent land area (no antarctica) = ", 100 * total_protected_area / total_hyde_land_area))

# add moirai Gcam default unavailable area

GCAM_Availability$unavail_area_km2 = GCAM_Availability$land_area_km2 - GCAM_Availability$suit_unprot_area_km2
GCAM_Availability = GCAM_Availability[,c("x", "y", "unavail_area_km2", "prot_area_km2", "unprot_area_km2", "unsuit_unprot_area_km2", "suit_unprot_area_km2", "land_area_km2",
                                      "cell_area_km2", "land_mask", "suit_unprot_area_frac", "unsuit_unprot_area_frac", "unprot_area_frac", "prot_area_frac")]

total_unavailable_area = sum(GCAM_Availability$unavail_area_km2)
cat(paste0("Total default unavailable land area in km^2 (no antarctica) = ", total_unavailable_area))
cat(paste0("Total default unavailable percent land area (no antarctica) = ", 100 * total_unavailable_area / total_hyde_land_area))

# for comparison calculate the hyde cell area version of protected land
# in three ways?
# not super useful

# fraction of cell area
#protected_frac_hyde_cell_area = sum(GCAM_Availability$prot_area_frac * GCAM_Availability$cell_area_km2)
#cat(paste0("Total protected fraction of cell area in km^2 (no antarctica) = ", protected_frac_hyde_cell_area))
#cat(paste0("Total protected fraction of cell area as percent of cell area (no antarctica) = ", 100 * protected_frac_hyde_cell_area / total_hyde_cell_area_masked))

# full cell times land area - full cell not appropriate
#GCAM_Availability$prot_area_cell = 0
#GCAM_Availability$prot_area_cell[GCAM_Availability$prot_area_frac != 0] = 1
#protected_cell_hyde_land_area = sum(GCAM_Availability$prot_area_cell * GCAM_Availability$land_area_km2)
#cat(paste0("Total protected full cell land area in km^2 (no antarctica) = ", protected_cell_hyde_land_area))
#cat(paste0("Total protected full cell land area as percent of land area (no antarctica) = ", 100 * protected_cell_hyde_land_area / total_hyde_land_area))

# full cell times cell area - full cell not appropriate
#protected_cell_hyde_cell_area = sum(GCAM_Availability$prot_area_cell * GCAM_Availability$cell_area_km2)
#cat(paste0("Total protected full cell area in km^2 (no antarctica) = ", protected_cell_hyde_cell_area))
#cat(paste0("Total protected full cell area as percent of cell area (no antarctica) = ", 100 * protected_cell_hyde_cell_area / total_hyde_cell_area_masked))



######## Define new protection scenarios
# this creates specific protection files for substituting into gcamdata
# the above three allan secenarios, protection only
# the three allan scenarios overlayed with default availability
# 30% uniform only
# 30% uniform overlayed with default availability


######### First read in ancillary data for making output files

# these are currently 2015
# these two are moirai diagnostic outputs, like most of the the others
# may need all four land type area files for 2015

veg_thematic_rast <- raster("mapping/refveg_thematic_2015.bil")
veg_thematic <- as.data.frame(rasterToPoints(veg_thematic_rast))
colnames(veg_thematic)<- c("x","y","unmanaged_vegetation_codes")
veg_thematic <- mutate(veg_thematic, x=round(x,3), y=round(y,3))

veg_land_rast <- raster("mapping/refveg_area_2015.bil")
veg_land <- as.data.frame(rasterToPoints(veg_land_rast))
colnames(veg_land)<- c("x","y","veg_area_km2")
veg_land <- mutate(veg_land, x=round(x,3), y=round(y,3))

pasture_land_rast <- raster("mapping/pasture_area_2015.bil")
pasture_land <- as.data.frame(rasterToPoints(pasture_land_rast))
colnames(pasture_land)<- c("x","y","pasture_area_km2")
pasture_land <- mutate(pasture_land, x=round(x,3), y=round(y,3))

# don't need cropland or urban areas

#crop_land_rast <- raster("mapping/cropland_area_2015.bil")
#crop_land <- as.data.frame(rasterToPoints(crop_land_rast))
#colnames(crop_land)<- c("x","y","crop_area_km2")
#crop_land <- mutate(crop_land, x=round(x,3), y=round(y,3))

#urban_land_rast <- raster("mapping/urban_area_2015.bil")
#urban_land <- as.data.frame(rasterToPoints(urban_land_rast))
#colnames(urban_land)<- c("x","y","urban_area_km2")
#urban_land <- mutate(urban_land, x=round(x,3), y=round(y,3))

# Now basin boundaries, region boundaries, country boundaries
# these are moirai diagnostic outputs

###### basins has only 232 because 3 basins do not have any land area:
# antarctica, Micronesia, and North_Marina_Islands_and_Guam

basins_rast <- raster("mapping/glu_raster.bil")
basins <- as.data.frame(rasterToPoints(basins_rast))
colnames(basins)<- c("x","y","GLU")
basins <- mutate(basins, x=round(x,3), y=round(y,3))

regions_rast <- raster("mapping/region_gcam_out.bil")
regions <- as.data.frame(rasterToPoints(regions_rast))
colnames(regions)<- c("x","y","GCAM_region_ID")
regions <- mutate(regions, x=round(x,3), y=round(y,3))

countries_rast <- raster("mapping/country_out.bil")
countries <- as.data.frame(rasterToPoints(countries_rast))
colnames(countries)<- c("x","y","country_ID")
countries <- mutate(countries, x=round(x,3), y=round(y,3))

# this is based on moirai input data SAGE_PVLT.csv, but the names are those in gcamdata
# just make the table here rather than relying on a modified file
LT_SAGE_CODE = c(1:15)
LT_GCAMDATA_NAME = c("Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Grassland", "Grassland", "Shrubland", "Shrubland",
                      "Tundra", "RockIceDesert", "RockIceDesert")
sage_mapping = data.frame(unmanaged_vegetation_codes = LT_SAGE_CODE, Land_Type = LT_GCAMDATA_NAME)



## allan scen 1

# now overlay the default unavailable data as protected

# first add any missing existing protected area; assume that scenario includes all existing protected area unless it is short
Allan_sce1_data_plus = left_join(Allan_sce1_data, GCAM_Availability[,c("x", "y", "unavail_area_km2", "prot_area_km2", "unsuit_unprot_area_km2")])
Allan_sce1_data_plus$protected_added = Allan_sce1_data_plus$prot_area_km2 - Allan_sce1_data_plus$protected_hyde_land_area_km2
Allan_sce1_data_plus$protected_added[which(Allan_sce1_data_plus$protected_added <= 0)] = 0
Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2 = Allan_sce1_data_plus$protected_hyde_land_area_km2 + Allan_sce1_data_plus$protected_added
# cannot exceed land area - although this should not happen here
Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2[Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2 > Allan_sce1_data_plus$land_area_km2] =
	Allan_sce1_data_plus$land_area_km2[Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2 > Allan_sce1_data_plus$land_area_km2]
Allan_sce1_data_plus$protected_added = Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2 - Allan_sce1_data_plus$protected_hyde_land_area_km2
total_allan_scen1_protected_added = sum(Allan_sce1_data_plus$protected_added)
total_allan_scen1_protected_plus_current = sum(Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2)
cat(paste0("Total protected added to Allan scen 1 in km^2 (no antarctica) = ", total_allan_scen1_protected_added))
cat(paste0("Total protected added to Allan scen 1 as percent of current protected (no antarctica) = ", 100 * total_allan_scen1_protected_added / total_protected_area))
cat(paste0("Total protected added to Allan scen 1 as percent of allan scen 1 protected (no antarctica) = ",
           100 * total_allan_scen1_protected_added / total_allan_scen1_protected_hyde_land_area))
cat(paste0("Total protected added to Allan scen 1 as percent of land area (no antarctica) = ",
           100 * total_allan_scen1_protected_added / total_hyde_land_area))
cat(paste0("Total protected Allan scen 1 plus current in km^2 (no antarctica) = ", total_allan_scen1_protected_plus_current))
cat(paste0("Total protected Allan scen 1 plus current as percent of land area (no antarctica) = ", 100 * total_allan_scen1_protected_plus_current / total_hyde_land_area))

# now add the unsuitable unprotected, which is the remaining amount of default unavailable land
Allan_sce1_data_plus$protected_plus_unavail_hyde_land_area_km2 = Allan_sce1_data_plus$protected_plus_protadd_hyde_land_area_km2 + Allan_sce1_data_plus$unsuit_unprot_area_km2
# cannot exceed land area - this could happen here
Allan_sce1_data_plus$protected_plus_unavail_hyde_land_area_km2[Allan_sce1_data_plus$protected_plus_unavail_hyde_land_area_km2 > Allan_sce1_data_plus$land_area_km2] =
	Allan_sce1_data_plus$land_area_km2[Allan_sce1_data_plus$protected_plus_unavail_hyde_land_area_km2 > Allan_sce1_data_plus$land_area_km2]
total_allan_scen1_protected_plus_unavail_hyde_land_area_km2 = sum(Allan_sce1_data_plus$protected_plus_unavail_hyde_land_area_km2)
cat(paste0("Total protected scen 1 plus default unavailable area in km^2 (no antarctica) = ", total_allan_scen1_protected_plus_unavail_hyde_land_area_km2))
cat(paste0("Total protected scen 1 plus default unavailable area as percent of land area (no antarctica) = ",
           100 * total_allan_scen1_protected_plus_unavail_hyde_land_area_km2 / total_hyde_land_area))

# join the ancillary data

# the land protection fractions are based on the protected land area and total land area in each cell
# the land protection fractions are independent of land type, so apply them directly to each land type
# the only cells with two land types are those with pasture area because there is only one ref veg per cell
# urban and cropland do not have protection info

Allan_sce1_data_plus %>% 
  left_join(basins) %>% 
  left_join(regions) %>% 
  left_join(veg_thematic) %>% 
  left_join(sage_mapping) %>% 
  left_join(pasture_land) %>%
  na.omit()-> Allan_sce1_data_joined
  
# drop non-id cells
Allan_sce1_data_joined = Allan_sce1_data_joined[Allan_sce1_data_joined$GCAM_region_ID != -9999 & Allan_sce1_data_joined$GLU != -9999,]

# create the psture records
pasture_df = Allan_sce1_data_joined[Allan_sce1_data_joined$pasture_area_km2 > 0,]
pasture_df$Land_Type = "Pasture"

## first write the scenario protection only file

# for the gcamdata file
# need to add pasture records - pasture_df already created
Allan_sce1_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_GCAM
# maximum value is 1
Allan_sce1_GCAM$prot_frac[Allan_sce1_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_GCAM$prot_frac[Allan_sce1_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
Allan_sce1_GCAM$year = 2015
Allan_sce1_GCAM$non_prot_frac = 1 - Allan_sce1_GCAM$prot_frac
Allan_sce1_GCAM$non_prot_frac[Allan_sce1_GCAM$non_prot_frac <= 0] = 0
Allan_sce1_GCAM = Allan_sce1_GCAM[,gcamdata_cols]
# change the GLU names
Allan_sce1_GCAM <- mutate(Allan_sce1_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "Allan_sce1_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(Allan_sce1_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
Allan_sce1_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_GCAM_plot_regXglu
# maximum value is 1
Allan_sce1_GCAM_plot_regXglu$prot_frac[Allan_sce1_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_GCAM_plot_regXglu$prot_frac[Allan_sce1_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(Allan_sce1_GCAM_plot_regXglu, "Allan_sce1_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
Allan_sce1_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_GCAM_plot_reg
# maximum value is 1
Allan_sce1_GCAM_plot_reg$prot_frac[Allan_sce1_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_GCAM_plot_reg$prot_frac[Allan_sce1_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(Allan_sce1_GCAM_plot_reg, "Allan_sce1_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
Allan_sce1_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_GCAM_plot_glu
# maximum value is 1
Allan_sce1_GCAM_plot_glu$prot_frac[Allan_sce1_GCAM_plot_glu$prot_frac > 1] = 1
# remove non region/basin records
Allan_sce1_GCAM_plot_glu = Allan_sce1_GCAM_plot_glu[Allan_sce1_GCAM_plot_glu$GLU != -9999,]
# some calcs go to negative near zero, so set them to zero
Allan_sce1_GCAM_plot_glu$prot_frac[Allan_sce1_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(Allan_sce1_GCAM_plot_glu, "Allan_sce1_GCAM_plot_glu.csv", row.names = FALSE)


## now write the scenario overlayed with default protection

# for the gcamdata file
# need to add pasture records - pasture_df already created
Allan_sce1_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_defprot_GCAM
# maximum value is 1
Allan_sce1_plus_defprot_GCAM$prot_frac[Allan_sce1_plus_defprot_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_defprot_GCAM$prot_frac[Allan_sce1_plus_defprot_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
Allan_sce1_plus_defprot_GCAM$year = 2015
Allan_sce1_plus_defprot_GCAM$non_prot_frac = 1 - Allan_sce1_plus_defprot_GCAM$prot_frac
Allan_sce1_plus_defprot_GCAM$non_prot_frac[Allan_sce1_plus_defprot_GCAM$non_prot_frac <= 0] = 0
Allan_sce1_plus_defprot_GCAM = Allan_sce1_plus_defprot_GCAM[,gcamdata_cols]
# change the GLU names
Allan_sce1_plus_defprot_GCAM <- mutate(Allan_sce1_plus_defprot_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "Allan_sce1_plus_defprot_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(Allan_sce1_plus_defprot_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
Allan_sce1_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_defprot_GCAM_plot_regXglu
# maximum value is 1
Allan_sce1_plus_defprot_GCAM_plot_regXglu$prot_frac[Allan_sce1_plus_defprot_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_defprot_GCAM_plot_regXglu $prot_frac[Allan_sce1_plus_defprot_GCAM_plot_regXglu $prot_frac <= 0] = 0
write.csv(Allan_sce1_plus_defprot_GCAM_plot_regXglu, "Allan_sce1_plus_defprot_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
Allan_sce1_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_defprot_GCAM_plot_reg
# maximum value is 1
Allan_sce1_plus_defprot_GCAM_plot_reg$prot_frac[Allan_sce1_plus_defprot_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_defprot_GCAM_plot_reg$prot_frac[Allan_sce1_plus_defprot_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(Allan_sce1_plus_defprot_GCAM_plot_reg, "Allan_sce1_plus_defprot_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
Allan_sce1_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_defprot_GCAM_plot_glu
# maximum value is 1
Allan_sce1_plus_defprot_GCAM_plot_glu$prot_frac[Allan_sce1_plus_defprot_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_defprot_GCAM_plot_glu$prot_frac[Allan_sce1_plus_defprot_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(Allan_sce1_plus_defprot_GCAM_plot_glu, "Allan_sce1_plus_defprot_GCAM_plot_glu.csv", row.names = FALSE)


## now write the scenario overlayed with default unavailability

# for the gcamdata file
# need to add pasture records - pasture_df already created
Allan_sce1_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_default_GCAM
# maximum value is 1
Allan_sce1_plus_default_GCAM$prot_frac[Allan_sce1_plus_default_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_default_GCAM$prot_frac[Allan_sce1_plus_default_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
Allan_sce1_plus_default_GCAM$year = 2015
Allan_sce1_plus_default_GCAM$non_prot_frac = 1 - Allan_sce1_plus_default_GCAM$prot_frac
Allan_sce1_plus_default_GCAM$non_prot_frac[Allan_sce1_plus_default_GCAM$non_prot_frac <= 0] = 0
Allan_sce1_plus_default_GCAM = Allan_sce1_plus_default_GCAM[,gcamdata_cols]
# change the GLU names
Allan_sce1_plus_default_GCAM <- mutate(Allan_sce1_plus_default_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "Allan_sce1_plus_default_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(Allan_sce1_plus_default_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
Allan_sce1_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_default_GCAM_plot_regXglu
# maximum value is 1
Allan_sce1_plus_default_GCAM_plot_regXglu$prot_frac[Allan_sce1_plus_default_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_default_GCAM_plot_regXglu$prot_frac[Allan_sce1_plus_default_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(Allan_sce1_plus_default_GCAM_plot_regXglu, "Allan_sce1_plus_default_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
Allan_sce1_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_default_GCAM_plot_reg
# maximum value is 1
Allan_sce1_plus_default_GCAM_plot_reg$prot_frac[Allan_sce1_plus_default_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_default_GCAM_plot_reg$prot_frac[Allan_sce1_plus_default_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(Allan_sce1_plus_default_GCAM_plot_reg, "Allan_sce1_plus_default_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
Allan_sce1_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce1_plus_default_GCAM_plot_glu
# maximum value is 1
Allan_sce1_plus_default_GCAM_plot_glu$prot_frac[Allan_sce1_plus_default_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce1_plus_default_GCAM_plot_glu$prot_frac[Allan_sce1_plus_default_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(Allan_sce1_plus_default_GCAM_plot_glu, "Allan_sce1_plus_default_GCAM_plot_glu.csv", row.names = FALSE)


## allan scen 2

# now overlay the default unavailable data as protected

# first add any missing existing protected area; assume that scenario includes all existing protected area unless it is short
Allan_sce2_data_plus = left_join(Allan_sce2_data, GCAM_Availability[,c("x", "y", "unavail_area_km2", "prot_area_km2", "unsuit_unprot_area_km2")])
Allan_sce2_data_plus$protected_added = Allan_sce2_data_plus$prot_area_km2 - Allan_sce2_data_plus$protected_hyde_land_area_km2
Allan_sce2_data_plus$protected_added[which(Allan_sce2_data_plus$protected_added <= 0)] = 0
Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2 = Allan_sce2_data_plus$protected_hyde_land_area_km2 + Allan_sce2_data_plus$protected_added
# cannot exceed land area - although this should not happen here
Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2[Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2 > Allan_sce2_data_plus$land_area_km2] =
	Allan_sce2_data_plus$land_area_km2[Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2 > Allan_sce2_data_plus$land_area_km2]
Allan_sce2_data_plus$protected_added = Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2 - Allan_sce2_data_plus$protected_hyde_land_area_km2
total_allan_scen2_protected_added = sum(Allan_sce2_data_plus$protected_added)
total_allan_scen2_protected_plus_current = sum(Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2)
cat(paste0("Total protected added to Allan scen 2 in km^2 (no antarctica) = ", total_allan_scen2_protected_added))
cat(paste0("Total protected added to Allan scen 2 as percent of current protected (no antarctica) = ", 100 * total_allan_scen2_protected_added / total_protected_area))
cat(paste0("Total protected added to Allan scen 2 as percent of allan scen 2 protected (no antarctica) = ",
           100 * total_allan_scen2_protected_added / total_allan_scen2_protected_hyde_land_area))
cat(paste0("Total protected added to Allan scen 2 as percent of land area (no antarctica) = ",
           100 * total_allan_scen2_protected_added / total_hyde_land_area))
cat(paste0("Total protected Allan scen 2 plus current in km^2 (no antarctica) = ", total_allan_scen2_protected_plus_current))
cat(paste0("Total protected Allan scen 2 plus current as percent of land area (no antarctica) = ", 100 * total_allan_scen2_protected_plus_current / total_hyde_land_area))

# now add the unsuitable unprotected, which is the remaining amount of default unavailable land
Allan_sce2_data_plus$protected_plus_unavail_hyde_land_area_km2 = Allan_sce2_data_plus$protected_plus_protadd_hyde_land_area_km2 + Allan_sce2_data_plus$unsuit_unprot_area_km2
# cannot exceed land area - this could happen here
Allan_sce2_data_plus$protected_plus_unavail_hyde_land_area_km2[Allan_sce2_data_plus$protected_plus_unavail_hyde_land_area_km2 > Allan_sce2_data_plus$land_area_km2] =
	Allan_sce2_data_plus$land_area_km2[Allan_sce2_data_plus$protected_plus_unavail_hyde_land_area_km2 > Allan_sce2_data_plus$land_area_km2]
total_allan_scen2_protected_plus_unavail_hyde_land_area_km2 = sum(Allan_sce2_data_plus$protected_plus_unavail_hyde_land_area_km2)
cat(paste0("Total protected scen 2 plus default unavailable area in km^2 (no antarctica) = ", total_allan_scen2_protected_plus_unavail_hyde_land_area_km2))
cat(paste0("Total protected scen 2 plus default unavailable area as percent of land area (no antarctica) = ",
           100 * total_allan_scen2_protected_plus_unavail_hyde_land_area_km2 / total_hyde_land_area))

# join the ancillary data

# the land protection fractions are based on the protected land area and total land area in each cell
# the land protection fractions are independent of land type, so apply them directly to each land type
# the only cells with two land types are those with pasture area because there is only one ref veg per cell
# urban and cropland do not have protection info

Allan_sce2_data_plus %>% 
  left_join(basins) %>% 
  left_join(regions) %>% 
  left_join(veg_thematic) %>% 
  left_join(sage_mapping) %>% 
  left_join(pasture_land) %>%
  na.omit()-> Allan_sce2_data_joined
  
# drop non-id cells
Allan_sce2_data_joined = Allan_sce2_data_joined[Allan_sce2_data_joined$GCAM_region_ID != -9999 & Allan_sce2_data_joined$GLU != -9999,]

# create the psture records
pasture_df = Allan_sce2_data_joined[Allan_sce2_data_joined$pasture_area_km2 > 0,]
pasture_df$Land_Type = "Pasture"

## first write the scenario protection only file

# for the gcamdata file
# need to add pasture records - pasture_df already created
Allan_sce2_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_GCAM
# maximum value is 1
Allan_sce2_GCAM$prot_frac[Allan_sce2_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_GCAM$prot_frac[Allan_sce2_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
Allan_sce2_GCAM$year = 2015
Allan_sce2_GCAM$non_prot_frac = 1 - Allan_sce2_GCAM$prot_frac
Allan_sce2_GCAM$non_prot_frac[Allan_sce2_GCAM$non_prot_frac <= 0] = 0
Allan_sce2_GCAM = Allan_sce2_GCAM[,gcamdata_cols]
# change the GLU names
Allan_sce2_GCAM <- mutate(Allan_sce2_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "Allan_sce2_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(Allan_sce2_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
Allan_sce2_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_GCAM_plot_regXglu
# maximum value is 1
Allan_sce2_GCAM_plot_regXglu$prot_frac[Allan_sce2_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_GCAM_plot_regXglu$prot_frac[Allan_sce2_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(Allan_sce2_GCAM_plot_regXglu, "Allan_sce2_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
Allan_sce2_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_GCAM_plot_reg
# maximum value is 1
Allan_sce2_GCAM_plot_reg$prot_frac[Allan_sce2_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_GCAM_plot_reg$prot_frac[Allan_sce2_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(Allan_sce2_GCAM_plot_reg, "Allan_sce2_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
Allan_sce2_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_GCAM_plot_glu
# maximum value is 1
Allan_sce2_GCAM_plot_glu$prot_frac[Allan_sce2_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_GCAM_plot_glu$prot_frac[Allan_sce2_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(Allan_sce2_GCAM_plot_glu, "Allan_sce2_GCAM_plot_glu.csv", row.names = FALSE)


## now write the scenario overlayed with default protection

# for the gcamdata file
# need to add pasture records - pasture_df already created
Allan_sce2_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_defprot_GCAM
# maximum value is 1
Allan_sce2_plus_defprot_GCAM$prot_frac[Allan_sce2_plus_defprot_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_defprot_GCAM$prot_frac[Allan_sce2_plus_defprot_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
Allan_sce2_plus_defprot_GCAM$year = 2015
Allan_sce2_plus_defprot_GCAM$non_prot_frac = 1 - Allan_sce2_plus_defprot_GCAM$prot_frac
Allan_sce2_plus_defprot_GCAM$non_prot_frac[Allan_sce2_plus_defprot_GCAM$non_prot_frac <= 0] = 0
Allan_sce2_plus_defprot_GCAM = Allan_sce2_plus_defprot_GCAM[,gcamdata_cols]
# change the GLU names
Allan_sce2_plus_defprot_GCAM <- mutate(Allan_sce2_plus_defprot_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "Allan_sce2_plus_defprot_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(Allan_sce2_plus_defprot_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
Allan_sce2_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_defprot_GCAM_plot_regXglu
# maximum value is 1
Allan_sce2_plus_defprot_GCAM_plot_regXglu$prot_frac[Allan_sce2_plus_defprot_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_defprot_GCAM_plot_regXglu$prot_frac[Allan_sce2_plus_defprot_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(Allan_sce2_plus_defprot_GCAM_plot_regXglu, "Allan_sce2_plus_defprot_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
Allan_sce2_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_defprot_GCAM_plot_reg
# maximum value is 1
Allan_sce2_plus_defprot_GCAM_plot_reg$prot_frac[Allan_sce2_plus_defprot_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_defprot_GCAM_plot_reg$prot_frac[Allan_sce2_plus_defprot_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(Allan_sce2_plus_defprot_GCAM_plot_reg, "Allan_sce2_plus_defprot_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
Allan_sce2_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_defprot_GCAM_plot_glu
# maximum value is 1
Allan_sce2_plus_defprot_GCAM_plot_glu$prot_frac[Allan_sce2_plus_defprot_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_defprot_GCAM_plot_glu$prot_frac[Allan_sce2_plus_defprot_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(Allan_sce2_plus_defprot_GCAM_plot_glu, "Allan_sce2_plus_defprot_GCAM_plot_glu.csv", row.names = FALSE)


## now write the scenario overlayed with default unavailability

# for the gcamdata file
# need to add pasture records - pasture_df already created
Allan_sce2_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_default_GCAM
# maximum value is 1
Allan_sce2_plus_default_GCAM$prot_frac[Allan_sce2_plus_default_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_default_GCAM$prot_frac[Allan_sce2_plus_default_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
Allan_sce2_plus_default_GCAM$year = 2015
Allan_sce2_plus_default_GCAM$non_prot_frac = 1 - Allan_sce2_plus_default_GCAM$prot_frac
Allan_sce2_plus_default_GCAM$non_prot_frac[Allan_sce2_plus_default_GCAM$non_prot_frac <= 0] = 0
Allan_sce2_plus_default_GCAM = Allan_sce2_plus_default_GCAM[,gcamdata_cols]
# change the GLU names
Allan_sce2_plus_default_GCAM <- mutate(Allan_sce2_plus_default_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "Allan_sce2_plus_default_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(Allan_sce2_plus_default_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
Allan_sce2_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_default_GCAM_plot_regXglu
# maximum value is 1
Allan_sce2_plus_default_GCAM_plot_regXglu$prot_frac[Allan_sce2_plus_default_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_default_GCAM_plot_regXglu$prot_frac[Allan_sce2_plus_default_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(Allan_sce2_plus_default_GCAM_plot_regXglu, "Allan_sce2_plus_default_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
Allan_sce2_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_default_GCAM_plot_reg
# maximum value is 1
Allan_sce2_plus_default_GCAM_plot_reg$prot_frac[Allan_sce2_plus_default_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_default_GCAM_plot_reg$prot_frac[Allan_sce2_plus_default_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(Allan_sce2_plus_default_GCAM_plot_reg, "Allan_sce2_plus_default_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
Allan_sce2_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> Allan_sce2_plus_default_GCAM_plot_glu
# maximum value is 1
Allan_sce2_plus_default_GCAM_plot_glu$prot_frac[Allan_sce2_plus_default_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
Allan_sce2_plus_default_GCAM_plot_glu$prot_frac[Allan_sce2_plus_default_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(Allan_sce2_plus_default_GCAM_plot_glu, "Allan_sce2_plus_default_GCAM_plot_glu.csv", row.names = FALSE)



## allan scen 3 - not working yet - don't bother



#### 30% of each cell, including current

# determine the scenario as what is added to current protected in each cell to get 30% at teh country level
# first go cell by cell to get the base scenario, if a cell already has >=30%, then don't add anything
thirty_percent_data = GCAM_Availability[,c("x", "y", "prot_area_km2", "land_area_km2")]
thirty_percent_data$protected_hyde_land_area_km2 = (0.3 * thirty_percent_data$land_area_km2) - thirty_percent_data$prot_area_km2
thirty_percent_data$protected_hyde_land_area_km2[thirty_percent_data$protected_hyde_land_area_km2 <= 0] = 0
# now group by country and determine how much to reduce the base scenario to result in 30% total
thirty_percent_data <- left_join(thirty_percent_data, countries)
#thirty_percent_data <- na.omit(thirty_percent_data)
#thirty_percent_data = thirty_percent_data[thirty_percent_data$country_ID != -9999,]
thirty_percent_data %>% 
   group_by(country_ID) %>%
   summarize(country_protected_area = sum(protected_hyde_land_area_km2), country_defprot_area = sum(prot_area_km2), country_land_area = sum(land_area_km2)) %>%
   mutate(country_allprot_area = country_protected_area + country_defprot_area, country_30pct_area = 0.3 * country_land_area) %>%
   mutate(excess_prot_area = country_allprot_area - country_30pct_area) %>%
   mutate(prot_scalar = ifelse( excess_prot_area <= 0, 1, (country_protected_area - excess_prot_area) / country_protected_area )) %>%
   mutate(prot_scalar = ifelse( prot_scalar <= 0, 0, prot_scalar)) %>%
   mutate(prot_scalar = ifelse( prot_scalar == Inf | prot_scalar == -Inf | is.na(prot_scalar), 0, prot_scalar)) ->
   thirty_percent_data_country
thirty_percent_data = left_join(thirty_percent_data, thirty_percent_data_country)
#thirty_percent_data <- na.omit(thirty_percent_data)
thirty_percent_data$protected_hyde_land_area_km2 = thirty_percent_data$protected_hyde_land_area_km2 * thirty_percent_data$prot_scalar
thirty_percent_data = thirty_percent_data[,c("x", "y", "protected_hyde_land_area_km2", "land_area_km2")]

total_thirty_protected_hyde_land_area = sum(thirty_percent_data$protected_hyde_land_area_km2)
cat(paste0("Total protected thirty in km^2 (no antarctica) = ", total_thirty_protected_hyde_land_area))
cat(paste0("Total protected thirty as percent of land area (no antarctica) = ", 100 * total_thirty_protected_hyde_land_area / total_hyde_land_area))

# now overlay the default unavailable data as protected

# first add any missing existing protected area; assume that scenario includes all existing protected area unless it is short
thirty_percent_data_plus = left_join(thirty_percent_data, GCAM_Availability[,c("x", "y", "unavail_area_km2", "prot_area_km2", "unsuit_unprot_area_km2")])
thirty_percent_data_plus$protected_added = thirty_percent_data_plus$prot_area_km2 - thirty_percent_data_plus$protected_hyde_land_area_km2
thirty_percent_data_plus$protected_added[which(thirty_percent_data_plus$protected_added <= 0)] = 0
thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2 = thirty_percent_data_plus$protected_hyde_land_area_km2 + thirty_percent_data_plus$protected_added
# cannot exceed land area - although this should not happen here
thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2[thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2 > thirty_percent_data_plus$land_area_km2] =
	thirty_percent_data_plus$land_area_km2[thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2 > thirty_percent_data_plus$land_area_km2]
thirty_percent_data_plus$protected_added = thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2 - thirty_percent_data_plus$protected_hyde_land_area_km2
total_thirty_protected_added = sum(thirty_percent_data_plus$protected_added)
total_thirty_protected_plus_current = sum(thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2)
cat(paste0("Total protected added to thirty in km^2 (no antarctica) = ", total_thirty_protected_added))
cat(paste0("Total protected added to thirty as percent of current protected (no antarctica) = ", 100 * total_thirty_protected_added / total_protected_area))
cat(paste0("Total protected added to thirty as percent of thirty protected (no antarctica) = ",
           100 * total_thirty_protected_added / total_thirty_protected_hyde_land_area))
cat(paste0("Total protected added to thirty as percent of land area (no antarctica) = ",
           100 * total_thirty_protected_added / total_hyde_land_area))
cat(paste0("Total protected thirty plus current in km^2 (no antarctica) = ", total_thirty_protected_plus_current))
cat(paste0("Total protected thirty plus current as percent of land area (no antarctica) = ", 100 * total_thirty_protected_plus_current / total_hyde_land_area))

# now add the unsuitable unprotected, which is the remaining amount of default unavailable land
thirty_percent_data_plus$protected_plus_unavail_hyde_land_area_km2 = thirty_percent_data_plus$protected_plus_protadd_hyde_land_area_km2 +
                   thirty_percent_data_plus$unsuit_unprot_area_km2
# cannot exceed land area - this could happen here
thirty_percent_data_plus$protected_plus_unavail_hyde_land_area_km2[thirty_percent_data_plus$protected_plus_unavail_hyde_land_area_km2 > thirty_percent_data_plus$land_area_km2] =
	thirty_percent_data_plus$land_area_km2[thirty_percent_data_plus$protected_plus_unavail_hyde_land_area_km2 > thirty_percent_data_plus$land_area_km2]
total_thirty_protected_plus_unavail_hyde_land_area_km2 = sum(thirty_percent_data_plus$protected_plus_unavail_hyde_land_area_km2)
cat(paste0("Total protected thirty plus default unavailable area in km^2 (no antarctica) = ", total_thirty_protected_plus_unavail_hyde_land_area_km2))
cat(paste0("Total protected thirty plus default unavailable area as percent of land area (no antarctica) = ",
           100 * total_thirty_protected_plus_unavail_hyde_land_area_km2 / total_hyde_land_area))

# join the ancillary data

# the land protection fractions are based on the protected land area and total land area in each cell
# the land protection fractions are independent of land type, so apply them directly to each land type
# the only cells with two land types are those with pasture area because there is only one ref veg per cell
# urban and cropland do not have protection info

thirty_percent_data_plus %>% 
  left_join(basins) %>% 
  left_join(regions) %>% 
  left_join(veg_thematic) %>% 
  left_join(sage_mapping) %>% 
  left_join(pasture_land) %>%
  na.omit()-> thirty_percent_data_joined
  
# drop non-id cells
thirty_percent_data_joined = thirty_percent_data_joined[thirty_percent_data_joined$GCAM_region_ID != -9999 & thirty_percent_data_joined$GLU != -9999,]

# create the psture records
pasture_df = thirty_percent_data_joined[thirty_percent_data_joined$pasture_area_km2 > 0,]
pasture_df$Land_Type = "Pasture"

## first write the scenario protection only file

# for the gcamdata file
# need to add pasture records - pasture_df already created
thirty_percent_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_GCAM
# maximum value is 1
thirty_percent_GCAM$prot_frac[thirty_percent_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_GCAM$prot_frac[thirty_percent_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
thirty_percent_GCAM$year = 2015
thirty_percent_GCAM$non_prot_frac = 1 - thirty_percent_GCAM$prot_frac
thirty_percent_GCAM$non_prot_frac[thirty_percent_GCAM$non_prot_frac <= 0] = 0
thirty_percent_GCAM = thirty_percent_GCAM[,gcamdata_cols]
# change the GLU names
thirty_percent_GCAM <- mutate(thirty_percent_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "thirty_percent_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(thirty_percent_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
thirty_percent_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_GCAM_plot_regXglu
# maximum value is 1
thirty_percent_GCAM_plot_regXglu$prot_frac[thirty_percent_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_GCAM_plot_regXglu$prot_frac[thirty_percent_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(thirty_percent_GCAM_plot_regXglu, "thirty_percent_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
thirty_percent_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_GCAM_plot_reg
# maximum value is 1
thirty_percent_GCAM_plot_reg$prot_frac[thirty_percent_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_GCAM_plot_reg$prot_frac[thirty_percent_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(thirty_percent_GCAM_plot_reg, "thirty_percent_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
thirty_percent_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_GCAM_plot_glu
# maximum value is 1
thirty_percent_GCAM_plot_glu$prot_frac[thirty_percent_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_GCAM_plot_glu$prot_frac[thirty_percent_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(thirty_percent_GCAM_plot_glu, "thirty_percent_GCAM_plot_glu.csv", row.names = FALSE)


## now write the scenario overlayed with default protection

# for the gcamdata file
# need to add pasture records - pasture_df already created
thirty_percent_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_defprot_GCAM
# maximum value is 1
thirty_percent_plus_defprot_GCAM$prot_frac[thirty_percent_plus_defprot_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_defprot_GCAM$prot_frac[thirty_percent_plus_defprot_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
thirty_percent_plus_defprot_GCAM$year = 2015
thirty_percent_plus_defprot_GCAM$non_prot_frac = 1 - thirty_percent_plus_defprot_GCAM$prot_frac
thirty_percent_plus_defprot_GCAM$non_prot_frac[thirty_percent_plus_defprot_GCAM$non_prot_frac <= 0] = 0
thirty_percent_plus_defprot_GCAM = thirty_percent_plus_defprot_GCAM[,gcamdata_cols]
# change the GLU names
thirty_percent_plus_defprot_GCAM <- mutate(thirty_percent_plus_defprot_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "thirty_percent_plus_defprot_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(thirty_percent_plus_defprot_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
thirty_percent_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_defprot_GCAM_plot_regXglu
# maximum value is 1
thirty_percent_plus_defprot_GCAM_plot_regXglu$prot_frac[thirty_percent_plus_defprot_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_defprot_GCAM_plot_regXglu$prot_frac[thirty_percent_plus_defprot_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(thirty_percent_plus_defprot_GCAM_plot_regXglu, "thirty_percent_plus_defprot_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
thirty_percent_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_defprot_GCAM_plot_reg
# maximum value is 1
thirty_percent_plus_defprot_GCAM_plot_reg$prot_frac[thirty_percent_plus_defprot_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_defprot_GCAM_plot_reg$prot_frac[thirty_percent_plus_defprot_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(thirty_percent_plus_defprot_GCAM_plot_reg, "thirty_percent_plus_defprot_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
thirty_percent_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_plus_protadd_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_defprot_GCAM_plot_glu
# maximum value is 1
thirty_percent_plus_defprot_GCAM_plot_glu$prot_frac[thirty_percent_plus_defprot_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_defprot_GCAM_plot_glu$prot_frac[thirty_percent_plus_defprot_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(thirty_percent_plus_defprot_GCAM_plot_glu, "thirty_percent_plus_defprot_GCAM_plot_glu.csv", row.names = FALSE)


## now write the scenario overlayed with default unavailability

# for the gcamdata file
# need to add pasture records - pasture_df already created
thirty_percent_data_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_default_GCAM
# maximum value is 1
thirty_percent_plus_default_GCAM$prot_frac[thirty_percent_plus_default_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_default_GCAM$prot_frac[thirty_percent_plus_default_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
thirty_percent_plus_default_GCAM$year = 2015
thirty_percent_plus_default_GCAM$non_prot_frac = 1 - thirty_percent_plus_default_GCAM$prot_frac
thirty_percent_plus_default_GCAM$non_prot_frac[thirty_percent_plus_default_GCAM$non_prot_frac <= 0] = 0
thirty_percent_plus_default_GCAM = thirty_percent_plus_default_GCAM[,gcamdata_cols]
# change the GLU names
thirty_percent_plus_default_GCAM <- mutate(thirty_percent_plus_default_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "thirty_percent_plus_default_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(thirty_percent_plus_default_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
thirty_percent_data_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_default_GCAM_plot_regXglu
# maximum value is 1
thirty_percent_plus_default_GCAM_plot_regXglu$prot_frac[thirty_percent_plus_default_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_default_GCAM_plot_regXglu$prot_frac[thirty_percent_plus_default_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(thirty_percent_plus_default_GCAM_plot_regXglu, "thirty_percent_plus_default_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
thirty_percent_data_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_default_GCAM_plot_reg
# maximum value is 1
thirty_percent_plus_default_GCAM_plot_reg$prot_frac[thirty_percent_plus_default_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_default_GCAM_plot_reg$prot_frac[thirty_percent_plus_default_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(thirty_percent_plus_default_GCAM_plot_reg, "thirty_percent_plus_default_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
thirty_percent_data_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(protected_plus_unavail_hyde_land_area_km2)/sum(land_area_km2)) -> thirty_percent_plus_default_GCAM_plot_glu
# maximum value is 1
thirty_percent_plus_default_GCAM_plot_glu$prot_frac[thirty_percent_plus_default_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
thirty_percent_plus_default_GCAM_plot_glu$prot_frac[thirty_percent_plus_default_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(thirty_percent_plus_default_GCAM_plot_glu, "thirty_percent_plus_default_GCAM_plot_glu.csv", row.names = FALSE)



## default values

# existing protected only

cat(paste0("Total default protected land area in km^2 (no antarctica) = ", total_protected_area))
cat(paste0("Total default protected as percent of land area (no antarctica) = ", 100 * total_protected_area / total_hyde_land_area))


# the land protection fractions are based on the protected land area and total land area in each cell
# the land protection fractions are independent of land type, so apply them directly to each land type
# the only cells with two land types are those with pasture area because there is only one ref veg per cell
# urban and cropland do not have protection info

GCAM_Availability %>% 
  left_join(basins) %>% 
  left_join(regions) %>% 
  left_join(veg_thematic) %>% 
  left_join(sage_mapping) %>% 
  left_join(pasture_land) %>%
  na.omit()-> GCAM_Availability_joined
  
# drop non-id cells
GCAM_Availability_joined = GCAM_Availability_joined[GCAM_Availability_joined$GCAM_region_ID != -9999 & GCAM_Availability_joined$GLU != -9999,]

# create the psture records
pasture_df = GCAM_Availability_joined[GCAM_Availability_joined$pasture_area_km2 > 0,]
pasture_df$Land_Type = "Pasture"

## first write the scenario protection only file

# for the gcamdata file
# need to add pasture records - pasture_df already created
GCAM_Availability_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(prot_area_km2)/sum(land_area_km2)) -> GCAM_Protected_GCAM
# maximum value is 1
GCAM_Protected_GCAM$prot_frac[GCAM_Protected_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Protected_GCAM$prot_frac[GCAM_Protected_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
GCAM_Protected_GCAM$year = 2015
GCAM_Protected_GCAM$non_prot_frac = 1 - GCAM_Protected_GCAM$prot_frac
GCAM_Protected_GCAM$non_prot_frac[GCAM_Protected_GCAM$non_prot_frac <= 0] = 0
GCAM_Protected_GCAM = GCAM_Protected_GCAM[,gcamdata_cols]
# change the GLU names
GCAM_Protected_GCAM <- mutate(GCAM_Protected_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "GCAM_Protected_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(GCAM_Protected_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
GCAM_Availability_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(prot_area_km2)/sum(land_area_km2)) -> GCAM_Protected_GCAM_plot_regXglu
# maximum value is 1
GCAM_Protected_GCAM_plot_regXglu$prot_frac[GCAM_Protected_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Protected_GCAM_plot_regXglu$prot_frac[GCAM_Protected_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(GCAM_Protected_GCAM_plot_regXglu, "GCAM_Protected_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
GCAM_Availability_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(prot_area_km2)/sum(land_area_km2)) -> GCAM_Protected_GCAM_plot_reg
# maximum value is 1
GCAM_Protected_GCAM_plot_reg$prot_frac[GCAM_Protected_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Protected_GCAM_plot_reg$prot_frac[GCAM_Protected_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(GCAM_Protected_GCAM_plot_reg, "GCAM_Protected_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
GCAM_Availability_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(prot_area_km2)/sum(land_area_km2)) -> GCAM_Protected_GCAM_plot_glu
# maximum value is 1
GCAM_Protected_GCAM_plot_glu$prot_frac[GCAM_Protected_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Protected_GCAM_plot_glu$prot_frac[GCAM_Protected_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(GCAM_Protected_GCAM_plot_glu, "GCAM_Protected_GCAM_plot_glu.csv", row.names = FALSE)

## now write the default unavailability

cat(paste0("Total default unavailable land area in km^2 (no antarctica) = ", total_unavailable_area))
cat(paste0("Total default unavailable as percent of land area (no antarctica) = ", 100 * total_unavailable_area / total_hyde_land_area))

# for the gcamdata file
# need to add pasture records - pasture_df already created
GCAM_Availability_joined %>%
  rbind(pasture_df) %>%
  group_by(GCAM_region_ID, GLU,Land_Type) %>% 
  summarize(prot_frac = sum(unavail_area_km2)/sum(land_area_km2)) -> GCAM_Unavailable_GCAM
# maximum value is 1
GCAM_Unavailable_GCAM$prot_frac[GCAM_Unavailable_GCAM$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Unavailable_GCAM$prot_frac[GCAM_Unavailable_GCAM$prot_frac <= 0] = 0
# add the year, not protected fraction, and reorder columns
GCAM_Unavailable_GCAM$year = 2015
GCAM_Unavailable_GCAM$non_prot_frac = 1 - GCAM_Unavailable_GCAM$prot_frac
GCAM_Unavailable_GCAM$non_prot_frac[GCAM_Unavailable_GCAM$non_prot_frac <= 0] = 0
GCAM_Unavailable_GCAM = GCAM_Unavailable_GCAM[,gcamdata_cols]
# change the GLU names
GCAM_Unavailable_GCAM <- mutate(GCAM_Unavailable_GCAM, GLU = ifelse( GLU < 10, paste0("GLU00",GLU), ifelse(GLU < 100, paste0("GLU0",GLU), paste0("GLU",GLU) ) ) )
# scale and add the other years?


ofname = "GCAM_Unavailable_GCAM.csv"
writeLines("# Manually created: Land types from SAGE, HYDE, and desired protection/suitability merged and reconciled; missing zeroes backfilled; 2015 scaled to AGLU land cover years", ofname)
write.table(GCAM_Unavailable_GCAM, ofname, sep=",", row.names = FALSE, append = TRUE, quote=FALSE)


# for plotting regionXglu
GCAM_Availability_joined %>% 
  group_by(GCAM_region_ID, GLU) %>% 
  summarize(prot_frac = sum(unavail_area_km2)/sum(land_area_km2)) -> GCAM_Unavailable_GCAM_plot_regXglu
# maximum value is 1
GCAM_Unavailable_GCAM_plot_regXglu$prot_frac[GCAM_Unavailable_GCAM_plot_regXglu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Unavailable_GCAM_plot_regXglu$prot_frac[GCAM_Unavailable_GCAM_plot_regXglu$prot_frac <= 0] = 0
write.csv(GCAM_Unavailable_GCAM_plot_regXglu, "GCAM_Unavailable_GCAM_plot_regXglu.csv", row.names = FALSE)

# for plotting region
GCAM_Availability_joined %>% 
  group_by(GCAM_region_ID) %>% 
  summarize(prot_frac = sum(unavail_area_km2)/sum(land_area_km2)) -> GCAM_Unavailable_GCAM_plot_reg
# maximum value is 1
GCAM_Unavailable_GCAM_plot_reg$prot_frac[GCAM_Unavailable_GCAM_plot_reg$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Unavailable_GCAM_plot_reg$prot_frac[GCAM_Unavailable_GCAM_plot_reg$prot_frac <= 0] = 0
write.csv(GCAM_Unavailable_GCAM_plot_reg, "GCAM_Unavailable_GCAM_plot_reg.csv", row.names = FALSE)

# for plotting glu
GCAM_Availability_joined %>% 
  group_by(GLU) %>% 
  summarize(prot_frac = sum(unavail_area_km2)/sum(land_area_km2)) -> GCAM_Unavailable_GCAM_plot_glu
# maximum value is 1
GCAM_Unavailable_GCAM_plot_glu$prot_frac[GCAM_Unavailable_GCAM_plot_glu$prot_frac > 1] = 1
# some calcs go to negative near zero, so set them to zero
GCAM_Unavailable_GCAM_plot_glu$prot_frac[GCAM_Unavailable_GCAM_plot_glu$prot_frac <= 0] = 0
write.csv(GCAM_Unavailable_GCAM_plot_glu, "GCAM_Unavailable_GCAM_plot_glu.csv", row.names = FALSE)



#################### write a table of the global protected/unavailable land areas/percents for each case

Scenario = c("Minimum protected land", "Minimum protected land plus default protection", "Minimum protected land plus default unavailable",
             "Protection away from land use", "Protection away from land use plus default protection", "Protection away from land use plus default unavailable",
             "Thirty percent uniform protection", "Thirty percent uniform protection plus default protection", "Thirty percent uniform protection plus default unavailable",
             "Current protection", "Current protection plus unsuitable unprotected (default unavailble)")
 
Protected_or_Unavailable_land_km2 = c(total_allan_scen1_protected_hyde_land_area, total_allan_scen1_protected_plus_current,
                                                               total_allan_scen1_protected_plus_unavail_hyde_land_area_km2,
                                   total_allan_scen2_protected_hyde_land_area, total_allan_scen2_protected_plus_current,
                                                            total_allan_scen2_protected_plus_unavail_hyde_land_area_km2,
                                   total_thirty_protected_hyde_land_area, total_thirty_protected_plus_current, total_thirty_protected_plus_unavail_hyde_land_area_km2,
                                   total_protected_area, total_unavailable_area)
                                   
Protected_or_Unavailable_percent_land = c(round(100 * total_allan_scen1_protected_hyde_land_area / total_hyde_land_area, 2),
                                       round(100 * total_allan_scen1_protected_plus_current / total_hyde_land_area, 2),
                                       round(100 * total_allan_scen1_protected_plus_unavail_hyde_land_area_km2 / total_hyde_land_area, 2),
                                       round(100 * total_allan_scen2_protected_hyde_land_area / total_hyde_land_area, 2),
                                       round(100 * total_allan_scen2_protected_plus_current / total_hyde_land_area, 2),
                                       round(100 * total_allan_scen2_protected_plus_unavail_hyde_land_area_km2 / total_hyde_land_area, 2),
                                       round(100 * total_thirty_protected_hyde_land_area / total_hyde_land_area, 2),
                                       round(100 * total_thirty_protected_plus_current / total_hyde_land_area, 2),
                                       round(100 * total_thirty_protected_plus_unavail_hyde_land_area_km2 / total_hyde_land_area, 2),
                                       round(100 * total_protected_area / total_hyde_land_area, 2),
                                       round(100 * total_unavailable_area / total_hyde_land_area, 2))
                                       
Protected_percent_land = c(round(100 * total_allan_scen1_protected_hyde_land_area / total_hyde_land_area, 2),
                           round(100 * total_allan_scen1_protected_plus_current / total_hyde_land_area, 2),
                           round(100 * total_allan_scen1_protected_plus_current / total_hyde_land_area, 2),
                           round(100 * total_allan_scen2_protected_hyde_land_area / total_hyde_land_area, 2),
                           round(100 * total_allan_scen2_protected_plus_current / total_hyde_land_area, 2),
                           round(100 * total_allan_scen2_protected_plus_current / total_hyde_land_area, 2),
                           round(100 * total_thirty_protected_hyde_land_area / total_hyde_land_area, 2),
                           round(100 * total_thirty_protected_plus_current / total_hyde_land_area, 2),
                           round(100 * total_thirty_protected_plus_current / total_hyde_land_area, 2),
                           round(100 * total_protected_area / total_hyde_land_area, 2),
                           round(100 * total_protected_area / total_hyde_land_area, 2))
                           
Unsuitable_and_Unprotected_percent_land = Protected_or_Unavailable_percent_land - Protected_percent_land
                                       
global_vals_df = data.frame(Scenario, Protected_or_Unavailable_land_km2, Protected_or_Unavailable_percent_land, Protected_percent_land, Unsuitable_and_Unprotected_percent_land)
write.csv(global_vals_df, "Protection_scenarios_global_values.csv", row.names=FALSE)     




############################################### plotting maps

#Scheme for figures
scheme_basic <- theme_bw() +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18, face = "bold")) +
  theme(plot.title = element_text(size = 15, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 9, face = "bold", vjust = 1))+ 
  theme(strip.text = element_text(size = 7))+
  theme(strip.text.x = element_text(size = 18, face = "bold"))+
  theme(strip.text.y = element_text(size = 15, face = "bold"))+
  theme(legend.position = "right")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12,color = "black",face="bold"))+
  theme(axis.text.x= element_text(hjust=1,angle=90))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

## GCAMDATA file default scenario, mean values
# from file

# glu

gcamdata_default %>% 
	filter(year==2015) %>% 
	group_by(GLU) %>% 
	summarize(prot_frac=mean(prot_frac)) -> gcamdata_default_2015_glu_mean

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
shapefile %>% 
  left_join(gcamdata_default_2015_glu_mean %>% 
              mutate(glu_id=as.integer(gsub("GLU","",GLU)))) -> plot_df
              
g <- ggplot()+
     geom_sf(data= plot_df, aes(fill= prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)


## allan scen 1 protection only

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_GCAM_plot_regXglu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_GCAM_plot_glu.pdf", plot=g, device="pdf")


## allan scen 1 protection plus default protection

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_plus_defprot_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_plus_defprot_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_plus_defprot_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_plus_defprot_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_plus_defprot_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_plus_defprot_GCAM_plot_glu.pdf", plot=g, device="pdf")


## allan scen 1 protection plus unavailable

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_plus_default_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_plus_default_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_plus_default_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_plus_default_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce1_plus_default_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce1_plus_default_GCAM_plot_glu.pdf", plot=g, device="pdf")


## allan scen 2 protection only

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_GCAM_plot_regXglu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_GCAM_plot_glu.pdf", plot=g, device="pdf")


## allan scen 2 protection plus default protection

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_plus_defprot_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_plus_defprot_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_plus_defprot_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_plus_defprot_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_plus_defprot_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_plus_defprot_GCAM_plot_glu.pdf", plot=g, device="pdf")


## allan scen 2 protection plus unavailable

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_plus_default_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_plus_default_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_plus_default_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_plus_default_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, Allan_sce2_plus_default_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("Allan_sce2_plus_default_GCAM_plot_glu.pdf", plot=g, device="pdf")


## 30% of each cell - protection only

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_GCAM_plot_regXglu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_GCAM_plot_glu.pdf", plot=g, device="pdf")


## 30% of each cell - overlayed with default protection

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_plus_defprot_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_plus_defprot_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_plus_defprot_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_plus_defprot_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_plus_defprot_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_plus_defprot_GCAM_plot_glu.pdf", plot=g, device="pdf")


## 30% of each cell - overlayed with default availability

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_plus_default_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_plus_default_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_plus_default_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_plus_default_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, thirty_percent_plus_default_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("thirty_percent_plus_default_GCAM_plot_glu.pdf", plot=g, device="pdf")



## existing protected only

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, GCAM_Protected_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("GCAM_Protected_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, GCAM_Protected_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("GCAM_Protected_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, GCAM_Protected_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("GCAM_Protected_GCAM_plot_glu.pdf", plot=g, device="pdf")


## default unavailable

# region X glu

shapefile <- sf::st_read("mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, GCAM_Unavailable_GCAM_plot_regXglu, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("GCAM_Unavailable_GCAM_plot_regXglu.pdf", plot=g, device="pdf")

# region

shapefile <- sf::st_read("mapping/region_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, GCAM_Unavailable_GCAM_plot_reg, join_by(reg_id == GCAM_region_ID) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("GCAM_Unavailable_GCAM_plot_reg.pdf", plot=g, device="pdf")

# glu

shapefile <- sf::st_read("mapping/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, GCAM_Unavailable_GCAM_plot_glu, join_by(glu_id == GLU) )

g <- ggplot()+
     geom_sf(data= plot_df, aes(fill=prot_frac))+
     scale_fill_viridis(limits=c(0,1))+
     scheme_basic

plot(g)
ggsave("GCAM_Unavailable_GCAM_plot_glu.pdf", plot=g, device="pdf")



