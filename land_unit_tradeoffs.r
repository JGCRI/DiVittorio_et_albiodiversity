# land_unit_tradeoffs.r
# look for high levels of relative land use and land cover change at the land unit level

# area units are thousand square km

library(rgcam)
library(dplyr)
library(reshape2)

# get the detailed land allocation

GCAM_defaultNZ <- rgcam::loadProject("./outputs/project_files/tables_gcam_defaultNZ.proj")
Allansce1NZ <- rgcam::loadProject("./outputs/project_files/tables_allansce1nz.proj")
Allansce2NZ <- rgcam::loadProject("./outputs/project_files/tables_allansce2nz.proj")
perc30NZ <- rgcam::loadProject("./outputs/project_files/tables_30percnz.proj")
GCAM_default <- rgcam::loadProject("./outputs/project_files/tables_gcam_default.proj")
Allansce1 <- rgcam::loadProject("./outputs/project_files/tables_allansce1.proj")
Allansce2 <- rgcam::loadProject("./outputs/project_files/tables_allansce2.proj")
perc30 <- rgcam::loadProject("./outputs/project_files/tables_30perc.proj")

# bind these for now, to make the landleaf transformation simpler
land_nz <- getQuery(GCAM_defaultNZ, "detailed land allocation") %>%
  mutate(scenario="CURRENT") %>% bind_rows(getQuery(Allansce1NZ, "detailed land allocation") %>%mutate(scenario="BIODIV")) %>%
  bind_rows(getQuery(Allansce2NZ, "detailed land allocation") %>%mutate(scenario="BIODIV30")) %>%
  bind_rows(getQuery(perc30NZ, "detailed land allocation") %>%mutate(scenario="UNIFORM30"))

# need to split the basin, land type, and tech
land_nz$Units = NULL
tl = strsplit(land_nz$landleaf, "_")
land_nz$basin_abr = sapply( tl, "[[", 2 )
land_nz$land_type = sapply( tl, "[[", 1 )
land_nz$water_tech = NA
land_nz$fert_tech = NA
wt_inds = which(sapply( tl, length ) > 2)
ft_inds = which(sapply( tl, length ) > 3)
land_nz$water_tech[wt_inds] = sapply( tl[wt_inds], "[[", 3 )
land_nz$fert_tech[ft_inds] = sapply( tl[ft_inds], "[[", 4 )
land_nz$landleaf = NULL

# aggregate to 6 types, drop the unchanging types
# non-grazed pasture is set as grassland
gcam_harv_forest_name = "Forest"
gcam_unman_forest_names = c("ProtectedUnmanagedForest", "UnmanagedForest", "UnsuitableUnmanagedForest")
gcam_pasture_names = c("Pasture")
gcam_grassland_names = c("Grassland", "ProtectedGrassland", "UnmanagedPasture", "ProtectedUnmanagedPasture", "UnsuitableGrassland", "UnsuitableUnmanagedPasture")
gcam_crop_names = c("CornC4", "FiberCrop", "FodderGrass", "FodderHerb", "FodderHerbC4", "Fruits", "FruitsTree", "Legumes", "MiscCrop",
                    "MiscCropTree", "NutsSeeds", "NutsSeedsTree", "OilCrop", "OilCropTree", "OilPalmTree", "OtherGrain", "OtherGrainC4", "Rice", "RootTuber", "Soybean",
                    "SugarCrop", "SugarCropC4", "Vegetables", "Wheat", "OtherArableLand")
gcam_biocrop_names = c("biomassGrass", "biomassTree")
gcam_shrubland_names = c("Shrubland", "ProtectedShrubland", "UnsuitableShrubland")
gcam_const_names = c("RockIceDesert", "Tundra", "UrbanLand")

#land_types_agg = c("biomass_crop", "crop", "unmanaged_forest", "harvested_forest", "grassland", "pasture", "shrubland")

land_nz = land_nz[!(land_nz$land_type %in% gcam_const_names),]
land_nz$land_type_agg = NA
land_nz$land_type_agg[land_nz$land_type %in% gcam_unman_forest_names] = "unmanaged_forest"
land_nz$land_type_agg[land_nz$land_type %in% gcam_grassland_names] = "grassland"
land_nz$land_type_agg[land_nz$land_type %in% gcam_shrubland_names] = "shrubland"
land_nz$land_type_agg[land_nz$land_type %in% c(gcam_crop_names, gcam_biocrop_names)] = "crop"
land_nz$land_type_agg[land_nz$land_type %in% gcam_harv_forest_name] = "harvested_forest"
land_nz$land_type_agg[land_nz$land_type %in% gcam_pasture_names] = "pasture"
#land_nz$land_type_agg[land_nz$land_type %in% gcam_biocrop_names] = "biomass_crop"

land_agg = aggregate(value ~ scenario + year + region + basin_abr + land_type_agg, data = land_nz, FUN = sum)
colnames(land_agg)[colnames(land_agg) == "value"] = "area_thous_sqkm"

# calculate percent change in area from 2015 to specified year
# drop the years before 2015 and move the years to columns
# biomass crop is referenced from 2025
ayear = 2050
ref_year = 2015
bio_ref_year = 2025
land_agg = land_agg[land_agg$year >= ref_year & land_agg$year <= ayear,]
land_data = dcast(land_agg, scenario + region + basin_abr + land_type_agg ~ year, value.var = "area_thous_sqkm")

a_year_col = which(colnames(land_data) == as.character(ayear))
ref_year_col = which(colnames(land_data) == as.character(ref_year))
bio_ref_year_col = which(colnames(land_data) == as.character(bio_ref_year))

land_data$area_change = NA
land_data$percent_area_change = NA

# not biomass crop
land_data$area_change[!(land_data$land_type_agg == "biomass_crop")] = land_data[!(land_data$land_type_agg == "biomass_crop"), a_year_col] - land_data[!(land_data$land_type_agg == "biomass_crop"), ref_year_col]
land_data$percent_area_change[!(land_data$land_type_agg == "biomass_crop")] = land_data$area_change[!(land_data$land_type_agg == "biomass_crop")] / land_data[!(land_data$land_type_agg == "biomass_crop"), ref_year_col] * 100
# biomass crop
land_data$area_change[land_data$land_type_agg == "biomass_crop"] = land_data[land_data$land_type_agg == "biomass_crop", a_year_col] - land_data[land_data$land_type_agg == "biomass_crop", bio_ref_year_col]
land_data$percent_area_change[land_data$land_type_agg == "biomass_crop"] = land_data$area_change[land_data$land_type_agg == "biomass_crop"] / land_data[land_data$land_type_agg == "biomass_crop", bio_ref_year_col] * 100

# set the NaNs to zero (0 divide by 0)
# so far no inf, but set to zero also anyway (value divide by zero)
na_inds_pac = which(is.na(land_data$percent_area_change))
inf_inds_pac = which(land_data$percent_area_change == Inf | land_data$percent_area_change == -Inf)
land_data$percent_area_change[c(na_inds_pac, inf_inds_pac)] = 0

# now put the percent changes for each scenario as columns
percent_change_data = dcast(land_data, region + basin_abr + land_type_agg ~ scenario, value.var = "percent_area_change")
area_change_data = dcast(land_data, region + basin_abr + land_type_agg ~ scenario, value.var = "area_change")

# identify the max/min percent change among the future cases
# calculate the range of percent change among the future cases
# this difference range indicates which land units are most sensitive to the future protection scenarios
percent_change_data$fut_change_max = apply(percent_change_data[,c("UNIFORM30", "BIODIV30", "BIODIV")], 1, max)
percent_change_data$fut_change_min = apply(percent_change_data[,c("UNIFORM30", "BIODIV30", "BIODIV")], 1, min)
percent_change_data$fut_change_range = percent_change_data$fut_change_max - percent_change_data$fut_change_min

# find the percent change values greater than a threshold percent

high_thresh = 25
lac_high = percent_change_data[percent_change_data$fut_change_range > high_thresh,]
high_units = unique(lac_high[,c("region", "basin_abr")])
nrow(high_units)

higher_thresh = 35
lac_higher = percent_change_data[percent_change_data$fut_change_range > higher_thresh,]
higher_units = unique(lac_higher[,c("region", "basin_abr")])
nrow(higher_units)

very_high_thresh = 50
lac_very_high = percent_change_data[percent_change_data$fut_change_range > very_high_thresh,]
very_high_units = unique(lac_very_high[,c("region", "basin_abr")])
nrow(very_high_units)

# write the high_thresh data to file
write.csv(lac_high, file="./supplemental_material/table_s6.csv", row.names=FALSE)