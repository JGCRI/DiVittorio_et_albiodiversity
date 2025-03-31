# proc_gcam_land_distribution_biodiv_temporal.r

# this is for gcam7 with suitable protected land split out
# read the gcam output proj files to get the land distribution data

# calculate areas and more for all years
# makes some maps for specified year
# do some stats on the time series

# input area is thous km^2
# output area is ha

# input land value is $/thous km^2
# output land value is $/ha
# land value of unmanaged land here is the opportunity cost of not converting it to use
# unsuitable land does not have value because it is not available for conversion
# we do value suitable proteted land separately, even though it is unavailable, because it could otherwise be converted
# the difference between the suitable protected land value and the available unmanaged land value
#    is the marginal opportunity cost of protecting suitable land

# the unmanaged land areas and values are merged into the managed land data frame
#    this means that the unmanaged land areas and values are correct even for the managed land records
#    note that there may be some land units or regions where there is only managed land or only unmanaged land

# note that the plotting functions for the maps don't work when running this as a function
# so set the arguments as variables in the console and run the code directly (don't run the function)

# make sure that this scripts directory is the working directory
setwd("./")

require(devtools)
library(rgcam)

library(raster)
library(rasterVis)
#library(rgdal)
library(sf)
library(RColorBrewer)
library(sp)
library(ggplot2)
library(randtests)
library(dplyr)
library(viridis)

# these are the default arguments for running code outside of the function

proj_names = c("outputs/project_files/tables_gcam_defaultNZ.proj", "outputs/project_files/tables_30percnz.proj",
				"outputs/project_files/tables_allansce1nz.proj", "outputs/project_files/tables_allansce2nz.proj")
out_names = c("current_nz", "uniform30_nz", "biodiv_nz", "biodiv30_nz")

#proj_names = c("outputs/project_files/tables_gcam_default.proj", "outputs/project_files/tables_30perc.proj",
#				"outputs/project_files/tables_allansce1.proj", "outputs/project_files/tables_allansce2.proj")
#out_names = c("current", "uniform30", "biodiv", "biodiv30")

gcam_reg_glu_rast_fname = "gcam_reg_glu_boundaries_moirai_land_cells_3p1_0p5arcmin.tif"
gcam_reg_glu_shape_lname = "reg_glu_boundaries_moirai_combined_3p1_0p5arcmin"
gcam_region_shape_lname = "region_boundaries_moirai_combined_3p1_0p5arcmin"
country_shape_lname = "country_boundaries_moirai_combined_3p1_0p5arcmin"
gcam_glu_name_map = "glu_to_gcamglu_mapping_c2021.csv"
gcam_reg_fname = "GCAM_region_names_32reg.csv"
rast_dir = "other_data/raster_files"
shape_dir = "other_data/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84"
indir = "other_data"
outdir = "outputs/gcam_land_distribution"
year = 2015


# not needed
# create a test proj file with detailed land allocation
#conn <- localDBConn(dbPath="../Rgcam_code/outputs_16feb2024/", dbFile="database_basexdbgcam_defaultNZ")
#scen_a_data <- addScenario(conn, "../Rgcam_code/outputs_16feb2024/test_gcam_default.proj", scenario=NULL, queryFile="land_allocation_query.xml")

HA2KMSQ = 1 / 100
HA2THOUSKMSQ = 1 / 100000
KMSQ2HA = 100
THOUSKMSQ2HA = 100000
	
PROJ4_STRING = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# these are the first part of the landleaf names, the second part is the abbrev. glu, third is IRR/RFD, fourth is hi/lo
# note that Forest is managed forest, and pasture is grazed pasture
# "Protected" is suitable protected and "Unsuitable" is the rest of the unavailable land
# for forest and pasture the first name must be the managed type
gcam_forest_names = c("UnmanagedForest", "ProtectedUnmanagedForest", "UnsuitableUnmanagedForest", "Forest")
# this order needs to match the corresponding 1:3 grassland names below
gcam_pasture_names = c("UnmanagedPasture", "ProtectedUnmanagedPasture", "UnsuitableUnmanagedPasture", "Pasture")
# this order needs to match the corresponding 1:3 pasture names above
gcam_grassland_names = c("Grassland", "ProtectedGrassland", "UnsuitableGrassland")
gcam_crop_names = c("biomassGrass", "biomassTree", "CornC4", "FiberCrop", "FodderGrass", "FodderHerb", "FodderHerbC4", "Fruits", "FruitsTree",
					"legumes", "MiscCrop", "MiscCropTree", "NutsSeeds", "NutsSeedsTree", "OilCrop", "OilCropTree", "OilPalmTree", "OtherGrain", "OtherGrainC4",
					"Rice", "RootTuber", "Soybean", "SugarCrop", "SugarCropC4", "Vegetables", "Wheat")
gcam_other_arable_name = "OtherArableLand"		# this is considered a managed cropland type
gcam_other_names = c("RockIceDesert", "Tundra")
gcam_shrubland_names = c("Shrubland", "ProtectedShrubland", "UnsuitableShrubland")
gcam_urban_name = "UrbanLand"

unmanaged_names = c("AvailableUnmanaged", "SuitableProtectedUnmanaged", "UnsuitableUnmanaged")

all_unmanaged_names = c("AllUnmanagedForest", "AllUnmanagedGrass", "AllUnmanagedShrub")
all_managed_names = c("ManagedForest", "Pasture", "Cropland")


proc_gcam_land_distribution <- function(	proj_names = c("outputs/project_files/tables_gcam_defaultNZ.proj",
											"outputs/project_files/tables_30percnz.proj",
											"outputs/project_files/tables_allansce1nz.proj",
											"outputs/project_files/tables_allansce2nz.proj"),
											out_names = c("current_nz", "uniform30_nz", "biodiv_nz", "biodiv30_nz"),
											gcam_reg_glu_rast_fname = "gcam_reg_glu_boundaries_moirai_land_cells_3p1_0p5arcmin.tif",
											gcam_reg_glu_shape_lname = "reg_glu_boundaries_moirai_combined_3p1_0p5arcmin",
											gcam_region_shape_lname = "region_boundaries_moirai_combined_3p1_0p5arcmin",
											country_shape_lname = "country_boundaries_moirai_combined_3p1_0p5arcmin",
											gcam_glu_name_map = "glu_to_gcamglu_mapping_c2021.csv",
											gcam_reg_fname = "GCAM_region_names_32reg.csv",
											rast_dir = "other_data/raster_files",
											shape_dir = "other_data/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84",
											indir = "other_data",
											outdir = "outputs/gcam_land_distribution",
											year = 2015) {
	
	cat("start proc_gcam_land_distribution_biodiv_temporal at", date(), "\n")
	
	# ensure that the paths end with "/"
	# except for the shape dir which cannot have the / for read OGR
  	if(substr(indir,nchar(indir), nchar(indir)) != "/") { indir = paste0(indir, "/") }
  	if(substr(outdir,nchar(outdir), nchar(outdir)) != "/") { outdir = paste0(outdir, "/") }
  	if(substr(rast_dir,nchar(rast_dir), nchar(rast_dir)) != "/") { rast_dir = paste0(rast_dir, "/") }
  	dir.create(outdir, recursive=TRUE)
  	
  	# make sure the shape dir does not end with "/"
	if(substr(shape_dir,nchar(shape_dir), nchar(shape_dir)) == "/") { shape_dir = substr(shape_dir,1, nchar(shape_dir)-1 ) }
	
	# read in the gcam glu name-abbr-id mapping
	glu_name_map = read.csv(paste0(indir, gcam_glu_name_map), header=TRUE, skip=7, stringsAsFactors=FALSE)
	
	# read in the region ids
	region_ids = read.csv(paste0(indir, gcam_reg_fname), header=TRUE, skip=3, stringsAsFactors=FALSE)
	
	# loop over the project data files; each one has one scenario
	num_scen = length(proj_names)
	for(s in 1:num_scen) {
	
		# load the gcam project file output
		data = loadProject(proj_names[s])
	
		# get the land distribution data (thous km^2)
		land = getQuery(data, query="detailed land allocation")
		
		# get the land value data ($/thous km^2)
		value = getQuery(data, query="profit rate")
	
		# keep all years, don't need units col
		# the in units are thousand km^2, so convert them to ha for now
		#land = land[land$year == year, c("scenario", "region", "landleaf", "value")]
		land$lt_gcam_area_ha = land$value * THOUSKMSQ2HA
		land$value = NULL
		land$Units = NULL
		
		#value = value[value$year == year, c("scenario", "region", "landleaf", "value")]
		value$Units = NULL
		colnames(value) = c("scenario", "region", "landleaf", "year", "value_usd_per_ha")
		value$value_usd_per_ha = value$value_usd_per_ha / THOUSKMSQ2HA
		
	
	    # include the land value here, and deal with it appropriately below
	    land = merge(land, value, by = c("scenario", "region", "landleaf", "year"), all.x=TRUE, sort=FALSE)	
	
		# separate the land type and basin names and determine the glu code
		land$LT_GCAM = sapply(strsplit(land$landleaf,"_"),"[[",1)
		land$gcam_glu_abbr = sapply(strsplit(land$landleaf,"_"),"[[",2)
		land$water[land$LT_GCAM %in% gcam_crop_names] = 
			sapply(strsplit(land$landleaf[land$LT_GCAM %in% gcam_crop_names],"_"),"[[",3)
		land$fert[land$LT_GCAM %in% gcam_crop_names] =
			sapply(strsplit(land$landleaf[land$LT_GCAM %in% gcam_crop_names],"_"),"[[",4)
		land$landleaf = NULL
		land = merge(land, glu_name_map[,c("GCAM_basin_ID", "GLU_name")], by.x = c("gcam_glu_abbr"), by.y = c("GLU_name"), all.x = TRUE, sort = FALSE)
		
		# now merge land types into: crop, managed forest, managed pasture, fixed (urban, rockicedesert, tundra), and unmanaged grassland, forest, shrubland
		#    with three columns for unmanaged forest, grassland, shrubland: available (suit unprot), suitable protected, unsuitable (prot and unprot)
		#    unmanaged pasture will be combined with the corresponding grassland categories
		
		###### calculate the forest records
		gcam_forest = land[land$LT_GCAM %in% gcam_forest_names,]
		gcam_mforest = gcam_forest[gcam_forest$LT_GCAM==gcam_forest_names[4],]
		gcam_mforest$LT_GCAM = all_managed_names[1]
		gcam_mforest$water = NULL
		gcam_mforest$fert = NULL
		gcam_mforest = gcam_mforest[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha", "value_usd_per_ha")]
		
		# keep separate unmanaged forest records in case no managed exists in a glu
		temp = gcam_forest[gcam_forest$LT_GCAM %in% gcam_forest_names[1:3],]
		gcam_umforest = aggregate(temp$lt_gcam_area_ha, by = list(temp$scenario, temp$region, temp$GCAM_basin_ID, temp$gcam_glu_abbr, temp$year), FUN = sum)
		colnames(gcam_umforest) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")
		gcam_umforest$LT_GCAM = all_unmanaged_names[1]
		gcam_umforest = gcam_umforest[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha")]
		# need to area weight the value
		gcam_umforest_value = aggregate(value_usd_per_ha * lt_gcam_area_ha ~ scenario + region + GCAM_basin_ID + gcam_glu_abbr + year, data = temp, FUN = sum)
		colnames(gcam_umforest_value) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")
		gcam_umforest = merge(gcam_umforest, gcam_umforest_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_umforest$value_usd_per_ha = gcam_umforest$value_usd_per_ha / gcam_umforest$lt_gcam_area_ha
		
		gcam_forest = rbind(gcam_mforest, gcam_umforest)
		# now add columns for the different unmanaged type areas
		# add both the area and the value
		for(u in 1:3){
			gcam_forest = merge(gcam_forest, temp[temp$LT_GCAM==gcam_forest_names[u], c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
			colnames(gcam_forest)[length(colnames(gcam_forest))] = paste0(unmanaged_names[u],"_area_ha")
			gcam_forest = merge(gcam_forest, temp[temp$LT_GCAM==gcam_forest_names[u], c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
			colnames(gcam_forest)[length(colnames(gcam_forest))] = paste0(unmanaged_names[u],"_value_usd_per_ha")
		}
		colnames(gcam_forest)[7] = "lt_gcam_area_ha"
		colnames(gcam_forest)[8] = "value_usd_per_ha"
												
	
		###### calculate the pasture/grass records
		gcam_pasture_in = land[land$LT_GCAM %in% gcam_pasture_names,]
		gcam_pasture_in$water = NULL
		gcam_pasture_in$fert = NULL
		gcam_pasture = gcam_pasture_in[gcam_pasture_in$LT_GCAM==gcam_pasture_names[4],]
		gcam_pasture = gcam_pasture[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha", "value_usd_per_ha")]
	
		# need to sum the unmanaged pasture with the grassland
		gcam_grass_in = land[land$LT_GCAM %in% gcam_grassland_names,]
		gcam_grass_in$water = NULL
		gcam_grass_in$fert = NULL
		gcam_gp_in = NULL
		for(g in 1:3){
			temp = merge(gcam_grass_in[gcam_grass_in$LT_GCAM==gcam_grassland_names[g],], gcam_pasture_in[gcam_pasture_in$LT_GCAM==gcam_pasture_names[g],],
						by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), sort=FALSE)
			temp = temp[!is.na(temp$scenario),]
			temp$LT_GCAM = temp$LT_GCAM.x
			temp$lt_gcam_area_ha = temp$lt_gcam_area_ha.x + temp$lt_gcam_area_ha.y
			temp$LT_GCAM.x = NULL
			temp$LT_GCAM.y = NULL
			# area weight the value
			temp$value_usd_per_ha = (temp$lt_gcam_area_ha.x * temp$value_usd_per_ha.x + temp$lt_gcam_area_ha.y * temp$value_usd_per_ha.y) /
										(temp$lt_gcam_area_ha.x + temp$lt_gcam_area_ha.y)							
			temp$lt_gcam_area_ha.x = NULL
			temp$lt_gcam_area_ha.y = NULL
			temp$value_usd_per_ha.x = NULL
			temp$value_usd_per_ha.y = NULL
			
			gcam_gp_in = rbind(gcam_gp_in, temp)
		}
		
		# note that some units do not have any unmanaged land, but still have pasture
		gcam_umgrass = aggregate(gcam_gp_in$lt_gcam_area_ha, by = list(gcam_gp_in$scenario, gcam_gp_in$region, gcam_gp_in$GCAM_basin_ID, gcam_gp_in$gcam_glu_abbr, gcam_gp_in$year), FUN = sum)
		colnames(gcam_umgrass) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")
		gcam_umgrass$LT_GCAM = all_unmanaged_names[2]
		gcam_umgrass = gcam_umgrass[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha")]
		
		# need to area weight the value
		gcam_umgrass_value = aggregate(value_usd_per_ha * lt_gcam_area_ha ~ scenario + region + GCAM_basin_ID + gcam_glu_abbr + year, data = gcam_gp_in, FUN = sum)
		colnames(gcam_umgrass_value) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")
		gcam_umgrass = merge(gcam_umgrass, gcam_umgrass_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_umgrass$value_usd_per_ha = gcam_umgrass$value_usd_per_ha / gcam_umgrass$lt_gcam_area_ha
		
		gcam_grass = rbind(gcam_pasture, gcam_umgrass)
		# now add columns for the different unmanaged type areas
		for(u in 1:3){
			gcam_grass = merge(gcam_grass, gcam_gp_in[gcam_gp_in$LT_GCAM==gcam_grassland_names[u], c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
			colnames(gcam_grass)[length(colnames(gcam_grass))] = paste0(unmanaged_names[u],"_area_ha")
			gcam_grass = merge(gcam_grass, gcam_gp_in[gcam_gp_in$LT_GCAM==gcam_grassland_names[u], c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
			colnames(gcam_grass)[length(colnames(gcam_grass))] = paste0(unmanaged_names[u],"_value_usd_per_ha")
		}
		colnames(gcam_grass)[7] = "lt_gcam_area_ha"
		colnames(gcam_grass)[8] = "value_usd_per_ha"								
			
		
		###### calculate the shrubland records
		
		gcam_shrub_in = land[land$LT_GCAM %in% gcam_shrubland_names,]
		gcam_shrub_in$water = NULL
		gcam_shrub_in$fert = NULL
		gcam_shrub = aggregate(gcam_shrub_in$lt_gcam_area_ha, by = list(gcam_shrub_in$scenario, gcam_shrub_in$region, gcam_shrub_in$GCAM_basin_ID, gcam_shrub_in$gcam_glu_abbr, gcam_shrub_in$year), FUN = sum)
		colnames(gcam_shrub) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")
		gcam_shrub$LT_GCAM = all_unmanaged_names[3]
		gcam_shrub = gcam_shrub[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha")]
		
		# need to area weight the value
		gcam_shrub_value = aggregate(value_usd_per_ha * lt_gcam_area_ha ~ scenario + region + GCAM_basin_ID + gcam_glu_abbr + year, data = gcam_shrub_in, FUN = sum)
		colnames(gcam_shrub_value) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")
		gcam_shrub = merge(gcam_shrub, gcam_shrub_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_shrub$value_usd_per_ha = gcam_shrub$value_usd_per_ha / gcam_shrub$lt_gcam_area_ha
		
		# now add columns for the different unmanaged type areas
		for(u in 1:3){
			gcam_shrub = merge(gcam_shrub, gcam_shrub_in[gcam_shrub_in$LT_GCAM==gcam_shrubland_names[u], c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
			colnames(gcam_shrub)[length(colnames(gcam_shrub))] = paste0(unmanaged_names[u],"_area_ha")
			gcam_shrub = merge(gcam_shrub, gcam_shrub_in[gcam_shrub_in$LT_GCAM==gcam_shrubland_names[u], c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
			colnames(gcam_shrub)[length(colnames(gcam_shrub))] = paste0(unmanaged_names[u],"_value_usd_per_ha")
		}
		colnames(gcam_shrub)[7] = "lt_gcam_area_ha"
		colnames(gcam_shrub)[8] = "value_usd_per_ha"

		
		###### calculate the crop records, including other arable land
		gcam_crop_in = land[land$LT_GCAM %in% gcam_crop_names | land$LT_GCAM %in% gcam_other_arable_name,]
		gcam_crop = aggregate(gcam_crop_in$lt_gcam_area_ha, by = list(gcam_crop_in$scenario, gcam_crop_in$region, gcam_crop_in$GCAM_basin_ID, gcam_crop_in$gcam_glu_abbr, gcam_crop_in$year), FUN = sum)
		colnames(gcam_crop) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")
		gcam_crop$LT_GCAM = all_managed_names[3]
		gcam_crop = gcam_crop[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha")]
		
		# need to area weight the value
		gcam_crop_value = aggregate(value_usd_per_ha * lt_gcam_area_ha ~ scenario + region + GCAM_basin_ID + gcam_glu_abbr + year, data = gcam_crop_in, FUN = sum)
		colnames(gcam_crop_value) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")
		gcam_crop = merge(gcam_crop, gcam_crop_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_crop$value_usd_per_ha = gcam_crop$value_usd_per_ha / gcam_crop$lt_gcam_area_ha
		
		
		###### calculate the static land (urban, rockicedesert, tundra) records
		gcam_static_in = land[land$LT_GCAM %in% gcam_other_names | land$LT_GCAM %in% gcam_urban_name,]
		gcam_static = aggregate(gcam_static_in$lt_gcam_area_ha,
			by = list(gcam_static_in$scenario, gcam_static_in$region, gcam_static_in$GCAM_basin_ID, gcam_static_in$gcam_glu_abbr, gcam_static_in$year), FUN = sum)
		colnames(gcam_static) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")
		gcam_static$LT_GCAM = "Static"
		gcam_static = gcam_static[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha")]
		
		# need to area weight the value
		gcam_static_value = aggregate(value_usd_per_ha * lt_gcam_area_ha ~ scenario + region + GCAM_basin_ID + gcam_glu_abbr + year, data = gcam_static_in, FUN = sum)
		colnames(gcam_static_value) = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")
		gcam_static = merge(gcam_static, gcam_static_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_static$value_usd_per_ha = gcam_static$value_usd_per_ha / gcam_static$lt_gcam_area_ha


		# add extra columns to crop and static to match the unmanaged df structure
		for(u in 1:3){
			gcam_crop[,paste0(unmanaged_names[u],"_area_ha")] = NA
			gcam_crop[,paste0(unmanaged_names[u],"_value_usd_per_ha")] = NA
			gcam_static[,paste0(unmanaged_names[u],"_area_ha")] = NA
			gcam_static[,paste0(unmanaged_names[u],"_value_usd_per_ha")] = NA
		}

		#### bind all the records into one data frame for each aggregation
		gcam_land1 = rbind(gcam_forest, gcam_grass, gcam_shrub, gcam_crop, gcam_static)
		gcam_land1 = gcam_land1[order(gcam_land1$year, gcam_land1$scenario, gcam_land1$region, gcam_land1$GCAM_basin_ID, gcam_land1$gcam_glu_abbr, gcam_land1$LT_GCAM),]


		# calculate total unmanaged land
	
		# by land unit
		gcam_unmanaged_land = aggregate(cbind(lt_gcam_area_ha, AvailableUnmanaged_area_ha, SuitableProtectedUnmanaged_area_ha, UnsuitableUnmanaged_area_ha) ~
										scenario + region + GCAM_basin_ID + gcam_glu_abbr + year,
										data = gcam_land1[gcam_land1$LT_GCAM %in% all_unmanaged_names,], FUN = sum)
		gcam_unmanaged_land$LT_GCAM = "AllUnmanaged"
		
		# need to area weight the value - but aggregate is behaving weirdly so do it differently
		temp = gcam_land1[gcam_land1$LT_GCAM %in% all_unmanaged_names,]
		temp$value_usd_per_ha = temp$value_usd_per_ha * temp$lt_gcam_area_ha
		temp$AvailableUnmanaged_value_usd_per_ha = temp$AvailableUnmanaged_value_usd_per_ha * temp$AvailableUnmanaged_area_ha
		temp$SuitableProtectedUnmanaged_value_usd_per_ha = temp$SuitableProtectedUnmanaged_value_usd_per_ha * temp$SuitableProtectedUnmanaged_area_ha
		temp$UnsuitableUnmanaged_value_usd_per_ha = temp$UnsuitableUnmanaged_value_usd_per_ha * temp$UnsuitableUnmanaged_area_ha
		gcam_unmanaged_land_value = aggregate(cbind(value_usd_per_ha = temp$value_usd_per_ha, AvailableUnmanaged_value_usd_per_ha = temp$AvailableUnmanaged_value_usd_per_ha,
								SuitableProtectedUnmanaged_value_usd_per_ha = temp$SuitableProtectedUnmanaged_value_usd_per_ha,
								UnsuitableUnmanaged_value_usd_per_ha = temp$UnsuitableUnmanaged_value_usd_per_ha),
								by=list(scenario = temp$scenario, region = temp$region, GCAM_basin_ID = temp$GCAM_basin_ID, gcam_glu_abbr = temp$gcam_glu_abbr, year = temp$year), FUN = sum, na.rm=TRUE)
																																						
		gcam_unmanaged_land = merge(gcam_unmanaged_land, gcam_unmanaged_land_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_unmanaged_land$value_usd_per_ha = gcam_unmanaged_land$value_usd_per_ha / gcam_unmanaged_land$lt_gcam_area_ha
		gcam_unmanaged_land$AvailableUnmanaged_value_usd_per_ha = gcam_unmanaged_land$AvailableUnmanaged_value_usd_per_ha / gcam_unmanaged_land$AvailableUnmanaged_area_ha
		gcam_unmanaged_land$SuitableProtectedUnmanaged_value_usd_per_ha = gcam_unmanaged_land$SuitableProtectedUnmanaged_value_usd_per_ha / gcam_unmanaged_land$SuitableProtectedUnmanaged_area_ha
		gcam_unmanaged_land$UnsuitableUnmanaged_value_usd_per_ha = gcam_unmanaged_land$UnsuitableUnmanaged_value_usd_per_ha / gcam_unmanaged_land$UnsuitableUnmanaged_area_ha
		
		gcam_unmanaged_land = gcam_unmanaged_land[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha","value_usd_per_ha",
												"AvailableUnmanaged_area_ha", "AvailableUnmanaged_value_usd_per_ha",
												"SuitableProtectedUnmanaged_area_ha", "SuitableProtectedUnmanaged_value_usd_per_ha",
												"UnsuitableUnmanaged_area_ha", "UnsuitableUnmanaged_value_usd_per_ha")]
		
		# calculate total managed land
	
		# by land unit
		gcam_managed_land = aggregate(cbind(lt_gcam_area_ha) ~
										scenario + region + GCAM_basin_ID + gcam_glu_abbr + year,
										data = gcam_land1[gcam_land1$LT_GCAM %in% all_managed_names,], FUN = sum)
		gcam_managed_land$LT_GCAM = "AllManaged"
		
		# need to area weight the value - but aggregate is behaving weirdly so do it differently
		temp = gcam_land1[gcam_land1$LT_GCAM %in% all_managed_names,]
		temp$value_usd_per_ha = 	temp$value_usd_per_ha * temp$lt_gcam_area_ha
		gcam_managed_land_value = aggregate(cbind(value_usd_per_ha = temp$value_usd_per_ha),
								by=list(scenario = temp$scenario, region = temp$region, GCAM_basin_ID = temp$GCAM_basin_ID, gcam_glu_abbr = temp$gcam_glu_abbr, year = temp$year), FUN = sum, na.rm=TRUE)
																																						
		gcam_managed_land = merge(gcam_managed_land, gcam_managed_land_value, by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x=TRUE, sort=FALSE)
		gcam_managed_land$value_usd_per_ha = gcam_managed_land$value_usd_per_ha / gcam_managed_land$lt_gcam_area_ha
		#gcam_managed_land$AvailableUnmanaged_area_ha = NA
		#gcam_managed_land$SuitableProtectedUnmanaged_area_ha = NA
		#gcam_managed_land$UnsuitableUnmanaged_area_ha = NA
		#gcam_managed_land$AvailableUnmanaged_value_usd_per_ha = NA
		#gcam_managed_land$SuitableProtectedUnmanaged_value_usd_per_ha = NA
		#gcam_managed_land$UnsuitableUnmanaged_value_usd_per_ha = NA
		
		gcam_managed_land = gcam_managed_land[,c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "LT_GCAM", "lt_gcam_area_ha","value_usd_per_ha")]


		#### combine the land and unmanaged land and managed land tables for writing and plotting
		# but add the managed land as columns just to the AllUnmanaaged records
		gcam_land = rbind(gcam_land1, gcam_unmanaged_land)
		
		gcam_land = merge(gcam_land, gcam_managed_land[, c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "lt_gcam_area_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
		colnames(gcam_land)[length(colnames(gcam_land))] = "AllManaged_area_ha"
		colnames(gcam_land)[which(colnames(gcam_land) == "lt_gcam_area_ha.x")] = "lt_gcam_area_ha"
		gcam_land = merge(gcam_land, gcam_managed_land[, c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year", "value_usd_per_ha")],
								by = c("scenario", "region", "GCAM_basin_ID", "gcam_glu_abbr", "year"), all.x = TRUE, sort = FALSE)
		colnames(gcam_land)[length(colnames(gcam_land))] = "AllManaged_value_usd_per_ha"
		colnames(gcam_land)[which(colnames(gcam_land) == "value_usd_per_ha.x")] = "value_usd_per_ha"
		gcam_land$AllManaged_area_ha[!gcam_land$LT_GCAM=="AllUnmanaged"] = NA
		gcam_land$AllManaged_value_usd_per_ha[!gcam_land$LT_GCAM=="AllUnmanaged"] = NA

		# aggregate to gcam region for regional analysis

		gcam_land_reg = aggregate(cbind(lt_gcam_area_ha = gcam_land$lt_gcam_area_ha, AvailableUnmanaged_area_ha = gcam_land$AvailableUnmanaged_area_ha,
								SuitableProtectedUnmanaged_area_ha = gcam_land$SuitableProtectedUnmanaged_area_ha,
								UnsuitableUnmanaged_area_ha = gcam_land$UnsuitableUnmanaged_area_ha, AllManaged_area_ha  = gcam_land$AllManaged_area_ha),
								by=list(scenario = gcam_land$scenario, region = gcam_land$region, year = gcam_land$year, LT_GCAM = gcam_land$LT_GCAM), FUN = sum, na.rm=TRUE)
											
		# need to area weight the value - but aggregate is behaving weirdly so do it differently
		temp = gcam_land
		temp$value_usd_per_ha = temp$value_usd_per_ha * temp$lt_gcam_area_ha
		temp$AvailableUnmanaged_value_usd_per_ha = temp$AvailableUnmanaged_value_usd_per_ha * temp$AvailableUnmanaged_area_ha
		temp$SuitableProtectedUnmanaged_value_usd_per_ha = temp$SuitableProtectedUnmanaged_value_usd_per_ha * temp$SuitableProtectedUnmanaged_area_ha
		temp$UnsuitableUnmanaged_value_usd_per_ha = temp$UnsuitableUnmanaged_value_usd_per_ha * temp$UnsuitableUnmanaged_area_ha
		temp$AllManaged_value_usd_per_ha = temp$AllManaged_value_usd_per_ha * temp$AllManaged_area_ha
		gcam_land_reg_value = aggregate(cbind(value_usd_per_ha = temp$value_usd_per_ha, AvailableUnmanaged_value_usd_per_ha = temp$AvailableUnmanaged_value_usd_per_ha,
								SuitableProtectedUnmanaged_value_usd_per_ha = temp$SuitableProtectedUnmanaged_value_usd_per_ha,
								UnsuitableUnmanaged_value_usd_per_ha = temp$UnsuitableUnmanaged_value_usd_per_ha, AllManaged_value_usd_per_ha = temp$AllManaged_value_usd_per_ha),
								by=list(scenario = temp$scenario, region = temp$region, year = temp$year, LT_GCAM = temp$LT_GCAM), FUN = sum, na.rm=TRUE)
																																						
		gcam_land_reg = merge(gcam_land_reg, gcam_land_reg_value, by = c("scenario", "region", "year", "LT_GCAM"), all.x=TRUE, sort=FALSE)
		gcam_land_reg$value_usd_per_ha = gcam_land_reg$value_usd_per_ha / gcam_land_reg$lt_gcam_area_ha
		gcam_land_reg$AvailableUnmanaged_value_usd_per_ha = gcam_land_reg$AvailableUnmanaged_value_usd_per_ha / gcam_land_reg$AvailableUnmanaged_area_ha
		gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha = gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha / gcam_land_reg$SuitableProtectedUnmanaged_area_ha
		gcam_land_reg$UnsuitableUnmanaged_value_usd_per_ha = gcam_land_reg$UnsuitableUnmanaged_value_usd_per_ha / gcam_land_reg$UnsuitableUnmanaged_area_ha
		gcam_land_reg$AllManaged_value_usd_per_ha = gcam_land_reg$AllManaged_value_usd_per_ha / gcam_land_reg$AllManaged_area_ha
										
		temp_globe = aggregate(cbind(lt_gcam_area_ha = gcam_land_reg$lt_gcam_area_ha, AvailableUnmanaged_area_ha = gcam_land_reg$AvailableUnmanaged_area_ha,
							SuitableProtectedUnmanaged_area_ha = gcam_land_reg$SuitableProtectedUnmanaged_area_ha,
							UnsuitableUnmanaged_area_ha = gcam_land_reg$UnsuitableUnmanaged_area_ha, AllManaged_area_ha  = gcam_land_reg$AllManaged_area_ha),
							by=list(scenario = gcam_land_reg$scenario, year = gcam_land_reg$year, LT_GCAM = gcam_land_reg$LT_GCAM), FUN = sum, na.rm=TRUE)
										
		region = rep("Global", nrow(temp_globe))
		temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
		
		# aggregate the land value
		temp = gcam_land_reg
		temp$value_usd_per_ha = temp$value_usd_per_ha * temp$lt_gcam_area_ha
		temp$AvailableUnmanaged_value_usd_per_ha = temp$AvailableUnmanaged_value_usd_per_ha * temp$AvailableUnmanaged_area_ha
		temp$SuitableProtectedUnmanaged_value_usd_per_ha = temp$SuitableProtectedUnmanaged_value_usd_per_ha * temp$SuitableProtectedUnmanaged_area_ha
		temp$UnsuitableUnmanaged_value_usd_per_ha = temp$UnsuitableUnmanaged_value_usd_per_ha * temp$UnsuitableUnmanaged_area_ha
		temp$AllManaged_value_usd_per_ha = temp$AllManaged_value_usd_per_ha * temp$AllManaged_area_ha
		temp_globe_value = aggregate(cbind(value_usd_per_ha = temp$value_usd_per_ha, AvailableUnmanaged_value_usd_per_ha = temp$AvailableUnmanaged_value_usd_per_ha,
							SuitableProtectedUnmanaged_value_usd_per_ha = temp$SuitableProtectedUnmanaged_value_usd_per_ha,
							UnsuitableUnmanaged_value_usd_per_ha = temp$UnsuitableUnmanaged_value_usd_per_ha, AllManaged_value_usd_per_ha = temp$AllManaged_value_usd_per_ha),
							by=list(scenario = temp$scenario, year = temp$year, LT_GCAM = temp$LT_GCAM), FUN = sum, na.rm=TRUE)
		
		temp_globe = merge(temp_globe, temp_globe_value, by = c("scenario", "year", "LT_GCAM"), all.x=TRUE, sort=FALSE)
		temp_globe$value_usd_per_ha = temp_globe$value_usd_per_ha / temp_globe$lt_gcam_area_ha
		temp_globe$AvailableUnmanaged_value_usd_per_ha = temp_globe$AvailableUnmanaged_value_usd_per_ha / temp_globe$AvailableUnmanaged_area_ha
		temp_globe$SuitableProtectedUnmanaged_value_usd_per_ha = temp_globe$SuitableProtectedUnmanaged_value_usd_per_ha / temp_globe$SuitableProtectedUnmanaged_area_ha
		temp_globe$UnsuitableUnmanaged_value_usd_per_ha = temp_globe$UnsuitableUnmanaged_value_usd_per_ha / temp_globe$UnsuitableUnmanaged_area_ha
		temp_globe$AllManaged_value_usd_per_ha = temp_globe $AllManaged_value_usd_per_ha / temp_globe$AllManaged_area_ha
		
		gcam_land_reg = rbind(gcam_land_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
		gcam_land_reg = gcam_land_reg[order(gcam_land_reg$year, gcam_land_reg$scenario, gcam_land_reg$region, gcam_land_reg$LT_GCAM),]

		gcam_land_reg = gcam_land_reg[, c("scenario", "region", "year", "LT_GCAM", "lt_gcam_area_ha","value_usd_per_ha",
												"AvailableUnmanaged_area_ha", "AvailableUnmanaged_value_usd_per_ha",
												"SuitableProtectedUnmanaged_area_ha", "SuitableProtectedUnmanaged_value_usd_per_ha",
												"UnsuitableUnmanaged_area_ha", "UnsuitableUnmanaged_value_usd_per_ha",
												"AllManaged_area_ha", "AllManaged_value_usd_per_ha")]	

		# calculate the ratio of suitable protected to available unmanaged area
		gcam_land$SuitProtUnman2AvailUnman_ratio = gcam_land $SuitableProtectedUnmanaged_area_ha / gcam_land $AvailableUnmanaged_area_ha
		gcam_land_reg$SuitProtUnman2AvailUnman_ratio = gcam_land_reg$SuitableProtectedUnmanaged_area_ha / gcam_land_reg$AvailableUnmanaged_area_ha
		
		# calculate the percent of convertible land available
		gcam_land$percent_conv_avail = 100 *
			gcam_land$AvailableUnmanaged_area_ha / gcam_land$lt_gcam_area_ha
		gcam_land_reg$percent_conv_avail = 100 *
			gcam_land_reg$AvailableUnmanaged_area_ha / gcam_land_reg$lt_gcam_area_ha
		
		
		#### calculate the ratio of the suitable protected land value to the avaialble unmanaged land value
		gcam_land$SuitProtUnman2AvailUnman_value_ratio = gcam_land$SuitableProtectedUnmanaged_value_usd_per_ha / gcam_land$AvailableUnmanaged_value_usd_per_ha
		gcam_land_reg$SuitProtUnman2AvailUnman_value_ratio = gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha / gcam_land_reg$AvailableUnmanaged_value_usd_per_ha
		
		#### calculate the marginal cost of protection as the difference between the suitable protected land value to the unmanaged land value
        #### normalize it by the unmanaged land value (this is just the ratio minus 1)
		gcam_land$prot_cost_norm = ( gcam_land$SuitableProtectedUnmanaged_value_usd_per_ha - gcam_land$AvailableUnmanaged_value_usd_per_ha ) /
			gcam_land$AvailableUnmanaged_value_usd_per_ha
		gcam_land_reg$prot_cost_norm = ( gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha - gcam_land_reg$AvailableUnmanaged_value_usd_per_ha ) /
			gcam_land_reg$AvailableUnmanaged_value_usd_per_ha  
		
		#### calculate the ratio of the suitable protected land area to the managed land area
		gcam_land$SuitProtUnman2Man_ratio = gcam_land$SuitableProtectedUnmanaged_area_ha / gcam_land$AllManaged_area_ha
		gcam_land_reg$SuitProtUnman2Man_ratio = gcam_land_reg$SuitableProtectedUnmanaged_area_ha / gcam_land_reg$AllManaged_area_ha
		
		#### calculate the ratio of the suitable protected land value to the managed land value
		gcam_land$SuitProtUnman2Man_value_ratio = gcam_land$SuitableProtectedUnmanaged_value_usd_per_ha / gcam_land$AllManaged_value_usd_per_ha
		gcam_land_reg$SuitProtUnman2Man_value_ratio = gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha / gcam_land_reg$AllManaged_value_usd_per_ha

		gcam_land = gcam_land[order(gcam_land$scenario, gcam_land$region, gcam_land$LT_GCAM),]
		gcam_land_reg = gcam_land_reg[order(gcam_land_reg$scenario, gcam_land_reg$region, gcam_land_reg$LT_GCAM),]
		
		# need to deal with Inf values!!!
		# leave na/nan for now as nodata values
		gcam_land$AvailableUnmanaged_value_usd_per_ha[gcam_land$AvailableUnmanaged_value_usd_per_ha == Inf] = NA
		gcam_land_reg$AvailableUnmanaged_value_usd_per_ha[gcam_land_reg$AvailableUnmanaged_value_usd_per_ha == Inf] = NA
		
		gcam_land$SuitableProtectedUnmanaged_value_usd_per_ha[gcam_land$SuitableProtectedUnmanaged_value_usd_per_ha == Inf] = NA
		gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha[gcam_land_reg$SuitableProtectedUnmanaged_value_usd_per_ha == Inf] = NA
		
		gcam_land$UnsuitableUnmanaged_value_usd_per_ha[gcam_land$UnsuitableUnmanaged_value_usd_per_ha == Inf] = NA
		gcam_land_reg$UnsuitableUnmanaged_value_usd_per_ha[gcam_land_reg$UnsuitableUnmanaged_value_usd_per_ha == Inf] = NA
		
		gcam_land$SuitProtUnman2AvailUnman_ratio[gcam_land$SuitProtUnman2AvailUnman_ratio == Inf] = NA
		gcam_land_reg$SuitProtUnman2AvailUnman_ratio[gcam_land_reg$SuitProtUnman2AvailUnman_ratio == Inf] = NA
		
		gcam_land$SuitProtUnman2AvailUnman_value_ratio[gcam_land$SuitProtUnman2AvailUnman_value_ratio == Inf] = NA
		gcam_land_reg$SuitProtUnman2AvailUnman_value_ratio[gcam_land_reg$SuitProtUnman2AvailUnman_value_ratio == Inf] = NA
		
		gcam_land$prot_cost_norm[gcam_land$prot_cost_norm == Inf] = NA
		gcam_land_reg$prot_cost_norm[gcam_land_reg$prot_cost_norm == Inf] = NA
		
		gcam_land$percent_conv_avail[gcam_land$percent_conv_avail == Inf] = NA
		gcam_land_reg$percent_conv_avail[gcam_land_reg$percent_conv_avail == Inf] = NA
		
		gcam_land$SuitProtUnman2Man_ratio[gcam_land$SuitProtUnman2Man_ratio == Inf] = NA
		gcam_land_reg$SuitProtUnman2Man_ratio[gcam_land_reg$SuitProtUnman2Man_ratio == Inf] = NA
		
		gcam_land$SuitProtUnman2Man_value_ratio[gcam_land$SuitProtUnman2Man_value_ratio == Inf] = NA
		gcam_land_reg$SuitProtUnman2Man_value_ratio[gcam_land_reg$SuitProtUnman2Man_value_ratio == Inf] = NA




		## # need the region id and the unit id for map plotting
		
		gcam_land_all = merge(gcam_land, region_ids, by="region", all.x = TRUE, sort = FALSE)
		gcam_land_all$unit_id = gcam_land_all$GCAM_region_ID * 1000 + gcam_land_all$GCAM_basin_ID
		gcam_land_all = gcam_land_all[order(gcam_land_all$year, gcam_land_all$region, gcam_land_all$GCAM_basin_ID, gcam_land_all$LT_GCAM),]
		
		gcam_land_reg_all = merge(gcam_land_reg, region_ids, by="region", all.x = TRUE, sort = FALSE)
		gcam_land_reg_all = gcam_land_reg_all[order(gcam_land_reg_all$year, gcam_land_reg_all$region, gcam_land_reg_all$LT_GCAM),]
		
		write.csv(gcam_land_all, file = paste0(outdir, "gcam_land_all_", out_names[s], "_", year, ".csv"), row.names = FALSE, na="")
		write.csv(gcam_land_reg_all, file = paste0(outdir, "gcam_land_reg_all_", out_names[s], "_", year, ".csv"), row.names = FALSE, na="")

		# need the land unit df
		land_units = unique(gcam_land_all$unit_id)

		##note that i don't find any relationships so far in a given year
		
		# so look at change over time within a scenario
	

		#### make some plots and stats for all land units
		# just for exploration
		if(FALSE){
		
		p = ggplot(gcam_land_all, aes(x= SuitProtUnman2Man_value_ratio, y= SuitProtUnman2Man_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_all, aes(x=SuitProtUnman2Man_value_ratio, y= SuitProtUnman2AvailUnman_ratio, color=unit_id)) +
			geom_point() +
			theme_bw()
			print(p)
		
		#### make some plots and stats by land unit
		
		p = ggplot(gcam_land_all[gcam_land_all$unit_id== land_units[1],], aes(x= SuitProtUnman2AvailUnman_value_ratio, y= SuitProtUnman2Man_value_ratio, color = SuitProtUnman2Man_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_all[gcam_land_all$unit_id== land_units[1],], aes(x= SuitProtUnman2AvailUnman_ratio, y= SuitProtUnman2Man_value_ratio, color = SuitProtUnman2AvailUnman_value_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_all[gcam_land_all$unit_id== land_units[1],], aes(x= SuitProtUnman2Man_ratio, y= SuitProtUnman2Man_value_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_all[gcam_land_all$unit_id== land_units[60],], aes(x= SuitProtUnman2Man_ratio, y= SuitProtUnman2AvailUnman_value_ratio, color = SuitProtUnman2Man_value_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_all[gcam_land_all$unit_id== land_units[60],], aes(x= SuitProtUnman2Man_value_ratio, y= SuitProtUnman2AvailUnman_value_ratio, color = SuitProtUnman2Man_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_all[gcam_land_all$unit_id== land_units[60],], aes(x= SuitProtUnman2AvailUnman_ratio, y= SuitProtUnman2AvailUnman_value_ratio, color = SuitProtUnman2Man_ratio)) +
			geom_point()
		print(p)
		
		# this one looks better overall
#		for(l in 1:length(land_units)){
#			t=gcam_land_all[gcam_land_all$unit_id==land_units[l],]
#			if(nrow(t[!is.na(t$SuitProtUnman2Man_value_ratio) & !is.na(t$SuitProtUnman2AvailUnman_ratio),]) > 0) {
#			   lmout=lm(SuitProtUnman2AvailUnman_value_ratio~ SuitProtUnman2Man_ratio,data=gcam_land_all[gcam_land_all$unit_id==land_units[l],], na.rm=TRUE)
#			   print(summary(lmout))
#			}
#		}
		
#		for(l in 1:length(land_units)){
#			t=gcam_land_all[gcam_land_all$unit_id==land_units[l],]
#			if(nrow(t[!is.na(t$SuitProtUnman2AvailUnman_value_ratio) & !is.na(t$SuitProtUnman2AvailUnman_ratio),]) > 0) {
#			   lmout=lm(SuitProtUnman2AvailUnman_value_ratio ~ SuitProtUnman2AvailUnman_ratio,data=gcam_land_all[gcam_land_all$unit_id==land_units[l],], na.rm=TRUE)
#			   print(summary(lmout))
#			}
#		}
		
		} # end if(FALSE) for exploratory plots
		
		
		# do the non-linear regression on all land unit values together
		# for the protected2unprotected value ratio vs the protected2managed area ratio
		
		########### but plot the difference normalized to the unmanaged value, which is just the ratio minus 1
		
		t=gcam_land_all[!is.na(gcam_land_all$prot_cost_norm)
			   & !is.na(gcam_land_all$SuitProtUnman2Man_ratio) & gcam_land_all$LT_GCAM=="AllUnmanaged" & gcam_land_all$year >= 2015,]
		nls_error = FALSE
        tr <- tryCatch(
           {fit = nls( prot_cost_norm  ~ SSasymp(SuitProtUnman2Man_ratio, yf, y0, log_alpha), data=t )},
		   error = function(e) {nls_error <<- TRUE})
		cat(" nls_error=", nls_error)
		if(!nls_error) {
		   t$y_exp = predict(fit, list(SuitProtUnman2Man_ratio = t$SuitProtUnman2Man_ratio))
		   pstitle=paste("nls")
		   coef=summary(fit)$coefficients
		   asym = coef[1] # yf
		   r0 = coef[2]   # y0
		   lrc = coef[3]  # log_alpha
		   # form: y=a+b*exp(c*x)
		   t$a = round(asym,2)
		   t$b = round(r0-asym,2)
		   t$c = round(-exp(lrc),2)
		   modtext = paste0("\n", "y=", t$a[1], "+", t$b[1], "*exp(", t$c[1], "*x)")
		}
		# check randomness of residuals
		t$residuals = t$prot_cost_norm - t$y_exp
		rt=runs.test(t$residuals)
		rand_pval = rt$p.value
		pstitle = paste(pstitle, "random pval =", round(rand_pval,5))
		
		pstitle = paste(pstitle,modtext)

		pdf(file=paste0(outdir, "prot_cost_2_area_all_", out_names[s], ".pdf"))

		p = ggplot(t, aes(x= SuitProtUnman2Man_ratio, y= prot_cost_norm, color = year)) +
			geom_point() +
			labs(title=paste("all land units"), subtitle=pstitle) +
			xlab("Suitable protected area to managed area ratio") +
			ylab("Relative protected land conversion pressure") +
			geom_line(aes(SuitProtUnman2Man_ratio, y_exp))
		print(p)
		
		dev.off()
  
        # probability that residuals are random (low prob, e.g. < 0.05, rejects null hypothesis of randomness)
        t$random_pval = rand_pval
        
        write.csv(t, file=paste0(outdir, "prot_cost_2_area_all_", out_names[s], ".csv"), row.names=FALSE)
		
        # build a land unit table to write output related to plots
        gcam_land_out = NULL

		# do some nonlinear regressions
		# SSasymp function for nls: y=yf+(y0-yf)*exp(-exp(log_alpha)*x) = a+b*exp(c*x)
		# linear-log(y) function for lm: ln(y)=i+m*x; or y = exp(i+m*x) = exp(i)*exp(m*x) = b*(c*x)
		
		pdf(file=paste0(outdir, "prot_cost_2_area_", out_names[s], ".pdf"))
		
		for(l in 1:length(land_units)){
			cat("\n",land_units[l])
			t=gcam_land_all[gcam_land_all$unit_id== land_units[l] & !is.na(gcam_land_all$prot_cost_norm)
			   & !is.na(gcam_land_all$SuitProtUnman2Man_ratio) & gcam_land_all$LT_GCAM=="AllUnmanaged" & gcam_land_all$year >= 2015,]
			   
			if(nrow(t[!is.na(t$prot_cost_norm) & !is.na(t$SuitProtUnman2Man_ratio),]) > 0) {
			   
               # first try the good nls fit
               nls_error = FALSE
               tr <- tryCatch(
                  {fit = nls( prot_cost_norm  ~ SSasymp(SuitProtUnman2Man_ratio, yf, y0, log_alpha), data=t )},
			      error = function(e) {nls_error <<- TRUE})
			   cat(" nls_error=", nls_error)
			   
			   py = t$prot_cost_norm
			   pylab = "Relative protected land conversion pressure"
			   
			   if(!nls_error) {
			      t$y_exp = predict(fit, list(SuitProtUnman2Man_ratio = t$SuitProtUnman2Man_ratio))
			      pstitle=paste("nls")
			      coef=summary(fit)$coefficients
			      asym = coef[1] # yf
			      r0 = coef[2]   # y0
			      lrc = coef[3]  # log_alpha
			      # form: y=a+b*exp(c*x)
			      t$a = round(asym,2)
			      t$b = round(r0-asym,2)
			      t$c = round(-exp(lrc),2)
			      t$fit = "nls"
			      modtext = paste0("\n", "y=", t$a[1], "+", t$b[1], "*exp(", t$c[1], "*x)")

			   } else {     # try a simpler fit
			   	  # the log of a negative is undefined and the log of zero is inf
			   	  # so if there are values <= 0 do the fit on the absolute ratio
			   	  if ( length(which(t$prot_cost_norm <= 0)) > 0) {
			   	  	lmout=lm(log(prot_cost_norm+1) ~ SuitProtUnman2Man_ratio,data = t, na.rm=TRUE)
			   	  	py = t$prot_cost_norm + 1
			   	  	pylab = "Protected land conversion pressure"
			   	  } else {
			      	lmout=lm(log(prot_cost_norm) ~ SuitProtUnman2Man_ratio,data = t, na.rm=TRUE)
			      }
			      slm = summary(lmout)
                  r2 = slm$r.squared
                  r2adj = slm$adj.r.squared
                  pval = slm$coefficients[8]
                  min_x = min(t$SuitProtUnman2Man_ratio, na.rm=TRUE)
                  max_x = max(t$SuitProtUnman2Man_ratio, na.rm=TRUE)
                  coef=slm$coefficients
                  lint = coef[1]
                  lslp = coef[2]
                  # form: y=b*exp(c*x)
                  t$a = 0.00
                  t$b = round(exp(lint),2)
			      t$c = round(lslp,2)
			      if(pylab == "Protected land conversion pressure"){
			      	# had to shift the data due to negative values
			      	t$fit = "shifted linear-log"
			      } else {
			      	t$fit = "linear-log"
			      }
               
                  t$y_exp = exp(predict(lmout, list(SuitProtUnman2Man_ratio = t$SuitProtUnman2Man_ratio)))
                  pstitle=paste("linear-log(y) r2 =", round(r2,5), "pval =", round(pval,5))
                  modtext = paste0("\n", "y=", t$b[1], "*exp(",t$c[1],"*x)")
			   }
			   
			   # check randomness of residuals
			   t$residuals = t$prot_cost_norm - t$y_exp
			   rt=runs.test(t$residuals)
			   rand_pval = rt$p.value
			   pstitle = paste(pstitle, "random pval =", round(rand_pval,5))
			   
			   # check relationships with area
			   other = paste0("\n2015 AU=", round(t$AvailableUnmanaged_area_ha[t$year==2015],0), " SPU=", round(t$SuitableProtectedUnmanaged_area_ha[t$year==2015],0),
			      " UU=", round(t$UnsuitableUnmanaged_area_ha[t$year==2015],0), " AM=", round(t$AllManaged_area_ha[t$year==2015],0), "\n",
			      "min spa2au = ", min(t$SuitProtUnman2AvailUnman_ratio), " max spa2au = ", max(t$SuitProtUnman2AvailUnman_ratio), "\n",
			      "min au = ", min(t$AvailableUnmanaged_area_ha), " max au = ", max(t$AvailableUnmanaged_area_ha))
			   
			   cat(other,"\n")
			   
			   pstitle = paste(pstitle,modtext)
			   
			   p = ggplot(t, aes(x= SuitProtUnman2Man_ratio, y= py, color = year)) +
					geom_point() +
					labs(title=paste(land_units[l], "Protected land conversion pressure"), subtitle=pstitle) +
					xlab("Suitable protected area to managed area ratio") +
					ylab(pylab) +
					geom_line(aes(SuitProtUnman2Man_ratio, y_exp))
			   print(p)
  
               # probability that residuals are random (low prob, e.g. < 0.05, rejects null hypothesis of randomness)
               t$random_pval = rand_pval

  			   gcam_land_out = rbind(gcam_land_out,t)
  
			} # end if non-zero rows
		} # end for l loop
		
    	dev.off()
		
		# need to select the corresponding y values for the fit method
		gcam_land_out$plot_y = gcam_land_out$prot_cost_norm
		gcam_land_out$plot_y[gcam_land_out$fit == "linear-log"] = gcam_land_out$SuitProtUnman2AvailUnman_value_ratio[gcam_land_out$fit == "linear-log"]
		
		write.csv(gcam_land_out, file=paste0(outdir, "prot_cost_2_area_", out_names[s], ".csv"), row.names=FALSE)
		
		region_names = unique(gcam_land_reg_all$region)
				
		#### make some plots and stats by region
		# exploratory plots
		if(FALSE){
		
		p = ggplot(gcam_land_reg_all[gcam_land_reg_all$region== region_names[25],], aes(x= SuitProtUnman2Man_value_ratio, y= SuitProtUnman2AvailUnman_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_reg_all[gcam_land_reg_all$region== region_names[25],], aes(x= SuitProtUnman2Man_value_ratio, y= SuitProtUnman2AvailUnman_value_ratio)) +
			geom_point()
		print(p)
		
		p = ggplot(gcam_land_reg_all[gcam_land_reg_all$region== region_names[25],], aes(x= SuitProtUnman2Man_value_ratio, y= SuitProtUnman2Man_ratio)) +
			geom_point()
		print(p)
		
		} # end if(FALSE) for exploratory plots
		
		
		# build a land unit table to write output related to plots
        gcam_land_reg_out = NULL

		# do some nonlinear regressions
		# SSasymp function for nls: y=yf+(y0-yf)*exp(-exp(log_alpha)*x) = a+b*exp(c*x)
		# linear-log(y) function for lm: ln(y)=i+m*x; or y = exp(i+m*x) = exp(i)*exp(m*x) = b*(c*x)
		
		pdf(file=paste0(outdir, "prot_cost_2_area_reg_", out_names[s], ".pdf"))
		
		for(r in 1:length(region_names)){
			cat("\n",region_names[r])
			t=gcam_land_reg_all[gcam_land_reg_all$region==region_names[r] & !is.na(gcam_land_reg_all$prot_cost_norm)
			   & !is.na(gcam_land_reg_all$SuitProtUnman2Man_ratio) & gcam_land_reg_all$LT_GCAM=="AllUnmanaged" & gcam_land_reg_all$year >= 2015,]
			   
			if(nrow(t[!is.na(t$prot_cost_norm) & !is.na(t$SuitProtUnman2Man_ratio),]) > 0) {
			   
               # first try the good nls fit
               nls_error = FALSE
               tr <- tryCatch(
                  {fit = nls( prot_cost_norm  ~ SSasymp(SuitProtUnman2Man_ratio, yf, y0, log_alpha), data=t )},
			      error = function(e) {nls_error <<- TRUE})
			   cat(" nls_error=", nls_error)
			   
			   py = t$prot_cost_norm
			   pylab = "Relative protected land conversion pressure"
			   
			   if(!nls_error) {
			      t$y_exp = predict(fit, list(SuitProtUnman2Man_ratio = t$SuitProtUnman2Man_ratio))
			      pstitle=paste("nls")
			      coef=summary(fit)$coefficients
			      asym = coef[1] # yf
			      r0 = coef[2]   # y0
			      lrc = coef[3]  # log_alpha
			      # form: y=a+b*exp(c*x)
			      t$a = round(asym,2)
			      t$b = round(r0-asym,2)
			      t$c = round(-exp(lrc),2)
			      t$fit = "nls"
			      modtext = paste0("\n", "y=", t$a[1], "+", t$b[1], "*exp(", t$c[1], "*x)")
			   } else {     # try a simpler fit
			   	  # the log of a negative is undefined and the log of zero is inf
			   	  # so if there are values <= 0 do the fit on the absolute ratio
			   	  if ( length(which(t$prot_cost_norm <= 0)) > 0) {
			   	  	lmout=lm(log(prot_cost_norm+1) ~ SuitProtUnman2Man_ratio,data = t, na.rm=TRUE)
			   	  	py = t$prot_cost_norm + 1
			   	  	pylab = "Protected land conversion pressure"
			   	  } else {
			      	lmout=lm(log(prot_cost_norm) ~ SuitProtUnman2Man_ratio,data = t, na.rm=TRUE)
			      }
			      slm = summary(lmout)
                  r2 = slm$r.squared
                  r2adj = slm$adj.r.squared
                  pval = slm$coefficients[8]
                  min_x = min(t$SuitProtUnman2Man_ratio, na.rm=TRUE)
                  max_x = max(t$SuitProtUnman2Man_ratio, na.rm=TRUE)
                  coef=slm$coefficients
                  lint = coef[1]
                  lslp = coef[2]
                  # form: y=b*exp(c*x)
                  t$a = 0.00
                  t$b = round(exp(lint),2)
			      t$c = round(lslp,2)
			      if(pylab == "Protected land conversion pressure"){
			      	# had to shift the data due to negative values
			      	t$fit = "shifted linear-log"
			      } else {
			      	t$fit = "linear-log"
			      }
               
                  t$y_exp = exp(predict(lmout, list(SuitProtUnman2Man_ratio = t$SuitProtUnman2Man_ratio)))
                  pstitle=paste("linear-log(y) r2 =", round(r2,5), "pval =", round(pval,5))
                  modtext = paste0("\n", "y=", t$b[1], "*exp(",t$c[1],"*x)")
			   }
			   
			   # check randomness of residuals
			   t$residuals = t$prot_cost_norm - t$y_exp
			   rt=runs.test(t$residuals)
			   rand_pval = rt$p.value
			   pstitle = paste(pstitle, "random pval =", round(rand_pval,5))
			   
			   # check relationships with area
			   other = paste0("\n2015 AU=", round(t$AvailableUnmanaged_area_ha[t$year==2015],0), " SPU=", round(t$SuitableProtectedUnmanaged_area_ha[t$year==2015],0),
			      " UU=", round(t$UnsuitableUnmanaged_area_ha[t$year==2015],0), " AM=", round(t$AllManaged_area_ha[t$year==2015],0), "\n",
			      "min spa2au = ", min(t$SuitProtUnman2AvailUnman_ratio), " max spa2au = ", max(t$SuitProtUnman2AvailUnman_ratio), "\n",
			      "min au = ", min(t$AvailableUnmanaged_area_ha), " max au = ", max(t$AvailableUnmanaged_area_ha))
			   
			   cat(other,"\n")
			   
			   pstitle = paste(pstitle,modtext)
			   
			   p = ggplot(t, aes(x= SuitProtUnman2Man_ratio, y= py, color = year)) +
					geom_point() +
					labs(title=paste(region_names[r], "protected land conversion pressure"), subtitle=pstitle) +
					xlab("Suitable protected area to managed area ratio") +
					ylab(pylab) +
					geom_line(aes(SuitProtUnman2Man_ratio, y_exp))
			   print(p)
			   
			   # probability that residuals are random (low prob, e.g. < 0.05, rejects null hypothesis of randomness)
               t$random_pval = rand_pval

  			   gcam_land_reg_out = rbind(gcam_land_reg_out,t)
  
			} # end if non-zero rows
		} # end for r loop
		
    	dev.off()

		##### make panel plot on one page for the regions for supplemental figure
		
		# need to select the corresponding y values for the linear fit method if there were negative values 
		gcam_land_reg_out$plot_y = gcam_land_reg_out$prot_cost_norm
		gcam_land_reg_out$plot_y[gcam_land_reg_out$fit == "shifted linear-log"] = gcam_land_reg_out$SuitProtUnman2AvailUnman_value_ratio[gcam_land_reg_out$fit == "shifted linear-log"]
		
		# need to reduce x ticks
		mybreaks <- function(n,s){
			function(x){
				x=x*10000
				d = s * diff(range(x)) / (1+2*s)
				sq = seq(min(x) + d, max(x) - d, length = n) / 10000
				round(sq,3)
			}
		}
		
		p = ggplot(gcam_land_reg_out, aes(x= SuitProtUnman2Man_ratio, y= plot_y, color = year)) +
					facet_wrap(~region, scales = "free") +
					geom_point() +
					labs(title="change in protected land conversion pressure") +
					xlab("Suitable protected area to managed area ratio") +
					ylab("Relative protected land conversion pressure") +
					geom_line(aes(SuitProtUnman2Man_ratio, y_exp)) +
					theme(axis.text.x = element_text(size = 5)) +
					theme(strip.text = element_text(size = 5)) +
					scale_x_continuous(breaks=mybreaks(2,0.1))
		print(p)
		
		ggsave(paste0(outdir, "prot_cost_2_area_reg_", out_names[s], "_facet", ".pdf"), plot=p, device="pdf")
		
		
        write.csv(gcam_land_reg_out, file=paste0(outdir, "prot_cost_2_area_reg_", out_names[s], ".csv"), row.names=FALSE)


		#### write specified year plots to pdf files
		
		#### try doing this using ggplot to be consistent with other plots
		#### do this for gcam land availability and the convertible land value
		#### need the reg-glu shapefile; don't draw lines for the data, then add the region boundaries
		
		#Scheme for ggplot figures
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
		
		gcam_land_all_year = gcam_land_all[gcam_land_all$year==year,]
		
		# output some value percentile stats
		# this will help set plot scale limits
		cat(out_names[s], "year=", year, " available unmanaged land value percentiles are:\nquantiles: 0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95\nvalues  ($/ha):",
			quantile(gcam_land_all_year$AvailableUnmanaged_value_usd_per_ha, na.rm=TRUE, probs=c(0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)), "\n")
		cat(out_names[s], "year=", year, "suitable protected land value percentiles are:\nquantiles: 0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95\nvalues  ($/ha):",
			quantile(gcam_land_all_year$SuitableProtectedUnmanaged_value_usd_per_ha, na.rm=TRUE, probs=c(0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)))
		
		pal <- colorRampPalette(brewer.pal(11,"Spectral"))

		# gcam region boundaries
		# readOGR is from deprecated rgdal
		#gcam_region_shapes = readOGR(shape_dir, gcam_region_shape_lname)
		#country_shapes = readOGR(shape_dir, country_shape_lname)
		gcam_region_shapes = as_Spatial(st_read(dsn=shape_dir, layer=gcam_region_shape_lname, stringsAsFactors=FALSE))
		country_shapes = as_Spatial(st_read(dsn=shape_dir, layer= country_shape_lname, stringsAsFactors=FALSE))
		
		reg_glu_sf = st_read(dsn=shape_dir, layer=gcam_reg_glu_shape_lname, stringsAsFactors=FALSE)
		region_sf = st_read(dsn=shape_dir, layer=gcam_region_shape_lname, stringsAsFactors=FALSE)
		
		# need to select one land type at a time:
		plot_df = gcam_land_all_year[gcam_land_all_year$LT_GCAM=="AllUnmanaged",c("region", "gcam_glu_abbr", "unit_id", "lt_gcam_area_ha", "value_usd_per_ha",
			"AvailableUnmanaged_area_ha", "AvailableUnmanaged_value_usd_per_ha", "SuitableProtectedUnmanaged_area_ha", "SuitableProtectedUnmanaged_value_usd_per_ha",
			"UnsuitableUnmanaged_area_ha", "UnsuitableUnmanaged_value_usd_per_ha", "SuitProtUnman2AvailUnman_ratio", "SuitProtUnman2AvailUnman_value_ratio",
			"SuitProtUnman2AvailUnman_value_ratio", "prot_cost_norm", "percent_conv_avail")]
		plot_df = unique(plot_df)
		# maybe want blank values if no data?
		#plot_df[is.na(plot_df)] = 0
		plot_df = plot_df[order(plot_df$region, plot_df$gcam_glu_abbr),]
		
		gcam_reg_glu_rast = raster(paste0(rast_dir,gcam_reg_glu_rast_fname))
		gcam_reg_glu_rast_rat = ratify(gcam_reg_glu_rast)
		rat = levels(gcam_reg_glu_rast_rat)[[1]]
		
		rat = merge(rat, plot_df, by.x = c("ID"), by.y = c("unit_id"), all.x = TRUE, sort = FALSE)
		# this ensures that the remaining units with non-convertible land have zero values rather than being blank
		# maybe want blank values if no data?
		#rat[is.na(rat)] = 0
		levels(gcam_reg_glu_rast_rat) <- rat
		
		avail_pcnt_conv_rast=deratify(gcam_reg_glu_rast_rat, att="percent_conv_avail")
    	outim <- levelplot(avail_pcnt_conv_rast, main="Available % convertible", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101))
   	 	#outim <- outim + latticeExtra::layer(sp.lines(country_shapes, lwd=0.4, col = "white")) + layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	outim <- outim + latticeExtra::layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	print(outim)
    	pdf(file=paste0(outdir, "avail_prcnt_conv_", out_names[s], "_", year, ".pdf"))
    	print(outim)
    	dev.off()
    	
    	# paper figure gcam available
    	plot2_df = left_join( reg_glu_sf, plot_df, join_by(key == unit_id) )
    	g <- ggplot()+
    		geom_sf(data = plot2_df, aes(fill = percent_conv_avail), colour = NA)+
    		geom_sf(data = region_sf, fill = NA, colour = "black")+
     		scale_fill_viridis(limits=c(0,100))+
     		labs(fill="Percent") +
     		scheme_basic
		plot(g)
		ggsave(paste0(outdir,"avail_prcnt_conv_", out_names[s],"_", year, "_paper.pdf"), plot=g, device="pdf")
    	
    	suitprot2avail_rast=deratify(gcam_reg_glu_rast_rat, att="SuitProtUnman2AvailUnman_ratio")
    	outim <- levelplot(suitprot2avail_rast, main="Suitable protected to Available ratio", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101)/50)
    	#outim <- outim + latticeExtra::layer(sp.lines(country_shapes, lwd=0.4, col = "white")) + layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	outim <- outim + latticeExtra::layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	print(outim)
    	pdf(file=paste0(outdir, "suitprot2avail_", out_names[s], "_", year, ".pdf"))
    	print(outim)
    	dev.off()
    	
    	# 95% of the current values are less than 380 $/ha for reference scenario
    	availum_landvalue_rast=deratify(gcam_reg_glu_rast_rat, att="AvailableUnmanaged_value_usd_per_ha")
    	outim <- levelplot(availum_landvalue_rast, main="Available unmanaged Land value in usd per ha", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101)*10)
    	#outim <- outim + latticeExtra::layer(sp.lines(country_shapes, lwd=0.4, col = "white")) + layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	outim <- outim + latticeExtra::layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	print(outim)
    	pdf(file=paste0(outdir, "availum_landvalue_", out_names[s], "_", year, ".pdf"))
    	print(outim)
    	dev.off()
    	
    	# 95% of the current values are less than 380 $/ha for reference scenario
    	suitprot_landvalue_rast=deratify(gcam_reg_glu_rast_rat, att="SuitableProtectedUnmanaged_value_usd_per_ha")
    	outim <- levelplot(suitprot_landvalue_rast, main="Suitable protected Land value in usd per ha", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101)*10)
    	#outim <- outim + latticeExtra::layer(sp.lines(country_shapes, lwd=0.4, col = "white")) + layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	outim <- outim + latticeExtra::layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	print(outim)
    	pdf(file=paste0(outdir, "suitprot_landvalue_", out_names[s], "_", year, ".pdf"))
    	print(outim)
    	dev.off()
    	
    	# paper figure suitable protected land value
    	plot2_df = left_join( reg_glu_sf, plot_df, join_by(key == unit_id) )
    	g <- ggplot()+
    		geom_sf(data = plot2_df, aes(fill = SuitableProtectedUnmanaged_value_usd_per_ha), colour = NA)+
    		geom_sf(data = region_sf, fill = NA, colour = "black")+
     		scale_fill_viridis(limits=c(0,200), na.value="yellow")+
     		labs(fill="$/ha") +
     		scheme_basic
		plot(g)
		ggsave(paste0(outdir,"suitprot_landvalue_", out_names[s],"_", year, "_paper.pdf"), plot=g, device="pdf")
    	
    	suitprot2avail_value_rast=deratify(gcam_reg_glu_rast_rat, att="SuitProtUnman2AvailUnman_value_ratio")
    	outim <- levelplot(suitprot2avail_value_rast, main="Suitable protected to Available value ratio", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101)/50)
    	#outim <- outim + latticeExtra::layer(sp.lines(country_shapes, lwd=0.4, col = "white")) + layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	outim <- outim + latticeExtra::layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    	print(outim)
    	pdf(file=paste0(outdir, "suitprot2avail_value_", out_names[s], "_", year, ".pdf"))
    	print(outim)
    	dev.off()
		
	
	} # end for s loop over scenario project files
	
	cat("finish proc_gcam_land_distribution_biodiv_temporal at", date(), "\n")
}