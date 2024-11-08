# make various plots of GCAM output land allocation

library(rgcam)library(dplyr)library(tidyr)library(ggplot2)library(viridis)

setwd("./")
#proj_names = c("outputs/project_files/tables_gcam_defaultNZ.proj", "outputs/project_files/tables_30percnz.proj",
#				"outputs/project_files/tables_allansce1nz.proj", "outputs/project_files/tables_allansce2nz.proj")
#out_names = c("current_nz", "uniform30_nz", "biodiv_nz", "biodiv30_nz")
#
#outdir = "outputs/land_allocation/"


proj_names = c("outputs/project_files/tables_gcam_default.proj", "outputs/project_files/tables_30perc.proj",
				"outputs/project_files/tables_allansce1.proj", "outputs/project_files/tables_allansce2.proj")
out_names = c("current", "uniform30", "biodiv", "biodiv30")

outdir = "outputs/land_allocation_ref/"

# Additional functionshdr_path = "helper_scripts/"# Load all support functions into memorysource( paste0(hdr_path,"diag_header.R"))	# configuration and helper functionssource( paste0(hdr_path,"diag_grapher.R"))  # functions to generate figuressource( paste0(hdr_path,"color_schemes.R" )) # some predefined color schemessource( paste0(hdr_path,"diag_util_functions.R")) # library of useful utility functions


current_proj <- loadProject(proj_names[1])uniform30_proj <- loadProject(proj_names[2])
biodiv_proj <- loadProject(proj_names[3])
biodiv30_proj <- loadProject(proj_names[4])

#Add constants for schemescheme_basic <- theme_bw() +  theme(legend.text = element_text(size = 15)) +  theme(legend.title = element_text(size = 15)) +  theme(axis.text = element_text(size = 18)) +  theme(axis.title = element_text(size = 18, face = "bold")) +  theme(plot.title = element_text(size = 15, face = "bold", vjust = 1)) +  theme(plot.subtitle = element_text(size = 9, face = "bold", vjust = 1))+   theme(strip.text = element_text(size = 6))+  theme(strip.text.x = element_text(size = 12, face = "bold"))+  theme(strip.text.y = element_text(size = 15, face = "bold"))+  theme(legend.position = "right")+  theme(legend.text = element_text(size = 12))+  theme(legend.title = element_text(size = 12,color = "black",face="bold"))+  theme(axis.text.x= element_text(angle = 90,hjust=1))+  theme(legend.background = element_blank(), legend.box.background = element_rect(colour = "black"))theme0 <- theme(  panel.border = element_rect(colour = "black", linewidth=1),  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),  axis.text.x = element_text(angle = 90, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),  strip.text = element_text(size = 16),  plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),  legend.position="right", legend.justification = "center",  legend.title = element_blank(),  legend.key.size = unit(3, "cm"),  legend.key.height=unit(1.5,"line"),  legend.spacing.x = unit(1, 'cm'),   legend.background = element_blank(),
  legend.box.background = element_rect(colour = "black"),)    

if(substr(outdir,nchar(outdir), nchar(outdir)) != "/") { outdir = paste0(outdir, "/") }
dir.create(outdir, recursive=TRUE)#SSP_spatial <- "Global"#spatial <- "Global"

## Figure 1: Land allocation
getQuery(current_proj, "Aggregated Land Allocation") %>%        mutate(Availability = paste0("CURRENT"), policy ="NZ")->current_tablegetQuery(uniform30_proj, "Aggregated Land Allocation") %>%        mutate(Availability = paste0("UNIFORM30"), policy ="NZ")->uniform30_table

getQuery(biodiv_proj, "Aggregated Land Allocation") %>% 
       mutate(Availability = paste0("BIODIV"), policy ="NZ")->biodiv_table
       
getQuery(biodiv30_proj, "Aggregated Land Allocation") %>% 
       mutate(Availability = paste0("BIODIV30"), policy ="NZ")->biodiv30_table
new_table <- bind_rows(current_table, uniform30_table, biodiv_table, biodiv30_table)


#Land allocation

# note that the commented GCAM crops have been aggregated into "crops" ?????
landuse.d <- new_table  landuse.d$agg.land <- landuse.d[["land-allocation"]]landuse.d[landuse.d[,"land-allocation"] == "grass", "agg.land"] <- "e) Grassland"landuse.d[landuse.d[,"land-allocation"] == "pasture (other)", "agg.land"] <- "e) Grassland"
landuse.d[landuse.d[,"land-allocation"] == "UnsuitableGrassland", "agg.land"] <- "e) Grassland"
landuse.d[landuse.d[,"land-allocation"] == "UnsuitableUnmanagedPasture", "agg.land"] <- "e) Grassland"landuse.d[landuse.d[,"land-allocation"] == "otherarable", "agg.land"] <- "b) Crops"landuse.d[landuse.d[,"land-allocation"] == "crops", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "CornC4", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "FiberCrop", "agg.land"] <- "b) Crops"
#landuse.d[landuse.d[,"land-allocation"] == "FodderGrass", "agg.land"] <- "b) Crops"
#landuse.d[landuse.d[,"land-allocation"] == "FodderHerb", "agg.land"] <- "b) Crops"

landuse.d[landuse.d[,"land-allocation"] == "FodderHerbC4", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "Fruits", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "FruitsTree", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "Legumes", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "MiscCrop", "agg.land"] <- "b) Crops"

landuse.d[landuse.d[,"land-allocation"] == "MiscCropTree", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "NutsSeeds", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "NutsSeedsTree", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "OilCrop", "agg.land"] <- "b) Crops"

landuse.d[landuse.d[,"land-allocation"] == "OilCropTree", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "OilPalmTree", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "OtherGrain", "agg.land"] <- "b) Crops"

landuse.d[landuse.d[,"land-allocation"] == "OtherGrainC4", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "Rice", "agg.land"] <- "b) Crops"

landuse.d[landuse.d[,"land-allocation"] == "RootTuber", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "Soybean", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "SugarCrop", "agg.land"] <- "b) Crops"

landuse.d[landuse.d[,"land-allocation"] == "SugarCropC4", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "Vegetables", "agg.land"] <- "b) Crops"

#landuse.d[landuse.d[,"land-allocation"] == "Wheat", "agg.land"] <- "b) Crops"
landuse.d[landuse.d[,"land-allocation"] == "biomassGrass", "agg.land"] <- "a) Bioenergy crops"landuse.d[landuse.d[,"land-allocation"] == "biomassTree", "agg.land"] <- "a) Bioenergy crops"landuse.d[landuse.d[,"land-allocation"] == "tundra", "agg.land"] <- "desert"landuse.d[landuse.d[,"land-allocation"] == "rock and desert", "agg.land"] <- "desert"landuse.d[landuse.d[,"land-allocation"] == "forest (managed)", "agg.land"] <- "c) Forests (Harvested)"landuse.d[landuse.d[,"land-allocation"] == "forest (unmanaged)", "agg.land"] <- "d) Forests (Unmanaged)"
landuse.d[landuse.d[,"land-allocation"] == "UnsuitableUnmanagedForest", "agg.land"] <- "d) Forests (Unmanaged)"landuse.d[landuse.d[,"land-allocation"] == "pasture (grazed)", "agg.land"] <- "f) Grazed Pasture"landuse.d[landuse.d[,"land-allocation"] == "shrubs", "agg.land"] <- "g) Shrubland"
landuse.d[landuse.d[,"land-allocation"] == "UnsuitableShrubland", "agg.land"] <- "g) Shrubland"stopifnot(sum(is.na(landuse.d$agg.land)) == 0)

landuse.agg = aggregate(landuse.d$value, by=list(landuse.d$Availability, landuse.d$region, landuse.d$year, landuse.d$policy, landuse.d$agg.land), FUN = sum)
names(landuse.agg) <- c("Availability", "region", "year", "policy", "agg.land", "value")landuse.agg <- add_global_sum(landuse.agg)landplot<-landuse.agg %>% filter(!agg.land %in% c("desert","urban"))landplot[TITLE_FIELD_NAME] <- "Land Use by Type"landplot$Availability <- factor(landplot$Availability, levels = c("CURRENT", "UNIFORM30", "BIODIV","BIODIV30"))
for(i in c(unique(landplot$region))){

    # plot the land allocation time series
	p <- ggplot(landplot %>% filter(region==i, year>=2015)) + geom_line(data=landplot%>% filter(region==i, year>=2015),aes(x=year, y=value, color=Availability), linewidth=1.2) +
		facet_wrap(~agg.land,scales = "free") +
		ylab("thousand km2") +
		ggtitle(paste0(toString(i), " Land allocation by type under different protection scenarios")) +
		scale_color_viridis(discrete = TRUE, option = "D") +
		labs(color="Scenario")
	
	plot(p)
	p+scheme_basic+ theme(plot.subtitle=element_text(size=12))

	ggsave( paste0(outdir ,'Land_allocation_', toString(i), '.pdf'),width = 10.5, height = 10)

    # calculate percent changes from 2015 (2025 CURRENT for bioenergy crops as the first non-zero year)
    for(a in unique(landplot$Availability)){
    	for(l in unique(landplot$agg.land)){
    		if(l=="a) Bioenergy crops"){
    			landplot$percent_change[landplot$region==i & landplot$agg.land==l & landplot$Availability==a] = 100 *
					(landplot$value[landplot$region==i & landplot$agg.land==l & landplot$Availability==a] -
					landplot$value[landplot$region==i & landplot$agg.land==l & landplot$Availability=="CURRENT" & landplot$year==2025]) /
					landplot$value[landplot$region==i & landplot$agg.land==l & landplot$Availability=="CURRENT" & landplot$year==2025]
				#landplot$percent_change[landplot$region==i & landplot$agg.land==l & landplot$Availability==a & landplot$year<2025] = 0
    		} else {
				landplot$percent_change[landplot$region==i & landplot$agg.land==l & landplot$Availability==a] = 100 *
					(landplot$value[landplot$region==i & landplot$agg.land==l & landplot$Availability==a] -
					landplot$value[landplot$region==i & landplot$agg.land==l & landplot$Availability==a & landplot$year==2015]) /
					landplot$value[landplot$region==i & landplot$agg.land==l & landplot$Availability==a & landplot$year==2015]
			} # end else non bioenergy crops
		} # end for l loop over land type
	} # end for a loop over availability

    # plot the percent change time series
	p <- ggplot(landplot %>% filter(region==i, year>=2015)) + geom_line(data=landplot%>% filter(region==i, year>=2015),aes(x=year, y= percent_change, color=Availability), linewidth=1.2) +
		facet_wrap(~agg.land,scales = "free") +
		ylab("percent change from 2015 (2025 CURRENT for bioenergy crops)") +
		ggtitle(paste0(toString(i), " Land allocation by type under different protection scenarios")) +
		scale_color_viridis(discrete = TRUE, option = "D") +
		labs(color="Scenario")
	
	plot(p)
	p+scheme_basic+ theme(plot.subtitle=element_text(size=12))	ggsave( paste0(outdir ,'percent_change_', toString(i), '.pdf'),width = 10.5, height = 10)

    # calculate the differences in allocation in future scenarios compared to current
    # but do this relative to the change in current values for a given year relative to the initial year (2025 current for biomass)
    # also do just the difference in percent change over time from current
    
    current = landplot[landplot$region==i & landplot$Availability=="CURRENT", c("region", "year", "policy", "agg.land", "Availability", "value", "percent_change")]
    uniform30 = landplot[landplot$region==i & landplot$Availability=="UNIFORM30", c("region", "year", "policy", "agg.land", "Availability", "value", "percent_change")]
    biodiv = landplot[landplot$region==i & landplot$Availability=="BIODIV", c("region", "year", "policy", "agg.land", "Availability", "value", "percent_change")]
    biodiv30 = landplot[landplot$region==i & landplot$Availability=="BIODIV30", c("region", "year", "policy", "agg.land", "Availability", "value", "percent_change")]
    
    temp = merge(current, uniform30, by=c("region", "year", "policy", "agg.land"))
    temp$Availability.x = NULL
    temp$Availability.y = NULL
    names(temp)[which(names(temp)=="value.x")] = "value.current"
    names(temp)[which(names(temp)=="value.y")] = "value.uniform30"
    names(temp)[which(names(temp)=="percent_change.x")] = "percent_change.current"
    names(temp)[which(names(temp)=="percent_change.y")] = "percent_change.uniform30"

    temp = merge(temp, biodiv, by=c("region", "year", "policy", "agg.land"))
    temp$Availability = NULL
    names(temp)[which(names(temp)=="value")] = "value.biodiv"
    names(temp)[which(names(temp)=="percent_change")] = "percent_change.biodiv"
    
    temp = merge(temp, biodiv30, by=c("region", "year", "policy", "agg.land"))
    temp$Availability = NULL
    names(temp)[which(names(temp)=="value")] = "value.biodiv30"
    names(temp)[which(names(temp)=="percent_change")] = "percent_change.biodiv30"
    
    # difference of case from current as percent of current change from initial
    # calculate 100*((case - current) / (current - current0))
    temp$diff_percent_diff_from_currchange.uniform30 = 100 * ( ( temp$value.uniform30 - temp$value.current )  /
    											     ( temp$value.current * (1 - 1/(temp$percent_change.current/100 + 1)) ) )
    temp$diff_percent_diff_from_currchange.biodiv = 100 * ( ( temp$value.biodiv  - temp$value.current )  /
    											  ( temp$value.current * (1 - 1/(temp$percent_change.current/100 + 1)) ) )
    temp$diff_percent_diff_from_currchange.biodiv30 = 100 * ( ( temp$value.biodiv30  - temp$value.current )  /
    											  ( temp$value.current * (1 - 1/(temp$percent_change.current/100 + 1)) ) )
    
    # difference of case percent change from current percent change
    temp$diff_percent_change.uniform30 = ( temp$percent_change.uniform30 - temp$percent_change.current )
    temp$diff_percent_change.biodiv = ( temp$percent_change.biodiv - temp$percent_change.current )
    temp$diff_percent_change.biodiv30 = ( temp$percent_change.biodiv30 - temp$percent_change.current )
    
    
    # put back into landplot df
    for(l in unique(landplot$agg.land)){
    	for(y in unique(landplot$year)){
    		# diff as percent of current change from initial
            landplot$diff_percent_diff_from_currchange[landplot$region==i & landplot$year==y & landplot$agg.land==l & landplot$Availability == "UNIFORM30"] =
            	temp$diff_percent_diff_from_currchange.uniform30[temp$region==i & temp$year==y & temp$agg.land==l]
            landplot$diff_percent_diff_from_currchange[landplot$region==i & landplot$year==y & landplot$agg.land==l & landplot$Availability == "BIODIV"] =
            	temp$diff_percent_diff_from_currchange.biodiv[temp$region==i & temp$year==y & temp$agg.land==l]
            landplot$diff_percent_diff_from_currchange[landplot$region==i & landplot$year==y & landplot$agg.land==l & landplot$Availability == "BIODIV30"] =
            	temp$diff_percent_diff_from_currchange.biodiv30[temp$region==i & temp$year==y & temp$agg.land==l]
            	
            # diff of percent change
            landplot$diff_percent_change[landplot$region==i & landplot$year==y & landplot$agg.land==l & landplot$Availability == "UNIFORM30"] =
            	temp$diff_percent_change.uniform30[temp$region==i & temp$year==y & temp$agg.land==l]
            landplot$diff_percent_change[landplot$region==i & landplot$year==y & landplot$agg.land==l & landplot$Availability == "BIODIV"] =
            	temp$diff_percent_change.biodiv[temp$region==i & temp$year==y & temp$agg.land==l]
            landplot$diff_percent_change[landplot$region==i & landplot$year==y & landplot$agg.land==l & landplot$Availability == "BIODIV30"] =
            	temp$diff_percent_change.biodiv30[temp$region==i & temp$year==y & temp$agg.land==l]
        }
    }
    
    # plot the diff from current percent change time series
	p <- ggplot(landplot %>% filter(region==i, year>=2015)) +
		geom_line(data=landplot%>% filter(region==i, year>=2015), aes(x=year, y= diff_percent_change, color=Availability), linewidth=1.2) +
		facet_wrap(~agg.land, scales = "free") +
		ylab("Difference from CURRENT in percent change from 2015 (2025 CURRENT for bioenergy crops)") +
		ggtitle(paste0(toString(i), " Land allocation by type under different protection scenarios")) +
		scale_color_viridis(discrete = TRUE, option = "D") +
		labs(color="Scenario")
	
	plot(p)
    p+scheme_basic+ theme(plot.subtitle=element_text(size=12))

	ggsave( paste0(outdir ,'diff_percent_change_from_current_', toString(i), '.pdf'),width = 10.5, height = 10)
    
} # end for i loop over region


# make a box plot of regional land allocation percent change from initial
    
g <- ggplot(data= landplot %>% filter(year %in% c(2025,2050,2075,2100) ) , aes(x=factor(year),y=percent_change, color=Availability))+
geom_boxplot() +
scale_color_viridis(discrete = TRUE, option = "D") +
scale_shape_manual(values= c(0))+
scale_alpha_manual(values = c(0.1, 1)) +
facet_wrap(~agg.land, scales="free") +
xlab("Year") +
ylab("Percent change from 2015 (2025 CURRENT for bioenergy crops)") +
labs(color="Scenario") 

plot(g)
g+scheme_basic

ggsave( paste0(outdir ,'regional_percent_change_box_plot.pdf'),width = 10.5, height = 10)
	
# write these percent change data to a file - format it for easier access
# colour is the scenario, PANEL is the land type, group denotes the scenario and year
pc_df = data.frame(ggplot_build(g)[[1]])

# panel ids are the indices of panel_names
panel_names = c("a) Bioenergy crops", "b) Crops", "c) Forests (Harvested)", "d) Forests (Unmanaged)", "e) Grassland", "f) Grazed Pasture", "g) Shrubland")
# map colors to scenarios
color_names = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")
scenario_names = c("CURRENT", "UNIFORM30", "BIODIV", "BIODIV30")
# map groups to scenario and year
group_vals = c(1:16)
group_scen_names = c(rep(c("CURRENT", "UNIFORM30", "BIODIV", "BIODIV30"), 4))
group_years = c(rep(2025,4), rep(2050,4), rep(2075,4), rep(2100,4))

out_df = pc_df[,c("colour", "PANEL", "group", "ymin_final", "ymin", "lower", "middle", "upper", "ymax", "ymax_final")]
   
# ymin/max_final is the true min/max; ymin/max is the 1.5*IQR value; lower/upper is the 25%/75$% qurtile; middle is the median
names(out_df) <- c("Scenario", "Land_Type", "year", "min", "low_1.5*IQR", "Q25", "median", "Q75", "hi_1.5*IQR", "max")
out_df$Land_Type <- as.character(out_df$Land_Type)
for (i in 1:length(group_vals)){
  	out_df$Scenario[which(out_df$year == group_vals[i])] = group_scen_names[i]
   	out_df$year[which(out_df$year == group_vals[i])] = group_years[i]
}
for (i in 1:length(panel_names)){
   	out_df$Land_Type[which(out_df$Land_Type == i)] = panel_names[i]
}
out_df = out_df[order(out_df$Land_Type, out_df$year),]

# write the region box plot summary
write.csv(out_df,paste0(outdir ,'regional_percent_change_box_plot.csv'))
	



# make a box plot of regional land allocation diff percent change from current change
    
g <- ggplot(data= landplot %>% filter(year %in% c(2025,2050,2075,2100) ) , aes(x=factor(year),y= diff_percent_diff_from_currchange, color=Availability))+
geom_boxplot() +
scale_color_viridis(discrete = TRUE, option = "D") +
scale_shape_manual(values= c(0))+
scale_alpha_manual(values = c(0.1, 1)) +
facet_wrap(~agg.land, scales="free") +
xlab("Year") +
ylab("Difference as percent of CURRENT change") +
labs(color="Scenario") 

plot(g)
g+scheme_basic

ggsave( paste0(outdir ,'regional_diff_percent_change_of_currchange_box_plot.pdf'),width = 10.5, height = 10)

# write these diff change data to a file - format it for easier access
# colour is the scenario, PANEL is the land type, group denotes the scenario and year
dpcfc_df = data.frame(ggplot_build(g)[[1]])

# panel ids are the indices of panel_names
panel_names = c("a) Bioenergy crops", "b) Crops", "c) Forests (Harvested)", "d) Forests (Unmanaged)", "e) Grassland", "f) Grazed Pasture", "g) Shrubland")
# map colors to scenarios
color_names = c("#440154FF", "#21908CFF", "#FDE725FF")
scenario_names = c("UNIFORM30", "BIODIV", "BIODIV30")
# map groups to scenario and year
group_vals = c(2,3,4,6,7,8,10,11,12,14,15,16)
group_scen_names = c("UNIFORM30", "BIODIV", "BIODIV30", "UNIFORM30", "BIODIV", "BIODIV30", "UNIFORM30", "BIODIV", "BIODIV30", "UNIFORM30", "BIODIV", "BIODIV30")
group_years = c(2025, 2025, 2025, 2050, 2050, 2050, 2075, 2075, 2075, 2100, 2100, 2100)

out_df = dpcfc_df[,c("colour", "PANEL", "group", "ymin_final", "ymin", "lower", "middle", "upper", "ymax", "ymax_final")]
   
# ymin/max_final is the true min/max; ymin/max is the 1.5*IQR value; lower/upper is the 25%/75$% qurtile; middle is the median
names(out_df) <- c("Scenario", "Land_Type", "year", "min", "low_1.5*IQR", "Q25", "median", "Q75", "hi_1.5*IQR", "max")
out_df$Land_Type <- as.character(out_df$Land_Type)
for (i in 1:length(group_vals)){
  	out_df$Scenario[which(out_df$year == group_vals[i])] = group_scen_names[i]
   	out_df$year[which(out_df$year == group_vals[i])] = group_years[i]
}
for (i in 1:length(panel_names)){
   	out_df$Land_Type[which(out_df$Land_Type == i)] = panel_names[i]
}
out_df = out_df[order(out_df$Land_Type, out_df$year),]

# write the region box plot summary
write.csv(out_df,paste0(outdir ,'regional_diff_percent_change_of_currchange_box_plot.csv'))



# write the total land allocation tablewrite.csv(landplot,paste0(outdir ,'Land_allocation_all_regions.csv'))

