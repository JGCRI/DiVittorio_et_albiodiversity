############# Libraries and loading ----------------
library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggpattern)
library(gcamdata)
library(gridExtra)
library(cowplot)
library(scales)

setwd("C:/Users/west001/OneDrive - PNNL/Documents/GCAM Biodiversity/GCAM outputs")

home_dir <- getwd()

fig_dir <- paste0(home_dir, "/figure")

DAT_NAME <- "tables_30perc.proj"
prj1 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_30percnz.proj"
prj2 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_allansce1.proj"
prj3 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_allansce1nz.proj"
prj4 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_allansce2.proj"
prj5 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_allansce2nz.proj"
prj6 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_gcam_default.proj"
prj7 <- rgcam::loadProject(DAT_NAME)

DAT_NAME <- "tables_gcam_defaultNZ.proj"
prj8 <- rgcam::loadProject(DAT_NAME)

listScenarios(prj1)
listQueries(prj1)



names(prj1) <- str_replace(names(prj1), "Reference", "30perc")

for(scenario_num in 1:length(prj1)){
  for(query_num in 1:length(prj1[[scenario_num]])){
    prj1[[scenario_num]][[query_num]]$scenario <- names(prj1)[scenario_num]
  }
}

names(prj2) <- str_replace(names(prj2), "Reference", "30percnz")

for(scenario_num in 1:length(prj2)){
  for(query_num in 1:length(prj2[[scenario_num]])){
    prj2[[scenario_num]][[query_num]]$scenario <- names(prj2)[scenario_num]
  }
}

names(prj3) <- str_replace(names(prj3), "Reference", "allansce1")

for(scenario_num in 1:length(prj3)){
  for(query_num in 1:length(prj3[[scenario_num]])){
    prj3[[scenario_num]][[query_num]]$scenario <- names(prj3)[scenario_num]
  }
}

names(prj4) <- str_replace(names(prj4), "Reference", "allansce1nz")

for(scenario_num in 1:length(prj4)){
  for(query_num in 1:length(prj4[[scenario_num]])){
    prj4[[scenario_num]][[query_num]]$scenario <- names(prj4)[scenario_num]
  }
}

names(prj5) <- str_replace(names(prj5), "Reference", "allansce2")

for(scenario_num in 1:length(prj5)){
  for(query_num in 1:length(prj5[[scenario_num]])){
    prj5[[scenario_num]][[query_num]]$scenario <- names(prj5)[scenario_num]
  }
}

names(prj6) <- str_replace(names(prj6), "Reference", "allansce2nz")

for(scenario_num in 1:length(prj6)){
  for(query_num in 1:length(prj6[[scenario_num]])){
    prj6[[scenario_num]][[query_num]]$scenario <- names(prj6)[scenario_num]
  }
}

names(prj7) <- str_replace(names(prj7), "Reference", "default")

for(scenario_num in 1:length(prj7)){
  for(query_num in 1:length(prj7[[scenario_num]])){
    prj7[[scenario_num]][[query_num]]$scenario <- names(prj7)[scenario_num]
  }
}

names(prj8) <- str_replace(names(prj8), "Reference", "defaultnz")

for(scenario_num in 1:length(prj8)){
  for(query_num in 1:length(prj8[[scenario_num]])){
    prj8[[scenario_num]][[query_num]]$scenario <- names(prj8)[scenario_num]
  }
}


###### combine .dat files

saveProject(prj1,"30perc.proj" )
saveProject(prj2, "30percnz.proj")
saveProject(prj3, "allansce1.proj")
saveProject(prj4, "allansce1nz.proj")
saveProject(prj5, "allansce2.proj")
saveProject(prj6, "allansce2nz.proj")
saveProject(prj7, "default.proj")
saveProject(prj8, "defaultnz.proj")

projlist <- list("30perc.proj","30percnz.proj", "allansce1.proj", "allansce1nz.proj","allansce2.proj", "allansce2nz.proj", "default.proj", "defaultnz.proj")

gcam_biodiv_220 <- mergeProjects(gcam_biodiv_220, projlist, save = TRUE )

listScenarios(gcam_biodiv_220)
listQueries(gcam_biodiv_220)

saveProject(gcam_biodiv_220, "gcam_biodiv_220.proj")

DAT_NAME <- "gcam_biodiv_220.proj"
gcam_biodiv_220 <- rgcam::loadProject(DAT_NAME)

################################

# electricity  ------------
tech.list <- c( "biomass", "coal", "gas", "geothermal", "hydro", "nuclear", "refined liquids", "solar", "wind", "rooftop solar")

tech.color <- c( "biomass" = "darkgreen",
                 "solar" = "goldenrod1", "rooftop_pv" = "yellow",
                 "wind" = "skyblue",
                 "geothermal" = "olivedrab1",
                 "hydro" = "mediumpurple4",
                 "coal" = "darkred", 
                 "nuclear" = "darkorange1",
                 "gas" = "mediumorchid3", 
                 "refined liquids" = "hotpink")

fillScaleTech <- scale_fill_manual(name = "technology",
                                   values = tech.color,
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))

# land  ------------
land.list <- c( "biomassGrass", "crops", "forest (managed)", "forest (unmanaged)", "grass", "otherarable", "pasture (grazed)", "pasture (other)", "urban")

land.color <- c( "biomassGrass" = "goldenrod", "crops" = "orange",
                 "forest (managed)" = "green", "forest (unmanaged)" = "dark green",
                 "grass" = "olivedrab1",
                 "otherarable" = "yellow",
                 "pasture (grazed)" = "skyblue",
                 "pasture (other)" = "dodgerblue", 
                 "urban" = "gray")

fillScaleLand <- scale_fill_manual(name = "land",
                                   values = land.color,
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))

###### global electricity generation


biomass_elec_gen_global <- getQuery(gcam_biodiv_220, "elec gen by subsector") %>%
  filter(output == "electricity"| output == "elec_td_bld", subsector == "biomass", year > 2015, year <= 2100) %>%
  
  
  group_by(Units, scenario, subsector, output, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()  %>%

  ggplot() +
  geom_bar(aes(x = year, y = value, fill = subsector), stat = "identity", size = 1) +
  theme_bw() + fillScaleTech + 
  facet_wrap(~scenario) +
  theme(axis.text.x = element_text(angle=90)) +  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
  ylab("Total generation (EJ)") +
  xlab("") + 
  ##scenario_colors + 
  ggtitle("Global Biomass Electricity Generation")

ggsave(paste0(fig_dir, "/biomass_elec_gen_subsector_global.png"), width = 13, height = 6.5)

###### global electricity generation


elec_gen_global <- getQuery(gcam_biodiv_220, "elec gen by subsector") %>%
  filter(output == "electricity"| output == "elec_td_bld", year > 2015, year <= 2100) %>%
  
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = subsector), stat = "identity", size = 1) +
  theme_bw() + fillScaleTech + 
  facet_wrap(~scenario) +
  theme(axis.text.x = element_text(angle=90))  +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
  ylab("Generation (EJ)") +
  xlab("") + 
  ##scenario_colors + 
  ggtitle("Global Electricity Generation")

ggsave(paste0(fig_dir, "/elec_gen_subsector_global.png"), width = 13, height = 6.5)


###########################

#PLOTS TO ADD - all elec gen (bar), biomass production (regional, line).  detailed land allocation bar chart by region

######  CO2 prices by region and scenario  #####################################

co2_prices <- getQuery(gcam_biodiv_220, "CO2 prices")  %>% filter(year > 2015, year <= 2100) %>% ggplot() + geom_hline(yintercept = 0) +
  geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
  theme_bw() +
  ##scenario_colors +
  
  labs(y = "MtCO2e") +
  facet_wrap(~market, scale = "free_y") +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle(bquote(~CO2_Price)) +
  xlab("") + 
  ylab("$1990/tC")  
  

ggsave(paste0(fig_dir, "/CO2_price_rgn.png"), width = 17, height = 8)

######  biomass electricity generation by region and scenario  #####################################

biomass_elec_gen <- getQuery(gcam_biodiv_220, "elec gen by subsector") %>%
  filter(output == "electricity"| output == "elec_td_bld", subsector == "biomass", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
  
  ggplot() + geom_hline(yintercept = 0) +
  geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
  theme_bw() +
  ##scenario_colors +
  
 
  facet_wrap(~region, scale = "free_y") +
  
  scale_color_manual(values =  c( "30percnz" = "dark orange",
                                  "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
  
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle(bquote(~'Biomass Electricity')) +
  xlab("") + 
  ylab("Generation (EJ)")  

  plot_data <- getQuery(gcam_biodiv_220, "elec gen by subsector") %>%
    filter(output == "electricity"| output == "elec_td_bld", subsector == "biomass", year > 2015, year <= 2100)
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/biomass_elec_gen_subsector_nz_region.csv")
  write_csv(d, dfn)

  ofn <- ggsave(paste0(fig_dir, "/biomass_elec_gen_subsector_nz_region.png"), width = 17, height = 8)


  ######  food demand prices - staples by region and scenario  #####################################
  
  food_demand_staples_prices <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_Staples", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Food Demand Prices (Staples)')) +
    xlab("") + 
    ylab("$2005/Mcal/day")  
  
  plot_data <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_Staples", year > 2015, year <= 2100)
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/food_demand_prices_staples_nz_region.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/food_demand_prices_staples_nz_region.png"), width = 17, height = 8)

  ######  food demand prices - non-staples by region and scenario  #####################################
  
  food_demand_nonstaples_prices <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_NonStaples", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Food Demand Prices (Non-Staples)')) +
    xlab("") + 
    ylab("$2005/Mcal/day")  
  
  plot_data <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_NonStaples", year > 2015, year <= 2100)
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/food_demand_prices_nonstaples_nz_region.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/food_demand_prices_nonstaples_nz_region.png"), width = 17, height = 8)
  
  ######  biomass production by region   #####################################
  
  biomass_production <- getQuery(gcam_biodiv_220, "purpose-grown biomass production") %>%
    filter( year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                   "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Biomass Production')) +
    xlab("") + 
    ylab("EJ")  
  
  plot_data <- getQuery(gcam_biodiv_220, "purpose-grown biomass production") %>%
    filter( year > 2015, year <= 2100, str_detect(scenario, "nz"))
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/biomass_production_nz_region.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/biomass_production_nz_region.png"), width = 17, height = 8)
  
  ###### global land allocation
  
  
  land_global <- getQuery(gcam_biodiv_220, "Aggregated Land Allocation") %>% filter(year > 2015, year <= 2100)
  
  colnames(land_global) <- sub("land-allocation", "land", colnames(land_global))
  
  land_global <-  land_global %>%
    filter(land == "biomassGrass"| land == "crops"
           | land == "forest (managed)" | land == "forest (unmanaged)" | 
             land == "grass" | land == "otherarable"|
             land == "pasture (grazed)" | land == "pasture (other)" |
             land == "urban", year > 2015, year <= 2100) %>%
    
    ggplot() +
    geom_bar(aes(x = year, y = value, fill = land), stat = "identity", size = 1) +
    theme_bw() + fillScaleLand + 
    facet_wrap(~scenario) +
    theme(axis.text.x = element_text(angle=90))  +
    scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
    ylab("Thousand sq. km.") +
    xlab("") + 
    ##scenario_colors + 
    ggtitle("Global Land Allocation")
  
  plot_data <- getQuery(gcam_biodiv_220, "Aggregated Land Allocation") %>%
    filter( year > 2015, year <= 2100)
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/land_allocation_global.csv")
  write_csv(d, dfn)
  
  
  ggsave(paste0(fig_dir, "/land_allocation_global.png"), width = 13, height = 6.5)
  
  
  ###### land allocation by type and region
  
  
  land_global <- getQuery(gcam_biodiv_220, "Aggregated Land Allocation") %>% filter(year > 2015, year <= 2100, str_detect(scenario, "nz"))
  
  colnames(land_global) <- sub("land-allocation", "land", colnames(land_global))
  
  land_type <- land_global  %>%
    filter( land == "biomassGrass") %>%
  
  ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'biomassGrass')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/biomassGrass_nz_region.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "crops") %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
     theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'crops')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/crops_nz_region.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "forest (managed)") %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
     theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'forest (managed)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/forest_managed_nz_region.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "forest (unmanaged)") %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'forest (unmanaged)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/forest_unmanaged_nz_region.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "grass") %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'grass')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/grass_nz_region.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "pasture (grazed)") %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'pasture (grazed)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/pasture_grazed_nz_region.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "pasture (other)") %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'pasture (other)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/pasture_other_nz_region.png"), width = 13, height = 6.5)

  ######  electricity prices by region   #####################################
  
  elec_price <- getQuery(gcam_biodiv_220, "elec prices by sector") %>%
    filter(fuel == "electricity", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Electricity Price')) +
    xlab("") + 
    ylab("$1975/GJ")  
  
  plot_data <- getQuery(gcam_biodiv_220, "elec prices by sector") %>%
    filter(fuel == "electricity", year > 2015, year <= 2100, str_detect(scenario, "nz")) 
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/elect_price_nz_region.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/elect_price_nz_region.png"), width = 17, height = 8)
  
  ######  beef and dairy prices by region   #####################################
  
  beef_price <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Beef", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Beef Price')) +
    xlab("") + 
    ylab("$1975/kg")  
  
  plot_data <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Beef", year > 2015, year <= 2100, str_detect(scenario, "nz"))
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/beef_price_nz_region.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/beef_price_nz_region.png"), width = 17, height = 8)
  
  dairy_price <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Dairy", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    ggplot() + geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    facet_wrap(~region, scale = "free_y") +
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Dairy Price')) +
    xlab("") + 
    ylab("$1975/kg")  
  
  plot_data <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Dairy", year > 2015, year <= 2100, str_detect(scenario, "nz"))
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/dairy_price_nz_region.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/dairy_price_nz_region.png"), width = 17, height = 8)
  
  ######  global mean beef and dairy prices   #####################################
  
  
  ##### could weight by production instead
  
  global_beef_price <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Beef", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, sector, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()  %>%
    
    ggplot() + # geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
    ggtitle(bquote(~'Global Beef Price (Regional Mean)')) +
    xlab("") + 
    ylab("$1975/kg")  
  
  plot_data <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Beef", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, sector, year) %>%
    summarise(value = mean(value)) %>%
    ungroup() 
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/beef_price_nz_global.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/beef_price_nz_global.png"), width = 8, height = 6)
  
  global_dairy_price <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Dairy", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, sector, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()  %>%
    
    ggplot() + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
    ggtitle(bquote(~'Global Dairy Price (Regional Mean)')) +
    xlab("") + 
    ylab("$1975/kg")  
  
  plot_data <- getQuery(gcam_biodiv_220, "meat and dairy prices") %>%
    filter(sector == "Dairy", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, sector, year) %>%
    summarise(value = mean(value)) %>%
    ungroup() 
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/dairy_price_nz_global.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/dairy_price_nz_global.png"), width = 8, height = 6)
  
  ###### global sum land allocation by scenario
 
  
  
  land_global <- getQuery(gcam_biodiv_220, "Aggregated Land Allocation") %>% filter(year > 2015, year <= 2100, str_detect(scenario, "nz"))
  
  colnames(land_global) <- sub("land-allocation", "land", colnames(land_global))
  
  land_type_scenario <- land_global  %>%
    filter( land == "biomassGrass") %>% 
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot()  + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
   
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'biomassGrass')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/biomassGrass_nz_scenario.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "crops") %>%  
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot()  + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
  
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'crops')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/crops_nz_scenario.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "forest (managed)") %>%  
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot()  + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'forest (managed)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/forest_managed_nz_scenario.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "forest (unmanaged)") %>%
    
  
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot() + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'forest (unmanaged)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/forest_unmanaged_nz_scenario.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "grass") %>%
   
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot()  + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
   
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'grass')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/grass_nz_scenario.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "pasture (grazed)") %>%
   
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot()  + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
   
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'pasture (grazed)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/pasture_grazed_nz_scenario.png"), width = 13, height = 6.5)
  
  land_type <- land_global  %>%
    filter( land == "pasture (other)") %>%
    
    
    group_by(Units, scenario, land, year) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    
    ggplot() + #geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
   
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'pasture (other)')) +
    xlab("") + 
    ylab("Thousand sq. km.")  
  
  ggsave(paste0(fig_dir, "/pasture_other_nz_scenario.png"), width = 13, height = 6.5) 
 
  ######  food demand prices - global region average  #####################################
  
  ### could weight by production
  
 
  
  food_demand_staples_prices <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_Staples", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, input, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()  %>%
    
    ggplot() + # geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
    ggtitle(bquote(~'Global Food Demand Prices (Staples) (Region Mean)')) +
    xlab("") + 
    ylab("$2005/Mcal/day")  
  
  
  plot_data <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_Staples", year > 2015, year <= 2100) %>%
    
    group_by(Units, scenario, input, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/food_demand_prices_staples_nz_global.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/food_demand_prices_staples_nz_global.png"), width = 17, height = 8)
  
  ######################## non staples
  
  food_demand_staples_prices <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_NonStaples", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, input, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()  %>%
    
    ggplot() + # geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_x_continuous(breaks = seq(2020, 2100, by = 10), expand = c(0, 0)) +
    ggtitle(bquote(~'Global Food Demand Prices (Non-Staples) (Region Mean)')) +
    xlab("") + 
    ylab("$2005/Mcal/day")  
  
  
  plot_data <- getQuery(gcam_biodiv_220, "food demand prices") %>%
    filter(input == "FoodDemand_NonStaples", year > 2015, year <= 2100) %>%
    
    group_by(Units, scenario, input, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/food_demand_prices_nonstaples_nz_global.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/food_demand_prices_nonstaples_nz_global.png"), width = 17, height = 8)
  
  ######  global electricity prices region mean   #####################################
  
  elec_price <- getQuery(gcam_biodiv_220, "elec prices by sector") %>%
    filter(fuel == "electricity", year > 2015, year <= 2100, str_detect(scenario, "nz")) %>%
    
    group_by(Units, scenario, fuel, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()  %>%
    
    ggplot() + # geom_hline(yintercept = 0) +
    geom_line(aes(x = year, y = value,  color = scenario, linetype = scenario), size = 1) +
    theme_bw() +
    ##scenario_colors +
    
    
    
    
    scale_color_manual(values =  c( "30percnz" = "dark orange",
                                    "allansce1nz" = "forestgreen",  "allansce2nz" = "dodgerblue", "defaultnz" = "gray")) +
    
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(bquote(~'Global Electricity Price (Region Mean)')) +
    xlab("") + 
    ylab("$1975/GJ")  
  
  plot_data <- getQuery(gcam_biodiv_220, "elec prices by sector") %>%
    filter(fuel == "electricity", year > 2015, year <= 2100, str_detect(scenario, "nz"))  %>%
    
    group_by(Units, scenario, fuel, year) %>%
    summarise(value = mean(value)) %>%
    ungroup()  
  
  d <- pivot_wider(plot_data, names_from = year)
  d[is.na(d)] <- 0
  dfn <-  paste0(fig_dir, "/figure_data/elect_price_nz_global.csv")
  write_csv(d, dfn)
  
  ofn <- ggsave(paste0(fig_dir, "/elect_price_nz_global.png"), width = 17, height = 8)
  
  
 