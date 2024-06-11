# bio_paper_regional_protection.r

# Evaluate spatial patterns of protection and suitability
# Use the aggregated protected and suitable protected data for regXglu
# show difference in relation to uniform30
# all data are 2015
# all units are fraction of land

library(raster)
library(ggplot2)
library(dplyr)

setwd("./")

out_dir = "outputs/gcam_land_distribution/"

region_file = "other_data/GCAM_region_names_32reg.csv"
glu_file = "other_data/glu_to_gcamglu_mapping_c2021.csv"

data_dir = "pre-processing/"
u30_p_file = paste0(data_dir, "thirty_percent_plus_defprot_GCAM_plot_regXglu.csv")
bio30_p_file = paste0(data_dir, "Allan_sce2_plus_defprot_GCAM_plot_regXglu.csv")
bio_p_file = paste0(data_dir, "Allan_sce1_plus_defprot_GCAM_plot_regXglu.csv")
u30_sp_file = paste0(data_dir, "thirty_percent_plus_default_suit_prot_GCAM_plot_regXglu.csv")
bio30_sp_file = paste0(data_dir, "Allan_sce2_plus_default_suit_prot_GCAM_plot_regXglu.csv")
bio_sp_file = paste0(data_dir, "Allan_sce1_plus_default_suit_prot_GCAM_plot_regXglu.csv")

# name mapping files
region_names = read.csv(region_file, stringsAsFactors=FALSE, skip=3)
glu_names = read.csv(glu_file, stringsAsFactors=FALSE, skip=7)

# protected data
u30_p_in = read.csv(u30_p_file, stringsAsFactors=FALSE)
bio30_p_in = read.csv(bio30_p_file, stringsAsFactors=FALSE)
bio_p_in = read.csv(bio_p_file, stringsAsFactors=FALSE)
# suitable protected data
u30_sp_in = read.csv(u30_sp_file, stringsAsFactors=FALSE)
bio30_sp_in = read.csv(bio30_sp_file, stringsAsFactors=FALSE)
bio_sp_in = read.csv(bio_sp_file, stringsAsFactors=FALSE)


# uniform30
comp = u30_p_in
names(comp)[ncol(comp)] <- "u30_tot_prot"
comp = merge(comp, u30_sp_in, by=c("GCAM_region_ID", "GLU"))
names(comp)[ncol(comp)] <- "u30_suit_prot"
comp$u30_unsuit_prot = comp$u30_tot_prot - comp$u30_suit_prot

# biodiv
comp = merge(comp, bio_p_in, by=c("GCAM_region_ID", "GLU"))
names(comp)[ncol(comp)] <- "b_tot_prot"
comp = merge(comp, bio_sp_in, by=c("GCAM_region_ID", "GLU"))
names(comp)[ncol(comp)] <- "b_suit_prot"
comp$b_unsuit_prot = comp$b_tot_prot - comp$b_suit_prot

# biodiv 30
comp = merge(comp, bio30_p_in, by=c("GCAM_region_ID", "GLU"))
names(comp)[ncol(comp)] <- "b30_tot_prot"
comp = merge(comp, bio30_sp_in, by=c("GCAM_region_ID", "GLU"))
names(comp)[ncol(comp)] <- "b30_suit_prot"
comp$b30_unsuit_prot = comp$b30_tot_prot - comp$b30_suit_prot


# add the region and glu names
comp = merge(comp, region_names)
comp = merge(comp, glu_names[,c("GCAM_basin_ID", "Basin_name")], by.x="GLU", by.y="GCAM_basin_ID", all.x=TRUE)

# biodiv scenario minus uniform 30
comp$b_u30_sp_diff = comp$b_suit_prot - comp$u30_suit_prot
comp$b_u30_usp_diff = comp$b_unsuit_prot - comp$u30_unsuit_prot
cat("min max of biodiv - uniform30 suit prot ", min(comp$b_u30_sp_diff), max(comp$b_u30_sp_diff))
cat("min max of biodiv - uniform30 unsuit prot ", min(comp$b_u30_usp_diff), max(comp$b_u30_usp_diff))

# biodiv 30 scenario minus uniform 30
comp$b30_u30_sp_diff = comp$b30_suit_prot - comp$u30_suit_prot
comp$b30_u30_usp_diff = comp$b30_unsuit_prot - comp$u30_unsuit_prot
cat("min max of biodiv30 - uniform30 suit prot ", min(comp$b30_u30_sp_diff), max(comp$b30_u30_sp_diff))
cat("min max of biodiv30 - uniform30 unsuit prot ", min(comp$b30_u30_usp_diff), max(comp$b30_u30_usp_diff))

# write the plot data
write.csv(comp[order(comp$region),], paste0(out_dir, "biodiv_prot_diff_plot_regXglu.csv"), row.names = FALSE)

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


shapefile <- sf::st_read("pre-processing/mapping/reg_glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
plot_df = left_join( shapefile, comp, join_by(reg_id == GCAM_region_ID, glu_id == GLU) )


# biodiv minus uniform 30

# suitable protected difference
g <- ggplot()+
     geom_sf(data= plot_df, aes(fill= b_u30_sp_diff))+
     scale_fill_distiller(palette="RdBu", limits = c(-0.5, 0.5), na.value="red") +
     labs(fill="Difference") +
     scheme_basic

plot(g)
ggsave(paste0(out_dir, "biodiv_u30_suit_prot_diff_regXglu.pdf"), plot=g, device="pdf")

# unsuitable protected difference
g <- ggplot()+
     geom_sf(data= plot_df, aes(fill= b_u30_usp_diff))+
     scale_fill_distiller(palette="RdBu", limits = c(-0.5, 0.5), na.value="red") +
     labs(fill="Difference") +
     scheme_basic

plot(g)
ggsave(paste0(out_dir, "biodiv_u30_unsuit_prot_diff_regXglu.pdf"), plot=g, device="pdf")


# biodiv30 minus uniform 30

# suitable protected difference
g <- ggplot()+
     geom_sf(data= plot_df, aes(fill= b30_u30_sp_diff))+
     scale_fill_distiller(palette="RdBu", limits = c(-0.5, 0.5), na.value="red") +
     labs(fill="Difference") +
     scheme_basic

plot(g)
ggsave(paste0(out_dir, "biodiv30_u30_suit_prot_diff_regXglu.pdf"), plot=g, device="pdf")

# unsuitable protected difference
g <- ggplot()+
     geom_sf(data= plot_df, aes(fill= b30_u30_usp_diff))+
     scale_fill_distiller(palette="RdBu", limits = c(-0.5, 0.5), na.value="red") +
     labs(fill="Difference") +
     scheme_basic

plot(g)
ggsave(paste0(out_dir, "biodiv30_u30_unsuit_prot_diff_regXglu.pdf"), plot=g, device="pdf")

