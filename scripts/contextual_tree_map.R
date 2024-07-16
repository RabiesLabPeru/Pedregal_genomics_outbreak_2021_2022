## map

#############################################
#            INSTALL PACKAGES               #
#############################################
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)
library(sf)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(ggnewscale)
library(dplyr)
library(patchwork)   
library(RColorBrewer)
library(pals)
#############################################
#              MAPS               #
#############################################


## metadata (called annot)
source("scripts/fix_gluemeta_for_phylo.R")

# base map of the world
world <- ne_countries(returnclass = "sf") 
lac=world[world$subregion=="South America"|world$subregion=="Central America"|world$subregion=="Caribbean",]
tiny=tiny_countries50
grenada=tiny[which(tiny$admin=="Grenada"),]

#' Find which country names do not match between data and map file
map_countries <- as.character(lac$admin)
countries <- unique(annot$sequence.m49_country.display_name)
no_match <- setdiff(countries, map_countries); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))

# List all the countries included in the metadata
countries <- unique(annot$sequence.m49_country.display_name)

# Have a quick look at the number of sequences per country, and the total number of countries
country_table<-table(annot$sequence.m49_country.display_name); country_table
length(country_table)

sequences=lac[lac$admin %in% countries,]



#############################################
#              MAKE THE MAPS                #
#############################################

### LAC map

# add centroid points
lac_points<- st_point_on_surface(lac)
lac_points <- cbind(lac, st_coordinates(st_point_on_surface(lac$geometry)))

# Plot a basic map which we will add pies to
plot_lac<-
  ggplot(data = 
           lac) +
  geom_sf(data=lac, fill="white")+
    geom_sf(data=sequences, aes(fill=admin))+
  colScale1+
  geom_sf(data=grenada,pch=15, size=2, aes(col=admin))+
  scale_color_manual(name = "Country",values = country_cols, na.translate=F)+
  theme(panel.grid.major = element_blank())+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        #this bit removes the axis lables     
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = "aliceblue"))+
   guides(fill="none", col="none"); plot_lac
 # geom_text(data=subset(lac_points, admin %in% countries),aes(x=X, y=Y, label=admin),color = "darkblue", fontface = "bold", check_overlap = F, size=3, nudge_y = -1.5)

