library(sf)
library(dplyr)
library(tools)

source("scripts/WGS_locations_map.R")
# shapefiles/ polygons
world <- ne_countries(returnclass = "sf") 
lac=world[world$subregion=="South America"|world$subregion=="Central America",]
peru_only=world[world$admin=="Peru",]
bordering <- world[world$admin %in% c("Bolivia", "Ecuador", "Chile", "Brazil", "Colombia"), ]

# Separate Chile data for customized label positioning
chile <- bordering %>%
  filter(admin == "Chile")


#Majes centroid to represent el pedregal
pedregal_point <- st_centroid(pedregal_extent_polygon)
# arequipa centroid point
arequipa_point <- st_centroid(aqp_extent_polygon)
# puno centroid point
puno_point <- st_centroid(puno)

# Plot a basic map which we will add pies to
 peru_inlac= ggplot(data = 
           lac) +
  geom_sf(data=lac, fill="lightgrey", col="lightgrey")+
    theme( axis.line = element_blank(),
           axis.text = element_blank(),
           axis.title = element_blank(),
           axis.ticks = element_blank(),
           legend.text = element_text(size=14),
           legend.title = element_text(size=16),
           panel.border = element_blank(),
           panel.background = element_rect(fill="aliceblue"),
           panel.grid.major = element_blank(),  # Remove major gridlines
           panel.grid.minor = element_blank())+ 
    geom_sf(data=peru_only, fill="brown3", col="black") +
    geom_sf(data=bordering, fill="lightgrey", col="black", linetype="dashed") +
    geom_sf_text(data = bordering %>% filter(admin != "Chile"), aes(label = admin), size = 4, nudge_x=1) +
    geom_sf_text(data = chile, aes(label = admin), size = 4, nudge_x = 0.5, nudge_y = 30) +
    geom_sf_text(data = peru_only,aes(label=admin), fontface="bold",size=6)+
    #geom_sf(data = puno, fill = "white",size=0.1, linetype="dashed")+
   # geom_sf(data = aq2, fill = "white",size=0.1)+ 
   #geom_sf(data = arequipa_point, color = "black", size = 2)+
   geom_label_repel(data = pedregal_point,aes(label="Arequipa", geometry=geometry), stat = "sf_coordinates", nudge_x=14, nudge_y=-7)+
   geom_label_repel(data = puno_point,aes(label="Puno", geometry=geometry), stat = "sf_coordinates", nudge_x=4.5, nudge_y=6)+
    geom_label_repel(data = pedregal_point,aes(label="El Pedregal", geometry=geometry), stat = "sf_coordinates", nudge_x=-10, nudge_y=-4); peru_inlac

 
 
 zoom=ggplot() + 
   geom_sf(data=peru, size=2)+
   geom_sf(data = puno, fill = "white",size=0.1, linetype="dashed")+
   geom_sf(data = aq2, fill = "white",size=0.1)+ 
   geom_sf(data = majes, fill = "white",size=0.1)+
   geom_sf(data = pedregal_point, color = "black", size = 3)+
   geom_sf(data = arequipa_point, color = "black", size = 3)+
   #geom_sf(data = PEDMAN, fill = "white",size=0.1, linetype="dashed")+
   geom_sf_text(data = puno,aes(label=NOMBDEP), nudge_y=-0.3 )+
   annotate("text", x = -69.1, y = -15.7, label = "Bolivia", 
            size = 3, color = "blue", fontface = "bold")+
   geom_sf_text(data = peru,aes(label=admin) )+
   annotate("text", x = -72, y = -15.5, label = "Peru", 
            size = 3, color = "blue", fontface = "bold")+
   geom_sf_text(data = peru,aes(label=admin) )+
   geom_sf_text(data = majes,aes(label=NOMBDIST), nudge_y=-0.18, , nudge_x=-0.1,size=3)+
   geom_sf_text(data = aq2,aes(label=NOMBPROV), nudge_y=-0.18, , nudge_x=-0.1,size=3)+
   #geom_sf_text(data = majes,aes(label=NOMBDIST), nudge_y=0.1)+
   geom_label_repel(data = aqp_extent_polygon,aes(label="Arequipa City", geometry=geometry), stat = "sf_coordinates", nudge_y=0.25, nudge_x=0.25,size=2)+
   geom_label_repel(data = pedregal_extent_polygon,aes(label="El Pedregal", geometry=geometry), stat = "sf_coordinates", nudge_y=0.25, size=2)+
   #geom_sf(data = puno_relevant, color = "lightgrey",size=0.1)+
   theme( axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          #panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_blank(),  # Remove major gridlines
          panel.grid.minor = element_blank())  + # Remove minor gridlines) +
   coord_sf(xlim = c(-68.8,-72.6), ylim = c(-15,-17), expand = FALSE)+
   annotation_scale(location = "br", height = unit(0.2, "cm"))+
   theme(legend.position="none"); zoom
 
  
  
    