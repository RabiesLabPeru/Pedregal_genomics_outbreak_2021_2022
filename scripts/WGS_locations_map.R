library(sf)
library(dplyr)
library(tools)


##----------------

source("scripts/Pedregal sequences map.R")
source("scripts/tree_annotation.R")


## convert sequence data points into sf
points_sf <- st_as_sf(rooted.meta[!is.na(rooted.meta$long),], coords = c("long", "lat"), crs = 4326)

# el pedregal city extent box
pedregal_extent <- st_bbox(PEDMAN)
# Create a polygon from the bounding box
pedregal_extent_polygon <- st_as_sfc(st_bbox(pedregal_extent))
# Set the CRS (assuming your data is in EPSG:4326)
st_crs(pedregal_extent_polygon ) <- st_crs(PEDMAN)

# Arequipa city extent box
aqp_extent <- st_bbox(points_sf[points_sf$outbreak_place=="Arequipa",])
# Create a polygon from the bounding box
aqp_extent_polygon <- st_as_sfc(st_bbox(aqp_extent))
# Set the CRS (assuming your data is in EPSG:4326)
st_crs(aqp_extent_polygon ) <- st_crs(points_sf)


# shapefiles/ polygons
world <- ne_countries(returnclass = "sf") 
peru=world[world$admin=="Peru"|world$admin=="Bolivia",]

department=read_sf("/Users/kirstyn.brunker/Library/CloudStorage/OneDrive-UniversityofGlasgow/General - RAGE/Peru/Peru Shapefiles/DEPARTAMENTOS_inei_geogpsperu_suyopomalia/DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")

district=read_sf("/Users/kirstyn.brunker/Library/CloudStorage/OneDrive-UniversityofGlasgow/General - RAGE/Peru/Peru Shapefiles/DISTRITOS_inei_geogpsperu_suyopomalia/DISTRITOS_inei_geogpsperu_suyopomalia.shp")

province=read_sf("/Users/kirstyn.brunker/Library/CloudStorage/OneDrive-UniversityofGlasgow/General - RAGE/Peru/Peru Shapefiles/PROVINCIAS_inei_geogpsperu_suyopomalia/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")

# subset to puno region, arequipa province and majes district
puno <- department %>% filter(NOMBDEP == "PUNO")
aq2 <- province %>% filter(NOMBPROV == "AREQUIPA")
aqq <- district %>% filter(NOMBDIST == "AREQUIPA")
majes <- district %>% filter(NOMBDIST == "MAJES")

# version1, area polygons coloured, generic points
# ggplot() 
#  geom_sf(data = puno, fill = wes_palette("FantasticFox1", 3, type = "discrete")[[3]],size=0.1)+
#  # geom_sf(data = aqp, fill = "white",size=0.1)+  
#   geom_sf(data = aq2, fill = wes_palette("FantasticFox1", 3, type = "discrete")[[1]],size=0.1)+ 
#   geom_sf(data = majes, fill = "white",size=0.1)+
#   geom_sf(data = extent_polygon, fill = "white",size=0.1)+
#   geom_sf(data = PEDMAN, color = wes_palette("FantasticFox1", 3, type = "discrete")[[2]],size=0.1)+
#   #geom_sf(data = puno_relevant, color = "lightgrey",size=0.1)+
#   theme( axis.line = element_blank(),
#          axis.text = element_blank(),
#          axis.title = element_blank(),
#          axis.ticks = element_blank(),
#          legend.text = element_text(size=14),
#          legend.title = element_text(size=16),
#          panel.border = element_rect(color = "black", fill = NA),
#          panel.grid.major = element_blank(),  # Remove major gridlines
#          panel.grid.minor = element_blank())  + # Remove minor gridlines) +
#   geom_sf(data = points_sf) +
#   scale_color_manual(name="Location",values=wes_palette("FantasticFox1", 3, type = "discrete"), labels =sort(unique(rooted.meta$outbreak_place)))+
#   scale_shape_manual(name="Location",values=c(15,17,16),
#                      labels =sort(unique(rooted.meta$outbreak_place)))
#   

# version2, points coloured, polygons labelled
WGS_map=ggplot() + 
  geom_sf(data=peru, size=2)+
  geom_sf(data = puno, fill = "white",size=0.1, linetype="dashed")+
 geom_sf(data = aq2, fill = "white",size=0.1)+ 
  geom_sf(data = majes, fill = "white",size=0.1)+
  geom_sf(data = pedregal_extent_polygon, fill = "white",size=0.1)+
  geom_sf(data = aqp_extent_polygon, fill = "white",size=0.1)+
  #geom_sf(data = PEDMAN, fill = "white",size=0.1, linetype="dashed")+
  geom_sf_text(data = puno,aes(label=NOMBDEP), nudge_y=-0.3 )+
  annotate("text", x = -69.1, y = -15.7, label = "Bolivia", 
           size = 5, color = "blue", fontface = "bold")+
  geom_sf_text(data = peru,aes(label=admin) )+
  annotate("text", x = -72, y = -15.5, label = "Peru", 
           size = 5, color = "blue", fontface = "bold")+
  geom_sf_text(data = peru,aes(label=admin) )+
 geom_sf_text(data = majes,aes(label=NOMBDIST), nudge_y=-0.18, , nudge_x=-0.1,size=3)+
  geom_sf_text(data = aq2,aes(label=NOMBPROV), nudge_y=-0.18, , nudge_x=-0.1,size=3)+
  #geom_sf_text(data = majes,aes(label=NOMBDIST), nudge_y=0.1)+
  geom_label_repel(data = aqp_extent_polygon,aes(label="Arequipa City", geometry=geometry), stat = "sf_coordinates", nudge_y=0.25, nudge_x=0.25)+
  geom_label_repel(data = pedregal_extent_polygon,aes(label="El Pedregal", geometry=geometry), stat = "sf_coordinates", nudge_y=0.25)+
  #geom_sf(data = puno_relevant, color = "lightgrey",size=0.1)+
  theme( axis.line = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.text = element_text(size=14),
         legend.title = element_text(size=16),
         panel.border = element_blank(),
         #panel.border = element_rect(color = "black", fill = NA),
         panel.grid.major = element_blank(),  # Remove major gridlines
         panel.grid.minor = element_blank())  + # Remove minor gridlines) +
  geom_sf(data = points_sf,aes(col=outbreak_place, shape=outbreak_place))+
  scale_color_manual(name="Location",values=wes_palette("FantasticFox1", 5, type = "discrete"), labels =sort(unique(rooted.meta$outbreak_place)))+
  scale_shape_manual(name="Location",values=c(15,17,16),
                     labels =sort(unique(rooted.meta$outbreak_place)))+
  coord_sf(xlim = c(-68.8,-72.6), ylim = c(-15,-17), expand = FALSE)+
  annotation_scale(location = "br", height = unit(0.4, "cm"),
                   pad_x = unit(3, "cm"),pad_y = unit(0.8, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.4, "cm"),
                         width = unit(1.4, "cm"),
                         pad_x = unit(3, "cm"), pad_y = unit(1.5, "cm"))+
  theme(legend.position="none"); WGS_map
