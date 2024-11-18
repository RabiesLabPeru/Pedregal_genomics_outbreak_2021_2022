# Mapeo de Pedregal asociado a casos de rabia
# Team Rabies - Elvis Diaz - Renzo Salazar
# June 2024 - Modified November 2024

  # Install needed packages
  install.packages(c("ggmap", "sp", "leaflet", "leaflet.extras", "ggspatial", "sf", 
                     "st", "ggnewscale", "rnaturalearth", "rnaturalearthdata", 
                     "cowplot", "ggrepel", "tidyverse", "patchwork"))
  
  # Libraries
    library(ggmap)
    library(sp)
    library(leaflet)
    library(leaflet.extras)
    library(ggspatial)
    library(sf)
    library(st)
    library(ggnewscale)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(cowplot)
    library(ggrepel)
    library(tidyverse)
    library(patchwork)
    
# Mapping cases of Pedregal ---- 
  
  # Calling databases
    setwd("/Users/pmacs/Documents/Mapeos")

  # Read data
    PEDREGAL<-read.csv("PEDREGAL_parcelas.csv")
    PEDREGALURBAN<-read.csv("PEDREGAL_centro.csv")
    CASOS2021<-read.csv2("Casos_Rabia_2021_Seq.csv")
    CASOS2022<-read.csv2("Casos_Rabia_2022_Seq.csv")
    
  # Rabies cases
    vector_2021 <- rep("2021", times = 76)
    CASOS2021 <- cbind(CASOS2021, Year = vector_2021) 
    
    vector_2022 <- rep("2022", times = 51)
    CASOS2022 <- cbind(CASOS2022, Year = vector_2022) 
  
  # join data by year
    CASOSPED <- bind_rows(CASOS2021,CASOS2022) 
    
    CASOSPED <- select(CASOSPED,ident,long,lat,Year,Sequenced,Cases_ID)
    CASOSPED <- na.omit(CASOSPED)
    CASOSPED$ident <- gsub("Caso0","",CASOSPED$ident)
    CASOSPED <- CASOSPED %>%
                st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # Pedregal blocks
    PED <- select(PEDREGAL,ident,long,lat)
    PED <- na.omit(PED)
    
    PED <- PED %>%
           st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
           group_by(ident) %>%
           summarise(geometry = st_combine(geometry)) %>%
           st_cast("POLYGON")
      
    rural <- rep("Rural", times = 2755)
    PED <- cbind(PED, zone = rural) 
  
  # Pedregal urban blocks
    PEDURB <- select(PEDREGALURBAN,ident,long,lat)
    PEDURB <- na.omit(PEDURB)
    
    PEDURB <- PEDURB %>%
              st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
              group_by(ident) %>%
              summarise(geometry = st_combine(geometry)) %>%
              st_cast("POLYGON")
    
    urban <- rep("Urban", times = 2620)
    PEDURB <- cbind(PEDURB, zone = urban)
  
  # join blocks data
    PEDMAN <- rbind(PEDURB,PED)
    PEDMAN$zone <- as.factor(PEDMAN$zone)
    
  # Figure
    cases = ggplot() + 
            geom_sf(data = PEDMAN, color = "white",size=0.1,aes(fill=zone) )+ 
            scale_fill_manual("Type of zone",
                              values = c("#fddbc7","steelblue1"),
                              breaks = c("Rural","Urban"),
                              labels = c("Agricultural lots","City blocks")) +
            guides(fill = guide_legend(order = 1))+
            new_scale_fill()+
            theme_minimal() +
            theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16),
                  panel.border = element_rect(color = "black", fill = NA),
                  panel.grid.major = element_blank(),  
                  panel.grid.minor = element_blank())  + 
          # geom_sf(data = CASOSPED, aes( color=Sequenced, shape= Year), size = 4,alpha=0.85)+
            # add case identifiers
            geom_label_repel(data = CASOSPED, aes(label = Cases_ID, geometry = geometry, fill=Year, col=Sequenced), size = 4, fontface = "bold", stat = "sf_coordinates", max.overlaps = 11)+
            scale_fill_manual("Year", values = c("light grey","white")) +
            scale_color_manual("Sequenced", values = c("#f21f1f","#0f0000"))+
            new_scale_fill()+
             # geom_sf_text(data = CASOSPED, aes(label = Cases_ID), size = 4, color = "black", check_overlap = F, fontface="bold") +
            annotation_scale(location = "bl", height = unit(0.4, "cm"),
                             pad_x = unit(0.5, "cm"),pad_y = unit(0.8, "cm")) +
            annotation_north_arrow(location = "bl", which_north = "true", 
                                   style = north_arrow_fancy_orienteering,
                                   height = unit(1.4, "cm"),
                                   width = unit(1.4, "cm"),
                                   pad_x = unit(2.5, "cm"), pad_y = unit(2, "cm"))+
            coord_sf(xlim = c(-72.155, -72.297), ylim = c(-16.29, -16.386), expand = FALSE); cases
  
      
   
# mapping regions with canine rabies ---- 
  # Download the limits of Peru and Bolivia
    peru <- ne_countries(scale = "medium", country = "Peru", returnclass = "sf")
    bolivia <- ne_countries(scale = "medium", country = "Bolivia", returnclass = "sf")
    chile <- ne_countries(scale = "medium", country = "Chile", returnclass = "sf")
    brazil <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")
  
  # Obtain the departmental boundaries of Peru
    peru_departments <- ne_states(country = "Peru", returnclass = "sf")
  
  # Filter the departments of Arequipa and Puno
    arequipa <- peru_departments %>% filter(name == "Arequipa")
    puno <- peru_departments %>% filter(name == "Puno")
  
  # Define coordinates of points of interest
    sites <- data.frame(
      place = c("Puno", "Arequipa", "El Pedregal"),
      lat = c(-15.8402, -16.4090, -16.4031),
      long = c(-70.0219, -71.5375, -72.1957))
    
    sites_1 <- data.frame(
      place = c("Puno\ncity", "Arequipa\ncity"),
      lat = c(-15.8402, -16.4090),
      long = c(-70.0219, -71.5375))
    
    sites_2 <- data.frame(
      place = c("El Pedregal"),
      lat = c(-16.4031),
      long = c(-72.1957))
    
  # Define coordinates for country names
    country_labels <- data.frame(
      country = c("Peru", "Bolivia", "Chile", "Brazil"),
      lat = c(-13, -16,-19.5, -9),
      long = c(-72.5, -67,-69.4,-68.7))
    
    ocean_label <- data.frame(
      label = "Pacífic Ocean",
      lat = -17,
      long = -73.8)
  
  # Convert the data frame to sf object
    sites_sf <- st_as_sf(sites, coords = c("long", "lat"), crs = 4326)
  
  # Create the map
    map_1 = ggplot() +
            geom_sf(data = peru, fill = "ivory", color = "gray10", alpha = 0.7) +
            geom_sf(data = bolivia, fill = "ivory", color = "gray10", alpha = 0.7) +
            geom_sf(data = chile, fill = "ivory", color = "gray10", alpha = 0.7) +
            geom_sf(data = brazil, fill = "ivory", color = "gray10", alpha = 0.7) +
            geom_sf(data = arequipa, aes(color = "Arequipa"), fill =  "burlywood1", alpha = 0.5) +
            geom_sf(data = puno, aes(color = "Puno"), fill = "lightpink", alpha = 0.4) +
            geom_sf(data = sites_sf, color = "darkred", size = 3, show.legend = FALSE) +
            geom_text(data = sites_1, aes(x = long, y = lat, label = place), vjust = -0.6, hjust = 0.52, size = 3.5, color = "black") +
            geom_text(data = sites_2, aes(x = long, y = lat, label = place), vjust = -1.2, hjust = 1.15, size = 3.5, color = "black") +
            geom_text(data = country_labels, aes(x = long, y = lat, label = country),
                      fontface = "bold", size = 7, color = "gray8") +
            geom_text(data = ocean_label, aes(x = long, y = lat, label = label),
                      angle = -30, fontface = "italic", size = 6, color = "lightskyblue4") +
            coord_sf(xlim = c(-76, -66), ylim = c(-20, -8), expand = FALSE) +
            scale_color_manual(name = "Region", values = c("Arequipa" = "#fddbc7", "Puno" = "lightpink")) +
            theme_minimal() +
            theme(panel.background = element_rect(fill = "lightcyan", color = NA),
                  axis.text.x = element_blank(),  
                  axis.text.y = element_blank(),  
                  axis.ticks = element_blank(),   
                  axis.title.x = element_blank(), 
                  axis.title.y = element_blank(),
                  panel.border = element_rect(color = "gray20", fill = NA),
                  legend.text = element_text(size = 12),  
                  legend.title = element_text(size = 14)) +
            annotation_scale(location = "bl", height = unit(0.4, "cm"),
                             pad_x = unit(1, "cm"),pad_y = unit(0.8, "cm")) +
            annotation_north_arrow(location = "br", which_north = "true", 
                                   style = north_arrow_fancy_orienteering,
                                   height = unit(1.4, "cm"),
                                   width = unit(1.4, "cm"),
                                 pad_x = unit(11.5, "cm"), pad_y = unit(2, "cm"))

    
  # joining maps ----
    mapa1 <- map_1 + labs(title = "A")
    mapa2 <- cases + labs(title = "B")
    
  # Combine the two maps into a single graph
    mapa_combinado <- mapa1 + mapa2 + plot_layout(ncol = 2)
    
  # save map in various formats
    ggsave("maps_pedregal.png", plot = mapa_combinado, width = 1900 / 96, height = 800 / 96, dpi = 96)
    ggsave("maps_pedregal.jpeg", plot = mapa_combinado, width = 1900 / 96, height = 800 / 96, dpi = 96)
    ggsave("maps_pedregal.tiff", plot = mapa_combinado, width = 1900 / 96, height = 800 / 96, dpi = 96)
    