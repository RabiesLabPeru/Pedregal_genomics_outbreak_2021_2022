# Mapeo de Pedregal asociado a casos de rabia
# Team Rabies - Elvis Diaz - Renzo Salazar
# June 2024


# Install needed packages
#install.packages("ggmap")
#install.packages("leaflet")
#install.packages("leaflet.extras")
#install.packages("st")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("tidyverse")


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


# Read data
  PEDREGAL<-read.csv("raw_data/PEDREGAL_parcelas.csv")
  
  PEDREGALURBAN<-read.csv("raw_data/PEDREGAL_centro.csv")
  
  CASOS2021<-read.csv2("raw_data/Casos_Rabia_2021_Seq.csv")
  
  CASOS2022<-read.csv2("raw_data/Casos_Rabia_2022_Seq.csv")
  
  
  dim(CASOS2022)
  

# Rabies cases
  vector_2021 <-rep("2021", times = 76)
  CASOS2021 <- cbind(CASOS2021, Year = vector_2021) 
  
  vector_2022 <-rep("2022", times = 51)
  CASOS2022 <- cbind(CASOS2022, Year = vector_2022) 

# join data by year
  CASOSPED <- bind_rows(CASOS2021,CASOS2022) 
  
  CASOSPED <- dplyr::select(CASOSPED,ident,long,lat,Year,Sequenced,Cases_ID)
  CASOSPED <- na.omit(CASOSPED)
  CASOSPED$ident <- gsub("Caso0","",CASOSPED$ident)
  CASOSPED <- CASOSPED %>%st_as_sf(coords = c("long", "lat"), crs = 4326)

# Pedregal blocks
  PED<-dplyr::select(PEDREGAL,ident,long,lat)
  PED<- na.omit(PED)
  
  PED<-PED %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(ident) %>%
    summarise(geometry = st_combine(geometry)) %>%st_cast("POLYGON")
  
  rural<-rep("Rural", times = 2755)
  PED <- cbind(PED, zone = rural) 

# Pedregal urban blocks
  PEDURB<-dplyr::select(PEDREGALURBAN,ident,long,lat)
  PEDURB<- na.omit(PEDURB)
  
  PEDURB<-PEDURB %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(ident) %>%
    summarise(geometry = st_combine(geometry)) %>%st_cast("POLYGON")
  
  urban<-rep("Urban", times = 2620)
  PEDURB <- cbind(PEDURB, zone = urban)

# join blocks data
  PEDMAN<-rbind(PEDURB,PED)
  PEDMAN$zone<-as.factor(PEDMAN$zone)

  
# Figure
  cases=ggplot() + 
  geom_sf(data = PEDMAN, color = "white",size=0.1,aes(fill=zone) )+ 
    scale_fill_manual("Type of zone",
                      values = c("#fddbc7","#d1e5f0"),
                      breaks = c("Rural","Urban"),
                      labels = c("Agricultural lots","City blocks")) +
    guides(fill = guide_legend(order = 1))+
    new_scale_fill()+
  theme_minimal() +
    theme( axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(size=14),
      legend.title = element_text(size=16),
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank())  + # Remove minor gridlines) 
 # geom_sf(data = CASOSPED, aes( color=Sequenced, shape= Year), size = 4,alpha=0.85)+
    # add case identifiers
    geom_label_repel(data = CASOSPED, aes(label = Cases_ID, geometry = geometry, fill=Year, col=Sequenced), size = 4, fontface = "bold", stat = "sf_coordinates", max.overlaps = 11)+
    scale_fill_manual("Year",
                      values = c("light grey","white")) +
    scale_color_manual("Sequenced", values = c("#f21f1f","#0f0000"))+
    new_scale_fill()+
     # geom_sf_text(data = CASOSPED, aes(label = Cases_ID), size = 4, color = "black", check_overlap = F, fontface="bold") +
    annotation_scale(location = "br", height = unit(0.4, "cm"),
                     pad_x = unit(1, "cm"),pad_y = unit(0.8, "cm")) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           style = north_arrow_fancy_orienteering,
                           height = unit(1.4, "cm"),
                           width = unit(1.4, "cm"),
                           pad_x = unit(1.60, "cm"), pad_y = unit(2.00, "cm"))+
    coord_sf(xlim = c(-72.11, -72.33), ylim = c(-16.25, -16.465), expand = FALSE); cases

    


