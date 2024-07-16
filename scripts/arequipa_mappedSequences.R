#install.packages("leaflet")
library(leaflet)
library(mapview)
library(htmltools)
library(shiny)
library(wesanderson)
library(rgdal)

source("R/tidyData.R")

#initialising options
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

#plot basic map layer
m <- leaflet(sequenced) %>% addTiles() 

#col palette
pal <- colorFactor(
  palette = as.character(wes_palette("Darjeeling1")),
  domain = sequenced$year.x)

aqp <- readOGR("/Users/kirstyn/Github/Rabies/Shapefiles AQP/outputs/AQP_districts_city_shp.new.shp")
mainRiver <- readOGR("/Users/kirstyn/Github/Rabies/Shapefiles AQP/outputs/chiliriver_shp_new.shp")

#clusteroptions clusters close cases
mappedWGS=
  m %>% 
  addPolygons(data=aqp,color="gray", popup =paste(aqp$NAME_3))%>% 
  addPolylines(data=mainRiver,color="red")%>% 
  addCircleMarkers(
    data = sequenced, lng=~long, lat=~lat, radius = 5, fillOpacity=0.7,popup =paste("Sample id:", sequenced$seq_id, "<br>","Year:", sequenced$year, "<br>", "District:", sequenced$DISTRITO), fillColor=~pal(year.x), color=~pal(year.x)
    )%>%
  addLegend(
    "topright", pal = pal, values = ~year.x,
            title = "Sample year",
            opacity = 1
    )

#mapshot(mappedWGS, url = paste0(getwd(), "/output/maps/mappedWGS.html"), file=paste0(getwd(), "/output/maps/mappedWGS_districtBorder.png"))


m %>% 
  addPolygons(data=aqp, color="black", popup =paste(aqp$NAME_3))%>% 
  addPolylines(data=mainRiver,color="red")%>%
  addCircleMarkers(
    data = sequenced, lng=~longitude, lat=~latitude, radius = 4, fillOpacity=1,popup =paste("Sample id:", sequenced$seq_id, "<br>","Year:", sequenced$year, "<br>", "District:", sequenced$district), fillColor="black")
    