##tree annotation

library(treeio)
library(phytools)
library(ggtree)
library(wesanderson)
library(ggplot2)  
library(ggpubfigs)
library(ggpubr)
library(ggtreeExtra)
library(RColorBrewer)
library(ape)
library(castor)
library(phangorn)
library(viridis)
library(ggnewscale)
library(patchwork)
library(readr)
library(lubridate)
library(BactDating)

## DATA#################
### trees
dated=read.beast("processed/trees/all_lsd_CI.date.nexus")

## metadata## metadprocessed/trees/analysis_seq_af4outgp.nwkata
annot=read.csv("processed/trees/deduped.csv")

# foundation tree plot:  aesthetics, ladderize
gplot <- ggtree(dated, ladderize = TRUE, size=0.1, mrsd="2022-11-17", aes(color=group)) %<+% annot
gplot
gplot$data$ACRdates=format(as.Date(decimal2Date(as.numeric(gplot$data$date))), "%d-%b-%y")


gplot+
  theme_tree2()+
  #geom_text(label=gplot$data$node)+
  geom_tiplab(aes(label=display), size=2.5)+
  scale_x_continuous(breaks=seq(2010, 2024, 2), minor_breaks=seq(2010, 2024, 1)) +
  geom_nodepoint(color="darkgray", shape=18, alpha=1, size=2 , aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
  geom_fruit(geom=geom_tile, mapping=aes(fill=outbreak_place), width=2, offset=0.2)+
  scale_fill_manual(name="Location",values=wes_palette("FantasticFox1", 3, type = "discrete"),
                    labels =sort(unique(rooted.meta$outbreak_place)))+
  theme(legend.position = "bottomright")+guides(fill=guide_legend(nrow=2,title.position="top"))+ 
    geom_nodelab(geom="label",aes(subset =  node %in% c(43,57), label=ACRdates), fontface=2,  nudge_x = -0.8, nudge_y = 1,size=3, color="black")

