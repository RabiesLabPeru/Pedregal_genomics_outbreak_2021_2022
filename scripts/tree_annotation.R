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
library(adephylo)


## DATA#################
### trees
all_seq=read.tree("processed_data/trees/aqp_paper_renamed.af4outgp.nwk")

## metadata## 
all_seq.annot=read.csv("raw_data/metadata/Pedregal_tree_metadata_corrected.csv")
all_seq.annot$Sequence.ID <- sub("(_[^_]*_)[^_]*$", "\\1", all_seq.annot$seq_id)
all_seq.annot$Sequence.ID <- sub("_$", "", all_seq.annot$Sequence.ID)
all_seq.annot$Sequence.ID <- gsub("_meta-illumina", "", all_seq.annot$Sequence.ID)

## outgrp rooted tree
rooted <- root(all_seq, outgroup = "KF154998.1", edgelabel = TRUE)
drop=c("KF154998.1", "CNExt_2023_aqp13","147_2021_aqp13","202_2021_aqp13","34_2021_aqp13","71_2021_aqp13", "676_1999_illumina")
rooted=drop.tip(rooted,drop)
rooted.meta=all_seq.annot[all_seq.annot$seq_id %in% rooted$tip.label,]
rooted.meta$display=gsub("_a.*|_meta.*","",rooted.meta$seq_id)
readdate=as.Date(rooted.meta$sample_collection_date, "%d-%b-%y")
rooted.meta$decimal_date=decimal_date(readdate)
#write.csv(rooted.meta,"processed/trees/deduped.csv", row.names=F)

rooted$edge.length=rooted$edge.length*11923
# foundation tree plot:  aesthetics, ladderize
gplot <- ggtree(rooted, ladderize = TRUE, size=0.7,col="darkgray") %<+% rooted.meta
gplot


final=gplot+
  #geom_tiplab(aes(label=display), size=2.5)+
#  geom_label2(aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
  geom_nodepoint(color="darkgray", shape=18, alpha=1, size=3 , aes(subset=as.numeric(label) >0.8))+
  geom_treescale(x=90, y=4, label="~SNPs",offset.label=-1, offset=1)+
geom_tippoint(mapping=aes(col=outbreak_place, shape=outbreak_place, size=outbreak_place))+
  scale_color_manual(name="Location",values=wes_palette("FantasticFox1", 3, type = "discrete"),
                    labels =sort(unique(rooted.meta$outbreak_place)))+
  scale_shape_manual(name="Location",values=c(15,17,16),
                     labels =sort(unique(rooted.meta$outbreak_place)))+
  scale_size_manual(name="Location",values=c(3,5,3),
                     labels =sort(unique(rooted.meta$outbreak_place)))+
  geom_tiplab(aes(label=case), size=4, hjust=1, fontface="bold")+
  geom_tiplab(aes(label=year), size=4, hjust=-0.3)+
  # geom_fruit(geom=geom_tile, mapping=aes(fill=as.factor(year)), width=5, offset=0.2)+
  # scale_fill_manual(name = "Year",values=brewer.bupu(10),labels =sort(unique(rooted.meta$year)),na.translate=FALSE)+
  # theme(legend.position = "bottomright")+guides(fill=guide_legend(nrow=2,title.position="top"))+
  # new_scale_fill()+
  # geom_fruit(geom=geom_tile, mapping=aes(fill=as.factor(run_id)), width=5, offset=0.07)+
  # scale_fill_manual(name = "Seq run",values=wes_palette("Zissou1", 6, type = "continuous"),labels =sort(unique(rooted.meta$run_id)),na.translate=FALSE)+
  theme(legend.position = "bottom",legend.text=element_text(size=12),legend.title=element_text(size=12));final

  
# 
# tiff("figures/fig3.tif", units = "in", width=11.69, height=8.27, res= 300, compression = "lzw")
# final
# dev.off()
# 
# pdf("figures/fig3.pdf", width=11.69, height=8.27)
# final
# dev.off()
# 
# jpeg("figures/fig3.jpg", quality=500, width=11.69, height=8.27, units="in", res=200)
# final
# dev.off()
