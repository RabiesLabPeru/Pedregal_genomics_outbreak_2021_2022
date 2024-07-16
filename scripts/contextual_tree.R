### Annotate contextual tree
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
library(pals)
library(ggplotify)
library(ggstar)


tree=read.newick("processed_data//trees/glue_bol_and_peru.nwk")

## metadata
source("scripts/fix_gluemeta_for_phylo.R")


# tree plots:  aesthetics, ladderize
root_tree=root(tree, "KF154998.1")
root_tree=drop.tip(root_tree, "KF154998.1")
#which(!root_tree$tip.label %in% annot$sequence.sequenceID)
gplot <- ggtree(root_tree,ladderize = TRUE, size=0.2, col="darkgrey") %<+% annot

##Peru relevant mrcas
#plot(tree_plot) + geom_text(aes(label=node), hjust=.3, size=0.6)
mrca=MRCA(root_tree, annot$sequence.sequenceID[annot$Study=="This study"])
#viewClade(tree_plot, mrca)
mrca2=1433 # for all peruvian seq in that section of tree

# highlight the romblon cases 
tree_plot=
  gplot+
#geom_tippoint(aes(subset=(Study %in% "This study")),col="grey", pch=21, fill="black")
  #theme_tree2()+
  #geom_treescale(x=0, y=700, width=20, color='black', label="SNPs",linesize=1, offset=2)+
 # labs(caption="Number of substitutions")+
  layout_rectangular()+
  geom_hilight(node=mrca)+
     geom_treescale(col="grey")+
    geom_fruit(
      geom=geom_star,
      mapping=aes(subset=(Study %in% "This study"),fill=Study,  starshape=Study),
      fill=country_cols[names(country_cols)=="Peru"],
      position="identity",colour="black",
      starstroke=0.1, size=1
    )+
    guides(starshape = guide_legend(order=1, title="",override.aes = list(size = 5)))+
geom_fruit(geom=geom_tile, mapping=aes(fill=alignment.displayName), width=0.02)+theme(legend.text=element_text(size=10))+
    colScale2+ theme(legend.position='bottom') + guides(fill = guide_legend(ncol = 3, order=2, title.position ="top"))+
  new_scale_fill()+
    # geom_fruit(geom=geom_tile, mapping=aes(fill=sequence.m49_country.display_name), width=0.05, offset = 0.15)+
    # colScale1+ guides(fill = guide_legend(ncol = 3))+
    geom_fruit(
      geom=geom_col,
      mapping=aes(x=sequence.gb_length, fill=sequence.m49_country.display_name),
      pwidth=0.6,
      offset = 0.1,
      axis.params=list(
        axis="x", # add axis text of the layer.
        text.angle=-45,
        text.size=2,# the text angle of x-axis.
        vjust=1, hjust=0 ,nbreak=10 # adjust the horizontal position of text of axis.
      )
    )+   colScale1+ 
  theme(legend.position="top", legend.box = "vertical",legend.margin = margin(0, 0, 0, 0)) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))+
  theme(legend.key.size = unit(0.4, "cm"))+
  geom_cladelabel(node=mrca2, label="Cosmo:Am5", 
                  color="black")+
  guides(fill = guide_legend(ncol = 3, order=3, title.position ="top")); tree_plot

leg <- as.ggplot(ggpubr::get_legend(tree_plot), position="top")+
  theme(legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"))
tree_plot2=tree_plot+theme(legend.position="none")
#%>%  scaleClade(node=mrca, scale=2)

######################

  ##expand the Peru clade 
    #MRCA(root_tree, annot$sequence.sequenceID[annot$sequence.m49_country.display_name=="Peru"])
  subset_tree <- tidytree::tree_subset(root_tree, mrca, levels_back = 0, root_edge = T)
  subset_tree2 <- tidytree::tree_subset(root_tree, mrca2, levels_back = 0)

  
# edit function to improve plot:
#edited.zoomClade<- edit(zoomClade)  
body(zoomClade)[[8]] <- substitute(p + facet_zoom(xlim = xr, ylim = yr, zoom.data=F, zoom.size=0,show.area=T, horizontal=F))
#  p + facet_zoom(xlim = xr, ylim = yr, zoom.data=F, zoom.size=0,show.area=F)


# peru MRCA only    
tree_plot %>% zoomClade(node=mrca, xexpand = 0)
# MRCA of all peru cluster

zoom_section=tree_plot %>% zoomClade(node=mrca2, xexpand = 0) 

zoom_peru <- ggtree(subset_tree,ladderize = TRUE, size=0.5, col="darkgrey") %<+% annot
zoom_peru2 <- ggtree(subset_tree2,ladderize = TRUE, size=0.5, col="darkgrey") %<+% annot

zoom1=zoom_peru+
  layout_rectangular()+
  geom_treescale(col="grey")+
  geom_nodepoint(color="grey21", shape=18, alpha=1, size=2 , aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
  # geom_fruit(
  #   geom=geom_star,
  #   mapping=aes(starshape=Study),
  #   position="identity",fill="grey",
  #   size=2.6
  # )+
  geom_fruit(
    geom=geom_star,
    mapping=aes(fill=sequence.m49_country.display_name,  starshape=Study),
    position="identity",colour="black", starstroke=0.1, 
   size=2
  )+
 colScale1+
#  scale_fill_manual(name = "",
                    # labels =  c('Other', 'This study'),
                    # values=country_cols, na.translate=FALSE) +   
  scale_starshape_manual(values=c(13,1), na.translate=FALSE)+
  guides(starshape = F,fill=FALSE)+
  guides(fill=FALSE)+
  new_scale_fill()+
  geom_fruit(geom=geom_tile, mapping=aes(fill=alignment.displayName), width=0.005)+theme(legend.text=element_text(size=10))+
  colScale2+ theme(legend.position='bottom') + guides(fill = guide_legend(ncol = 1, order=2,title.position = "top"))+
  guides(fill =F)+
  new_scale_fill()+
  geom_fruit(
    geom=geom_col,
    mapping=aes(x=sequence.gb_length, fill=sequence.m49_country.display_name),
    pwidth=0.6,
    offset = 0.1,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.angle=-45,
      text.size=2,# the text angle of x-axis.
      vjust=1, hjust=0 ,nbreak=10 # adjust the horizontal position of text of axis.
    )
  )+  
colScale1+
  theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"))+ # top, right, bottom, left)
  guides(fill =F);zoom1



zoom2=zoom_peru2+
  layout_rectangular()+
  geom_treescale(col="grey")+
  geom_nodepoint(color="grey21", shape=18, alpha=1, size=3 , aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.9))+
  geom_fruit(
    geom=geom_star,
    mapping=aes(fill=sequence.m49_country.display_name,  starshape=Study),
    position="identity",colour="black",
    starstroke=0.2, size=2.5
  )+
  colScale1+
  #  scale_fill_manual(name = "",
  # labels =  c('Other', 'This study'),
  # values=country_cols, na.translate=FALSE) +   
  scale_starshape_manual(values=c(13,1), na.translate=FALSE)+
  guides(starshape = FALSE,fill=FALSE)+
  guides(fill=FALSE)+
  new_scale_fill()+
  geom_fruit(geom=geom_tile, mapping=aes(fill=alignment.displayName), width=0.008)+theme(legend.text=element_text(size=10))+
  colScale2+ theme(legend.position='bottom') +guides(fill=FALSE)+
  new_scale_fill()+
  geom_fruit(
    geom=geom_col,
    mapping=aes(x=sequence.gb_length, fill=sequence.m49_country.display_name),
    pwidth=0.6,
    offset = 0.1,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.angle=-45,
      text.size=2,# the text angle of x-axis.
      vjust=1, hjust=0 ,nbreak=10 # adjust the horizontal position of text of axis.
    )
  )+  
  colScale1+
  guides(fill =FALSE);zoom2

  