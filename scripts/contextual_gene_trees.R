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
library(dplyr)

# n gene tree
tree=read.newick("processed_data/trees/gene_trees/glue_bol_and_peru.aln_n.fasttree")

## metadata
source("scripts/fix_gluemeta_for_phylo.R")

## have to edit lenghts in annot, as sequences have been subset to N gene only (max 1353)
annot$sequence.gb_length[which(annot$sequence.gb_length>1355)]=1355
## edit display names
annot$display=gsub("_illumina","",annot$sequence.sequenceID)
annot$display=gsub("_meta.*","",annot$display)
annot$display=gsub("_aqp.*","",annot$display)

# tree plots:  aesthetics, ladderize
root_tree=root(tree, "KF154998.1")
root_tree=drop.tip(root_tree, "KF154998.1")


#which(!root_tree$tip.label %in% annot$sequence.sequenceID)
gplot <- ggtree(root_tree,ladderize = TRUE, size=0.2, col="darkgrey") %<+% annot

##Peru relevant mrcas
#plot(tree_plot) + geom_text(aes(label=node), hjust=.3, size=0.6)
mrca=MRCA(root_tree, annot$sequence.sequenceID[annot$Study=="This study"])
#viewClade(tree_plot, mrca)                 
mrca2=1607 # for all peruvian seq in that section of tree


# highlight the study cases 
tree_plot=
  gplot+
  ## added bootstraps >0.8 on internal nodes
  geom_nodepoint(color="grey", shape=18, alpha=1, size=2 , aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
  #geom_tippoint(aes(subset=(Study %in% "This study")),col="grey", pch=21, fill="black")
  #theme_tree2()+
  #geom_treescale(x=0, y=700, width=20, color='black', label="SNPs",linesize=1, offset=2)+
  # labs(caption="Number of substitutions")+
  layout_rectangular()+
  geom_hilight(node=mrca)+
  geom_treescale(offset=2, fontsize=6)+
  geom_fruit(
    geom=geom_star,
    mapping=aes(subset=(Study %in% "This study"),fill=Study,  starshape=Study),
    fill=country_cols[names(country_cols)=="Peru"],
    position="identity",colour="black",
    starstroke=0.1, size=1,offset=0.2
  )+
  guides(starshape = guide_legend(order=1, title="",override.aes = list(size = 5)))+
  geom_fruit(geom=geom_tile, mapping=aes(fill=alignment.displayName), width=0.02)+theme(legend.text=element_text(size=10))+
  colScale2+ theme(legend.position='bottom') + guides(fill = guide_legend(ncol = 3, order=2, title.position ="top"))+
  new_scale_fill()+
  # geom_fruit(geom=geom_tile, mapping=aes(fill=sequence.m49_country.display_name), width=0.05, offset = 0.15)+
  # colScale1+ guides(fill = guide_legend(ncol = 3))           +                                     
  geom_fruit(
    geom=geom_col,
    mapping=aes(x=sequence.gb_length, fill=sequence.m49_country.display_name),
    pwidth=0.6,
    offset = 0.1,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.angle=0,
      text.size=6,# the text angle of x-axis.
      vjust=1, hjust=0 ,nbreak=3 # adjust the horizontal position of text of axis.
    )
  )+   colScale1+
  theme(legend.position="top", legend.box = "vertical",legend.margin = margin(0, 0, 0, 0)) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))+
  theme(legend.key.size = unit(0.4, "cm"))+
  geom_cladelabel(node=mrca2, label="Cosmo:Am5",color="black",fontsize=6)+
  theme(legend.position="none"); tree_plot

#guides(fill = guide_legend("ncol = 3, order=3, title.position ="top""none)); tree_plot

leg <- as.ggplot(ggpubr::get_legend(tree_plot), position="top")+
  theme(legend.title=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"))
tree_plot2=tree_plot+theme(legend.position="none")+
  geom_text(aes(x=0.25,y= 1080), label="Clade",size=5)+
  geom_text(aes(x=0.34,y= 1080), label="Length & country", size=5); tree_plot2



#%>%  scaleClade(node=mrca, scale=2)

######################

##expand the Peru clade 
#MRCA(root_tree, annot$sequence.sequenceID[annot$sequence.m49_country.display_name=="Peru"])

subset_tree <- tidytree::tree_subset(root_tree, mrca, levels_back = 0, root_edge = T)
zoom_peru <- ggtree(subset_tree,ladderize = TRUE, size=0.5, col="darkgrey") %<+% annot


zoom1=zoom_peru+
  layout_rectangular()+
  geom_tiplab(aes(label=display), size=4, hjust=-0.1)+
  geom_treescale(offset=0.5, fontsize=6)+
  geom_nodepoint(color="grey", shape=18, alpha=1, size=3, aes(subset= !is.na(as.numeric(label)) & as.numeric(label) > 0.8))+
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
    size=3, offset=0.4
  )+
  colScale1+
  #  scale_fill_manual(name = "",
  # labels =  c('Other', 'This study'),
  # values=country_cols, na.translate=FALSE) +   
  scale_starshape_manual(values=c(13,1), na.translate=FALSE)+
  guides(starshape = F,fill=FALSE)+
  guides(fill=FALSE)+
  new_scale_fill()+
  geom_fruit(geom=geom_tile, 
            mapping=aes(fill=alignment.displayName),
            width=0.002)+
              theme(legend.text=element_text(size=10))+
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
      text.angle=0,
      text.size=6,# the text angle of x-axis.
      vjust=1, hjust=0 ,nbreak=3 #adjust the horizontal position of text of axis
    )
  )+  
  colScale1+
  theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"))+ # top, right, bottom, left)
  guides(fill =F)+  geom_text(aes(x=0.03,y= 60), label="Clade", size=5)+
  geom_text(aes(x=0.04,y= 60), label="Length & country",size=5);zoom1





