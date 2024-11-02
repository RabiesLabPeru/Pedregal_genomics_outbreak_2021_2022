# appendix figures

## ngene contextual tree

## figure 2

library(patchwork)

source("scripts/contextual_tree_map.R")
source("scripts/contextual_gene_trees.R")


plot=tree_plot2|zoom1
plot+ plot_annotation(tag_levels = 'A')



tiff("figures/fig_ngene_tree.tif", units = "in", width=11.69, height=8.27, res= 300, compression = "lzw")
plot+ plot_annotation(tag_levels = 'A')
dev.off()

pdf("figures/fig_ngene_tree.pdf", width=11.69, height=8.27,)
plot+ plot_annotation(tag_levels = 'A')
dev.off()

jpeg("figures/fig_ngene_tree.jpg", quality=1000, width=11.69, height=8.27,, units="in", res=200)
plot+ plot_annotation(tag_levels = 'A')
dev.off()
