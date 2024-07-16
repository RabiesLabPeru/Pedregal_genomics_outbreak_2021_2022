## figure 2

library(patchwork)

source("scripts/contextual_tree_map.R")
source("scripts/contextual_tree.R")


plot=(tree_plot2/zoom1)|(plot_lac/leg)
plot+ plot_annotation(tag_levels = 'A')


tiff("figures/fig2.tif", units = "in", width=11.69, height=8.27, res= 300, compression = "lzw")
plot+ plot_annotation(tag_levels = 'A')
dev.off()

pdf("figures/fig2.pdf", width=11.69, height=8.27,)
plot+ plot_annotation(tag_levels = 'A')
dev.off()

jpeg("figures/fig2.jpg", quality=1000, width=11.69, height=8.27,, units="in", res=200)
plot+ plot_annotation(tag_levels = 'A')
dev.off()
