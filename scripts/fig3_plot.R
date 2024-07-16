### FIGURE 3
library(patchwork)

source("scripts/tree_annotation.R")
source("scripts/WGS_locations_map.R")

## put together components of figure 

#final|WGS_map

final+inset_element(WGS_map, left=0, bottom=0.3, right=0.55, top=1.1,align_to = "full")


tiff("figures/fig3.tif", units = "in", width=11.69, height=8.27, res= 300, compression = "lzw")
final+inset_element(WGS_map, left=0, bottom=0.5, right=0.5, top=1,align_to = "full")
dev.off()

pdf("figures/fig3.pdf", width=11.69, height=8.27)
final+inset_element(WGS_map, left=0, bottom=0.5, right=0.5, top=1,align_to = "full")
dev.off()

jpeg("figures/fig3.jpg", quality=500, width=11.69, height=8.27, units="in", res=200)
final+inset_element(WGS_map, left=0, bottom=0.5, right=0.5, top=1,align_to = "full")
dev.off()

