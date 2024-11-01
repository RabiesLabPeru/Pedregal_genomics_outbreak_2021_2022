## ### FIGURE 3
library(patchwork)

source("scripts/peru_contextMap.R")
source("scripts/Pedregal sequences map.R")

## put together components of figure 3
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
#cases+inset_element(peru_inlac, left=0, bottom=0.7, right=0.4, top=1.5,align_to = "panel")


tiff("figures/fig1_revised.tif", units = "in", width=11.69, height=9, res= 300, compression = "lzw")
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
dev.off()

pdf("figures/fig1_revised.pdf", width=11.69, height=9)
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
dev.off()

jpeg("figures/fig1_revised.jpg", quality=500, width=11.69, height=9, units="in", res=200)
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
dev.off()


