## ### FIGURE 3
library(patchwork)

source("scripts/peru_contextMap.R")
source("scripts/Pedregal sequences map.R")

## put together components of figure 3
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')


tiff("figures/fig1.tif", units = "in", width=11.69, height=9, res= 300, compression = "lzw")
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
dev.off()

pdf("figures/fig1.pdf", width=11.69, height=9)
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
dev.off()

jpeg("figures/fig1.jpg", quality=500, width=11.69, height=9, units="in", res=200)
(peru_inlac|cases)+ plot_annotation(tag_levels = 'A')
dev.off()


