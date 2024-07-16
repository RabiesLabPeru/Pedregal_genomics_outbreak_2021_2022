

source("scripts/tree_annotation.R")

library(reshape2)
library(outbreaker2)
## patristic distances
pat=distTips(rooted, tips = "all", method ="patristic",useC = TRUE)
#temp <- pairDistPlot(pat, )
desired_columns <- rooted.meta$seq_id[rooted.meta$outbreak_place == "El Pedregal"]
pat.pedregal = pat[which(names(pat) %in% desired_columns)]
rooted.meta$proposedChain="Other"
rooted.meta$proposedChain[which(!is.na(rooted.meta$case))]="Pedregal"
rooted.meta$proposedChain[which(rooted.meta$Sequence.ID %in% c("34_2021","52_2021","68_2021","71_2021","107_2021"))]="chain1"


pat.mat=as.matrix(pat.pedregal)

# temp <- pairDistPlot(pat.mat, rooted.meta$case)
# ## see plots
# temp$boxplot
# temp$violin
# temp$jitter

#write.table(pat.mat, file="output/phylogenetic_distances/romblonSeq_24_patristicDist_matrix.txt")

# Plot frequency distribution of Hamming distances
hist(pat.mat, 
     breaks = seq(0, max(pat.mat) + 0.0005, by=0.0005),
     col = "cyan",
     xlab = "Hamming distance", main = "Distribution of frequencies")
abline(v = 0.0004, col = "red", lwd = 2)
heatmap(pat.mat, col = brewer.pal(9, "YlGnBu")) 
pat.melt <- melt(pat.mat)

View(pat.melt)
# Perform hierarchical clustering
hc <- hclust(as.dist(pat.mat))

# Get the order of tips from the dendrogram
dend_order <- order.dendrogram(as.dendrogram(hc))

# Reorder the matrix based on the dendrogram order
pat.mat.ordered <- pat.mat[dend_order, dend_order]

# Melt the ordered matrix for ggplot
pat.melt <- melt(pat.mat.ordered)

# Create the heatmap with ggplot
ggplot(pat.melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu")) + 
  theme_minimal() +
  labs(x = "Tip 1", y = "Tip 2", fill = "Distance",
       title = "Patristic Distance Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_colorbar(title = "Distance"))

ggplot(pat.melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("grey", "yellow", "orange", "blue", "darkblue"), values = c(0, 0.017, 0.02, 0.5, 1), breaks = c(0.0004, 0.01, 0.02)) +
  theme_minimal() +
  labs(x = "Tip 1", y = "Tip 2", fill = "Distance",
       title = "Patristic Distance Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_colorbar(title = "Distance"))








