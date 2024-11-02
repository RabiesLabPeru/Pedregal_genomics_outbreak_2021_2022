#extract partitions for beast
library(seqinr)
#BiocManager::install("ORFik")
#BiocManager::install("DECIPHER")
library(ORFik)
library(DECIPHER)
library(devtools)
#install_github("thibautjombart/apex")
#install.packages("apex")
library("apex")
library(stringr)

#input file
file="processed_data/multifasta/glue_alignments/glue_bol_and_peru.aln.fasta"

#output files
dir.create(file.path(paste(dirname(file), "glue_gene_alignments", sep="/")), showWarnings = FALSE)
# set output file location (same as input) and prefix
newfiles=paste(dirname(file), "glue_gene_alignments", gsub(".fasta|.fst","",basename(file)), sep="/")

# read in sequences as string set for ORF function
string=readDNAStringSet(file)
writeXStringSet(string, paste0(newfiles, ".fasta"), format="fasta")

# find most complete genome and search for ORFs
chosen=which.max(str_count(as.character(string), "A|T|G|C"))
#chosen.seq=RemoveGaps(string[chosen], removeGaps = "common")
genes=as.data.frame(findORFs(string[chosen], startCodon = "ATG", minimumLength =200))

# it can't find the correct M gene start point so have to pull out manually
genes=genes[order(genes$start),]
genes=genes[-3,]
find.m=as.data.frame(findORFs(string[chosen], startCodon = "ATG", minimumLength =200, longestORF = F))
find.m=find.m[order(find.m$start),]
m=find.m[which(find.m$start>=2468 & find.m$width==609),]
#join with other genes
genes=rbind(genes, m)
genes=genes[order(genes$start),]
genes=genes[,-c(1,2)]
genes$gene=NA
genes$gene=c("n","p","m","g","l")

# read in sequences as fasta file
seq=read.fasta(paste0(newfiles, ".fasta"))

# split into coding partitions (5 genes)
# based on ORF positions
n=getFrag(seq, begin=genes$start[1],end=genes$end[1])
p=getFrag(seq, begin=genes$start[2],end=genes$end[2])
m=getFrag(seq, begin=genes$start[3],end=genes$end[3])
g=getFrag(seq, begin=genes$start[4],end=genes$end[4])
l=getFrag(seq, begin=genes$start[5],end=genes$end[5])

n_strip=n[sapply(n, function(x) any(x != "-"))]
p_strip=n[sapply(p, function(x) any(x != "-"))]
m_strip=n[sapply(m, function(x) any(x != "-"))]
g_strip=n[sapply(g, function(x) any(x != "-"))]
l_strip=n[sapply(l, function(x) any(x != "-"))]

# output partitions as fasta files
write.fasta(n_strip,names=sapply(n_strip, function(x) attr(x, "seqMother")), paste(newfiles,"n.fasta",sep="_"))
write.fasta(p_strip,names=seq_mothers <- sapply(p_strip, function(x) attr(x, "seqMother")), paste(newfiles,"p.fasta",sep="_"))
write.fasta(m_strip,names=seq_mothers <- sapply(m_strip, function(x) attr(x, "seqMother")), paste(newfiles,"m.fasta",sep="_"))
write.fasta(g_strip,names=seq_mothers <- sapply(g_strip, function(x) attr(x, "seqMother")), paste(newfiles,"g.fasta",sep="_"))
write.fasta(l_strip,names=seq_mothers <- sapply(l_strip, function(x) attr(x, "seqMother")), paste(newfiles,"l.fasta",sep="_"))
