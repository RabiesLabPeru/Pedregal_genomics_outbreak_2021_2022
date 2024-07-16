# Genomic analysis rabies sequences Gene N
##########################################

# manipulate sequences
install.packages("seqinr")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ORFik")

BiocManager::install("DECIPHER")

install.packages("ORFik")
#install.packages("DECIPHER")
install.packages("devtools")
#install_github("thibautjombart/apex")
install.packages("apex")
library("apex")
install.packages("stringr")
install.packages("Biostrings")
install.packages("sjPlot")
library(ggplot2)
library(remotes)
install_github("r-spatial/sf")
install.packages("sf")
install.packages("reshape")
install.packages("ggspatial")
install.packages("viridis")
library(dplyr)

#---------------------------------
#load Packages
library("seqinr")
#BiocManager::install("ORFik")
#BiocManager::install("DECIPHER")
library("ORFik")
library("DECIPHER")
library("devtools")
#install_github("thibautjombart/apex")
#install.packages("apex")
library("apex")
library("stringr")
library("Biostrings")
library("sjPlot")
library("ggplot2")
library("sf")
library("reshape")
library("ggspatial")
library("viridis")
library("dplyr")

#------------------------------------
#setwd
setwd("D:/RENZOSSS/RSSS/RABIES MINION/genomic data/PEDREGAL/N gene")
getwd()

#--------------------------------
#input file
file="pedregal_analysis.aln_SHORTnames.fasta"
#######################################
#outgroup
# run the next line to get split the outgroup and get the n gene
#file="KX148106.1 Lyssavirus rabies vulpes OUTGROUP.fasta"
#########################################
#---------------------------------
#output files
dir.create(file.path(paste(dirname(file), "output", sep="/")), showWarnings = FALSE)
# set output file location (same as input) and prefix
newfiles=paste(dirname(file), "output", gsub(".fasta|.fst","",basename(file)), sep="/")
#---------------------------------
# read in sequences as string set for ORF function
string=readDNAStringSet(file)
#remove any alignment errors (sometimes present if alignment has been subset). Write and replace file
string=RemoveGaps(string, removeGaps = "common")
#names(string) <- vapply(strsplit(labels(string),".",fixed=T), `[`, 1, FUN.VALUE=character(1)) #extract sampleID from seq name
names(string) <- vapply(strsplit(labels(string),"/",fixed=T), `[`, 1, FUN.VALUE=character(1)) #extract sampleID from seq name
names(string)
#writeXStringSet(string, paste0(newfiles, ".fasta"), format="fasta")

#-------------------------------------------------
# find most complete genome and search for ORFs
#chosen=which.max(str_count(as.character(string), “A|T|G|C”))# first option
chosen=which.max(width(string))
#chosen.seq=RemoveGaps(string[chosen], removeGaps = "common")
genes=as.data.frame(findORFs(string[chosen], startCodon = "ATG", minimumLength =200))

#-------------------------------------------------
# it can't find the correct M gene start point so have to pull out manually
genes=genes[order(genes$start),]
genes=genes[-3,]
find.m=as.data.frame(findORFs(string[chosen], startCodon = "ATG", minimumLength =200, longestORF = F))
find.m=find.m[order(find.m$start),]
m=find.m[which(find.m$start>=2468 & find.m$width==609),]

#------------------------------------------------
#join with other genes
genes=rbind(genes, m)
genes=genes[order(genes$start),]
genes=genes[,-c(1,2)]
genes$gene=NA
genes$gene=c("n","p","m","g","l")

seq=read.fasta(paste0(newfiles, ".fasta"))
names(seq)

#------------------------------------------------------
# split into coding partitions (5 genes)
# based on ORF positions
n=getFrag(seq, begin=genes$start[1],end=genes$end[1])
p=getFrag(seq, begin=genes$start[2],end=genes$end[2])
m=getFrag(seq, begin=genes$start[3],end=genes$end[3])
g=getFrag(seq, begin=genes$start[4],end=genes$end[4])
l=getFrag(seq, begin=genes$start[5],end=genes$end[5])
#---------------------------------------------------
#check sequences names in Ngene
class(n)
names(n)
names(n)<-substring(names(string),1,19)
names(n)
length(n)
#--------------------------------------
# output partitions as fasta files
write.fasta(n,names=names(seq), paste(newfiles,"Ngene_Outgroup.fasta",sep="_"))
#write.fasta(p,names=names(seq), paste(newfiles,"p.fasta",sep="_"))
#write.fasta(m,names=names(seq), paste(newfiles,"m.fasta",sep="_"))
#write.fasta(g,names=names(seq), paste(newfiles,"g.fasta",sep="_"))
#write.fasta(l,names=names(seq), paste(newfiles,"l.fasta",sep="_"))
Ngene.pedr<-n
############################################################
#run next lines to get fasta file for Ngene Outgroup
#write.fasta(n,names=names(seq), paste(newfiles,"Ngene_Outgroup.fasta",sep="_"))
#Ngene.out<-n
###########################################################
###########################################################
#-------------------------------------------------------------
#subset Ngene from large search into long seqs and short seqs
#-------------------------------------------------------------
#import file
seqs <- read.fasta(file = "Ngene_only.unaln_n.fasta")
#outg<- read.fasta(file = "pedregal_analysis.aln_SHORTnames.fasta")
length(seqs)
getLength(seqs)
#-----------------------------------------------
#subset: use 90% coverage as reference for subseting
seqsL<-subset(seqs,getLength(seqs)>1216)
seqsS<-subset(seqs,getLength(seqs)<1217)
length(getLength(seqsL))
#----------------------------------------------
#add Ngenes sequences from Pedregal and outgroup to long seqs subset
class(seqsL)
class(n)
all.Ngene.seqs<-c(seqsL,Ngene.pedr,Ngene.out)
length(seqsL)
length(Ngene.pedr)
length(Ngene.out)
length(all.Ngene.seqs)
#---------------------------------------------------------
#export
write.fasta(sequences=seqsL, names=names(seqsL),file.out="Ngene_long_seqs.fasta")
write.fasta(sequences=seqsS, names=names(seqsS),file.out="Ngene_short_seqs.fasta")

write.fasta(sequences=all.Ngene.seqs, names=names(all.Ngene.seqs),file.out="All_Ngene_seqs3.fasta")
---------------------------------------------------------------

#####

#add pedregal seqs to Long seqs file and align
#after this add fragments







#######################

names(seq) <- vapply(strsplit(labels(seq),"/",fixed=T), `[`, 1, FUN.VALUE=character(1)) #extract sampleID from seq name
names(string)<-Short.names
names(string)
names(seq)<-Short.names
names(genes)

#change to short names

names.seqs<- vapply(strsplit(labels(string),"/",fixed=T), `[`, 1, FUN.VALUE=character(1)) #extract sampleID from seq name
class(names.seqs)
Short.names<-substring(names.seqs,1,10)
Short.names[c(1,7,9,15,16,18,19)]<-substring(names.seqs[c(1,7,9,15,16,18,19)],11,20)
Short.names[4:6]<-substring(names.seqs[4:6],11,20)
names.seqs[c(1,7,9,15,16,18,19)]
Short.names
names(n)