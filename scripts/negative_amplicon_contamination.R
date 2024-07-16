## investigate products sequenced 
library(ggplot2)
#library(plyranges)
library(dplyr)
library(optparse)
library(readr)
library(plyranges)
#BiocManager::install("plyranges")

## code using negative control coverage profile to mask contamination

#output:
dir.create(paste0("processed/contamination/negative_contamination_profiles"),recursive = T)

# input=alignment report from artic pipeline
files=list.files("data/sequence_data/negatives_alignreport/dp50_alignment/",full.names = T)
# here: using an alignment report that contains incorrect pairings
for (i in 1:length(files)){
a=read.table(files[i], header=T)
name=gsub(".alignreport.txt","",paste(sapply(strsplit(basename(files[i]),"_"), `[`, 4),sapply(strsplit(basename(files[i]),"_"), `[`, 5), sep="_"))
name=gsub("feb20a", "feb20_a",name)
a$size=as.numeric(a$End)-as.numeric(a$Start)
#summarise by primerPair
b=summarise(group_by(a, PrimerPair, Primer1, Start, End,size),n =dplyr::n())
b$lab=parse_number(gsub("rabvPeru2","",b$Primer1))
reads=sum(b$n)

# amplicons > 200,100,50  read coverage
b.200=b[b$n>200,]
b.100=b[b$n>100,]
b.50=b[b$n>50,]

#proportion of genome contaminated at those levels, merging overlapping amplicons
if(nrow(b.50)==0){prop_50=0}else{
range50=reduce(data.frame(seqnames="chr1", start=b.50$Start, end=b.50$End)  %>% as_granges())
prop_50=round(sum(width(ranges(range50)))/11923,2)
}

if(nrow(b.100)==0){prop_100=0}else{
range100=reduce(data.frame(seqnames="chr1", start=b.100$Start, end=b.100$End)  %>% as_granges())
prop_100=round(sum(width(ranges(range100)))/11923,2)
}

if(nrow(b.200)==0){prop_200=0}else{
range200=reduce(data.frame(seqnames="chr1", start=b.200$Start, end=b.200$End)  %>% as_granges())
prop_200=round(sum(width(ranges(range200)))/11923,2)
}
#prop_50; prop_100; prop_200
genome_contamination=data.frame(name, reads, prop_50, prop_100,prop_200)

#write.csv(b, file=paste("barcode02.alignreport.txt", 'productList','.csv',sep=''),row.names=F)
# plot- amplicons by read number
# show the specific amplicon products i.e. between 300-500 in length
p=ggplot(subset(b,size>=300 & size<=500), aes(x=Start, xend=End, y=n, yend=n)) + geom_segment(alpha=0.3,col="blue") + theme_bw(base_size=14) + xlab("Position in genome") + ylab("Number of reads")+
  geom_text(aes(label=lab, x=(Start + End)/2), size=2,col="black") + geom_hline(yintercept=200, linetype="dashed", color = "blue") + geom_hline(yintercept=50, linetype="dashed", color = "lightblue")
# But... what about non specific products? added to plot
p+geom_segment(data=subset(b,size>=500), aes(x=Start, xend=End, y=n, yend=n), col="red", alpha=0.3)+ ggtitle(paste(name))
#
ggsave(paste0("processed/contamination/negative_contamination_profiles/",name,"_amplicon_contamination",'.png'), width=10, height=6)
write.csv(b,paste0("processed/contamination/negative_contamination_profiles/",name, "_amplicon_contamination.csv"), row.names=F)
write.csv(b.200,paste0("processed/contamination/negative_contamination_profiles/",name, "_mask_amp_200.csv"), row.names=F)
write.csv(b.100,paste0("processed/contamination/negative_contamination_profiles/",name, "_mask_amp_100.csv"), row.names=F)
write.csv(b.50,paste0("processed/contamination/negative_contamination_profiles/",name, "_mask_amp_50.csv"), row.names=F)
if (!file.exists(paste0("processed/contamination/negative_contamination_profiles/", "genome_contamination.csv"))){
  write.csv(genome_contamination, file =paste0("processed/contamination/negative_contamination_profiles/", "genome_contamination.csv"), row.names=F)
}else{
write.table(genome_contamination, file =paste0("processed/contamination/negative_contamination_profiles/", "genome_contamination.csv") , sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)}

#
}
