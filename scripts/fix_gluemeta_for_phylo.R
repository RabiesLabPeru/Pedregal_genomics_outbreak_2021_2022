## fix metadata for phylo plots and associate map

## metadata
annot=read.table("raw_data/sequence_data/glue/glue_LAC_bol_peru34_dogRABV_metadata.txt", sep="\t", header=T)
## tidy clade names
annot$alignment.displayName[annot$alignment.displayName=="Rabies Virus (RABV)"]="Unknown"
annot$alignment.displayName=gsub(" ",":",annot$alignment.displayName)
annot$alignment.displayName[annot$alignment.displayName=="Cosmopolitan"]="Cosmopolitan:NA"
annot$Study="Other"
annot$alignment.displayName=gsub("Cosmopolitan","Cosmo",annot$alignment.displayName)
# Use grepl to find matches and update the 'Study' column
matches = grepl("aqp12|aqp14|illumina", annot$sequence.sequenceID, ignore.case=TRUE)
annot$Study[matches] = "This study"
annot$sequence.gb_length=as.numeric(annot$sequence.gb_length)



# colour schemes
## colours for lots of data
## colours for lots of data
n <-length(unique(annot$sequence.m49_country.display_name))
country_cols=kelly(n+3)[-c(1,2,9)]
#country_cols=trubetskoy(n) # -1 removes the gray
pie(rep(1, n), col=country_cols)
names(country_cols) <- unique(annot$sequence.m49_country.display_name)
colScale1 <- scale_fill_manual(name = expression(italic(II) * ". Country"),values = country_cols, na.translate=F)
n2=length(unique(annot$alignment.displayName))
clade_cols=trubetskoy(n2)
#clade_cols=kelly(n+3)[-c(1,2,9)]
pie(rep(1, length(unique(annot$alignment.displayName))), col=clade_cols)
names(clade_cols) <- unique(annot$alignment.displayName)
colScale2 <- scale_fill_manual(name = expression(italic(I) * ". Phylogenetic clade"),values = clade_cols)

