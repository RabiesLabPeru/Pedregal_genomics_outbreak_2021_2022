# Mask amplicon contamination in consensus sequence

library(seqinr) 
#install.packages("remotes")
#remotes::install_github("joelnitta/baitfindR")
library(baitfindR)

## Inputs
## Consensus sequences from the run at different depth thresholds

depth50_files=list.files("data/sequence_data/consensus_sequences/aqp13", pattern=".consensus.fasta", full.names = T, recursive=T)
depth100_files=list.files("data/sequence_data/consensus_sequences/depth100", pattern=".consensus.fasta", full.names = T)
depth200_files=list.files("data/sequence_data/consensus_sequences/depth200", pattern=".consensus.fasta", full.names = T)


## Code to mask each sequence

### From consensus threshold 50, mask anything with at depth greater than 50
for (i in 1:length(depth50_files)){
run=seqinr::read.fasta(depth50_files[i], forceDNAtolower = F)
name=gsub(".consensus.fasta","",paste(sapply(strsplit(basename(depth50_files[i]),"_"), `[`, ),sapply(strsplit(basename(depth50_files[i]),"_"), `[`, 5), sep="_"))
name="aqp13"

## output dirs
dir.create(paste0("contamination/consensus_masking/",name,"/dp50_mask50"),recursive = T)


## Problematic amplicon info (from processed negative control)
# merge if negative template and negative extraction controls, otherwise just import one csv
contamination1=read.csv(paste0("contamination/negative_contamination_profiles/",name,"_mask_amp_50.csv"))
all_contamination=contamination1

# convert into bed format 
## need the exact fasta file header in first column of bed file so have to extract that info for each sequence individually
bed <- data.frame(chr=c(rep("name",nrow(all_contamination))),
                  start=all_contamination$Start,
                  end=all_contamination$End)
names(bed)=NULL

dir.create(paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed"), recursive=T)

  tempname=names(run)
  filename=sapply(strsplit(tempname,"/"), `[`, 1)
  bed[,1]=names(run)
  write.table(bed, file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.bed"), sep="\t", row.names=F, quote=F)
  write.fasta(run, names=names(run), file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.fasta"))
  
  input_fasta=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.fasta")
  bed_file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.bed")
  output_fasta=paste0("contamination/consensus_masking/",name,"/dp50_mask50/", filename,".dp50_mask50.fasta") 
  
  
#mask_regions_in_fasta(bed_file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.bed"), fasta_file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.fasta"), out_fasta_file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/", filename,".dp50_mask50.fasta") )
  
  command <- paste("conda run -n ngs bedtools", "maskfasta", "-fi", input_fasta, "-bed", bed_file, "-fo", output_fasta)
  system(command)
  
unlink(paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed"), recursive=T)

}

input_fasta=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.fasta")
bed_file=paste0("contamination/consensus_masking/",name,"/dp50_mask50/temp_bed/temp.bed")
output_fasta=paste0("contamination/consensus_masking/",name,"/dp50_mask50/", filename,".dp50_mask50.fasta") 
Sys.setenv(PATH=paste("/Users/kirstyn.brunker/mambaforge/bin", Sys.getenv("PATH"), sep=":"))

command <- paste("conda run -n ngs bedtools", "maskfasta", "-fi", input_fasta, "-bed", bed_file, "-fo", output_fasta)
system(command)

### From consensus threshold 50, mask anything with depth greater than 200

for (i in 1:length(depth50_files)){
  run=seqinr::read.fasta(depth50_files[i], forceDNAtolower = F)
  name=gsub(".consensus.fasta","",paste(sapply(strsplit(basename(depth50_files[i]),"_"), `[`, 4),sapply(strsplit(basename(depth50_files[i]),"_"), `[`, 5), sep="_"))
  
  
  dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200"),recursive = T)
  
  ## Problematic amplicon info (from processed negative control)
  # merge if negative template and negative extraction controls, otherwise just import one csv
  all_contamination=read.csv(paste0("processed/sequences/contamination/negative_contamination_profiles/",name,"_mask_amp_200.csv"))
  if(nrow(all_contamination)==0){
#next
    file.copy(depth50_files[i], paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/"))
  }else{
  
  # convert into bed format 
  ## need the exact fasta file header in first column of bed file so have to extract that info for each sequence individually
  bed <- data.frame(chr=c(rep("name",nrow(all_contamination))),
                    start=all_contamination$Start,
                    end=all_contamination$End)
  names(bed)=NULL
  
  dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/temp_bed"), recursive=T)
  
    tempname=names(run)
    filename=sapply(strsplit(tempname,"/"), `[`, 1)
    bed[,1]=names(run)
    write.table(bed, file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/temp_bed/temp.bed"), sep="\t", row.names=F, quote=F)
    write.fasta(run, names=names(run), file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/temp_bed/temp.fasta"))
    mask_regions_in_fasta(bed_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/temp_bed/temp.bed"), fasta_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/temp_bed/temp.fasta"), out_fasta_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/", filename,".dp50_mask200.fasta") )
  unlink(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp50_mask200/temp_bed"), recursive=T)
}
}
### From consensus threshold 100, mask anything with at depth greater than 100

for (i in 1:length(depth100_files)){
  run=seqinr::read.fasta(depth100_files[i], forceDNAtolower = F)
  name=gsub(".consensus.fasta","",paste(sapply(strsplit(basename(depth100_files[i]),"_"), `[`, 4),sapply(strsplit(basename(depth100_files[i]),"_"), `[`, 5), sep="_"))



  dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100"),recursive = T)

  ## Problematic amplicon info (from processed negative control)
  # merge if negative template and negative extraction controls, otherwise just import one csv
  contamination1=read.csv(paste0("processed/sequences/contamination/negative_contamination_profiles/",name,"_mask_amp_100.csv"))
  all_contamination=contamination1
  if(nrow(all_contamination)==0){
    #next
    file.copy(depth200_files[i], paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/"))
  }else{

  # convert into bed format
  ## need the exact fasta file header in first column of bed file so have to extract that info for each sequence individually
  bed <- data.frame(chr=c(rep("name",nrow(all_contamination))),
                    start=all_contamination$Start,
                    end=all_contamination$End)
  names(bed)=NULL

  dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/temp_bed"), recursive=T)
  dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/"), recursive=T)

    tempname=names(run)
    filename=sapply(strsplit(tempname,"/"), `[`, 1)
    bed[,1]=names(run)
    write.table(bed, file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/temp_bed/temp.bed"), sep="\t", row.names=F, quote=F)
    write.fasta(run, names=names(run), file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/temp_bed/temp.fasta"))
    mask_regions_in_fasta(bed_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/temp_bed/temp.bed"), fasta_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/temp_bed/temp.fasta"), out_fasta_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/", filename,".dp100_mask100.fasta") )

  unlink(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp100_mask100/temp_bed"), recursive=T)

 }
}
#

### From consensus threshold 200, mask anything with at depth greater than 200

for (i in 1:length(depth200_files)){
  run=seqinr::read.fasta(depth200_files[i], forceDNAtolower = F)
  name=gsub(".consensus.fasta","",paste(sapply(strsplit(basename(depth200_files[i]),"_"), `[`, 4),sapply(strsplit(basename(depth200_files[i]),"_"), `[`, 5), sep="_"))
  
  
  
  dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200"),recursive = T)
  
  ## Problematic amplicon info (from processed negative control)
  # merge if negative template and negative extraction controls, otherwise just import one csv
  contamination1=read.csv(paste0("processed/sequences/contamination/negative_contamination_profiles/",name,"_mask_amp_200.csv"))
  all_contamination=contamination1
  if(nrow(all_contamination)==0){
    #next
    file.copy(depth200_files[i], paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/"))
  }else{
    
    # convert into bed format 
    ## need the exact fasta file header in first column of bed file so have to extract that info for each sequence individually
    bed <- data.frame(chr=c(rep("name",nrow(all_contamination))),
                      start=all_contamination$Start,
                      end=all_contamination$End)
    names(bed)=NULL
    
    dir.create(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/temp_bed"), recursive=T)

      tempname=names(run)
      filename=sapply(strsplit(tempname,"/"), `[`, 1)
      bed[,1]=names(run)
      write.table(bed, file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/temp_bed/temp.bed"), sep="\t", row.names=F, quote=F)
      write.fasta(run, names=names(run), file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/temp_bed/temp.fasta"))
      mask_regions_in_fasta(bed_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/temp_bed/temp.bed"), fasta_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/temp_bed/temp.fasta"), out_fasta_file=paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/", filename,".dp200_mask200.fasta") )
    unlink(paste0("processed/sequences/contamination/consensus_masking/",name,"/dp200_mask200/temp_bed"), recursive=T)
  }
}

