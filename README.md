# Genomic Characterization of a Dog-Mediated Rabies Outbreak in El Pedregal, Arequipa, Peru

Public repository containing all the code and de-identified data for: https://www.biorxiv.org/content/10.1101/2024.08.21.608982v1

Renzo Salazar*, Kirstyn Brunker*, Elvis W. Díaz, Edith Zegarra, Ynes Monroy,  Gorky N. Baldarrago, Katty Borrini-Mayorí1, Micaela De la Puente-León1, Sandeep Kasaragod, Michael Z. Levy, Katie Hampson, Ricardo Castillo-Neyra

*Equal contributions

## About
**Background** Rabies, a re-emerging zoonosis with the highest known human case fatality rate, has been largely absent from Peru, except for endemic circulation in the Puno region on the Bolivian border and re-emergence in Arequipa City in 2015, where it has persisted. In 2021, an outbreak occurred in the rapidly expanding city of El Pedregal near Arequipa, followed by more cases in 2022 after nearly a year of epidemiological silence. While currently under control, questions remain regarding the origin of the El Pedregal outbreak and implications for maintaining rabies control in Peru.

**Methods** We sequenced 25 dog rabies virus (RABV) genomes from the El Pedregal outbreak (n=11) and Arequipa City (n=14) from 2021-2023 using Nanopore sequencing in Peru. Historical genomes from Puno (n=4, 2010-2012) and Arequipa (n=5, 2015-2019), were sequenced using an Illumina approach in the UK. In total, 34 RABV genomes were generated, including archived and newly obtained samples. The genomes were analyzed phylogenetically to understand the outbreak's context and origins. 

**Results** Phylogenomic analysis identified two genetic clusters in El Pedregal: 2021 cases stemmed from a single introduction unrelated to Arequipa cases, while the 2022 sequence suggested a new introduction from Arequipa rather than persistence. In relation to canine RABV diversity in Latin America, all new sequences belonged to a new minor clade, Cosmopolitan Am5, sharing relatives from Bolivia, Argentina, and Brazil.

**Conclusion** Genomic insights into the El Pedregal outbreak revealed multiple introductions over a 2-year window. Eco-epidemiological conditions, including migratory worker patterns, suggest human-mediated movement drove introductions. Despite outbreak containment, El Pedregal remains at risk of dog-mediated rabies due to ongoing circulation in Arequipa, Puno, and Bolivia. Human-mediated movement of dogs presents a major risk for rabies re-emergence in Peru, jeopardizing regional dog-mediated rabies control. Additional sequence data is needed for comprehensive phylogenetic analyses.


## Methods
This paper combines epidemiological and genomic analyses briefly described below:

### Sequencing
Illumina sequencing was carried out on archived RNA using protocols described in [Brunker et al. 20023](https://doi.org/10.1093/ve/vev011).

Nanopore sequencing was carried out following protocols from [Brunker *et al*. 2020](https://dx.doi.org/10.17504/protocols.io), with videos to support implementation available from [Bautista *et al* 2023](https://www.jove.com/de/t/65414/whole-genome-sequencing-for-rapid-characterization-rabies-virus-using).


### Phylogenetic analysis
1. Trees were generated from publicly available data downloaded from [RABV-GLUE](http://rabv-glue.cvr.gla.ac.uk/#/home) and new sequences generated as part of this study. The sequence data and associated metadata is available in the data folder.
2. Data were processed following a pipeline for combining WGS and partial sequences described in [Holz *et al*. 2022](https://www.nature.com/articles/s41467-023-39847-x) and Yuson & Bautista et al. in press, with custom R scipts to curate, clean and process data.
3. Sequence alignment and phylogenetic reconstruction was undertaken following methods described in [Bautista *et al. in press*](https://github.com/boydorr/outbreak_romblon_PHL/blob/main/README.md).
   
## License
This repository is licensed under the GPL-2 License. 




 

