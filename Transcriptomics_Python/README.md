# Transcriptomics_Python
Differential Gene Expression Analysis of Circadian Clock Genes in the Suprachiasmatic Nucleus in Short Day Night Cycle Mice (22 hour days) and Normal Day Night Cycle Mice (24 hour days)


<br />

Bioinformatics Pipeline for generating quant.sf files is as follows: <br />

Download raw SRA files of "Circadian behavior is light-reprogrammed by plastic DNA methylation (sequencing)" study from NCBI SRA archive. <br />
Convert to Fastq files with fastq clipper. <br />
Use Salmon's paired-end clipping tool to clip low-confidence reads and possible adapters. <br />
Generate index file for mm38 transcriptome downloaded from Ensemble. <br />
Align and quantify raw sequence data with Salmon's Quant function. <br />
