# SCN-Per1-TS-Analysis
Time series and differential gene expression analysis of Per1 gene expression in the Suprachiasmatic Nucleus of Mice brains

## Time series Analysis 

Download raw SCN images from "Circadian Rhythm in the Brain" kaggle dataset
Isolate SCN Venus fluorophore expression loci.

Fit Regression line -> Fit ARIMA model on residuals -> Fit Neural Net on residuals

## Differential Gene Expression

Analysis of Circadian Clock Genes in the Suprachiasmatic Nucleus in Short Day Night Cycle Mice (22 hour days) and Normal Day Night Cycle Mice (24 hour days)

Bioinformatics Pipeline for generating quant.sf files is as follows:

Download raw SRA files of "Circadian behavior is light-reprogrammed by plastic DNA methylation (sequencing)" study from NCBI SRA archive.
Convert to Fastq files with fastq clipper.
Use Salmon's paired-end clipping tool to clip low-confidence reads and possible adapters.
Generate index file for mm38 transcriptome downloaded from Ensemble.
Align and quantify raw sequence data with Salmon's Quant function.
