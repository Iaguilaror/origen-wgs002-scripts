#!/bin/bash

# RHD 5' coordinate: 25302393
# RHD 3' coordinate: 25360445
# sample example url: https://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241661/HG00380.final.cram

start="$1"
end="$2"
url="$3"
marker="$4"

samtools view -b \
  -T https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/reference/GRCh38_reference_genome/GRCh38_full_analysis_set_plus_decoy_hla.fa \
  "$url" \
  "chr1:"$start"-"$end > results/"$(basename $url .final.cram)"_subset_RHD.bam

samtools index results/"$(basename $url .final.cram)"_subset_RHD.bam

rm *.crai

Rscript --vanilla plot_cov.R results/"$(basename $url .final.cram)"_subset_RHD.bam $marker
