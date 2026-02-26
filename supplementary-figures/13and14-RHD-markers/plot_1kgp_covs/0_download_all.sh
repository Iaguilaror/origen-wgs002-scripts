#!/bin/bash

# coordinates to check:
# RHD coords
# 1:25272393-25330445
# - 30k, + 30k para checar

#############################################################################################################
# Marker 1_25235176_G_A ##############################################################################

# marker HOM gt2
# DEL HOM 0
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242593/HG03291.final.cram
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240199/HG00126.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243023/HG02774.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242767/HG00742.final.cram

# marker HET gt1
# DEL HET 1
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242235/HG01894.final.cram
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240114/HG00096.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243010/HG02494.final.cram
# EAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239497/NA18546.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241759/HG00740.final.cram

# marker HOM gt0
# DEL HOM >1
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242293/HG01880.final.cram
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240127/HG00111.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242905/HG03949.final.cram
# EAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241732/HG00653.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241728/HG00640.final.cram

rm -rf results
mkdir -p results

# prepare all samples
allsamples="ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242593/HG03291.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240199/HG00126.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243023/HG02774.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242767/HG00742.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242235/HG01894.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240114/HG00096.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243010/HG02494.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239497/NA18546.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241759/HG00740.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242293/HG01880.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240127/HG00111.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242905/HG03949.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241732/HG00653.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241728/HG00640.final.cram"

for ibam in $allsamples
do
	bash download_slice.sh \
		25242393 \
		25390445 \
		$ibam \
		1_25235176_G_A
done

#############################################################################################################
# 1_25257119_C_G ##############################################################################

# marker HOM gt2
# DEL HOM 0
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240096/NA19923.final.cram
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240132/HG00116.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242406/HG02658.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241757/HG00737.final.cram

# marker HET gt1
# DEL HET 1
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242732/HG02455.final.cram 
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240124/HG00108.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243039/HG02597.final.cram
# EAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239497/NA18546.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241760/HG01047.final.cram

# marker HOM gt0
# DEL HOM >1
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242191/HG01885.final.cram
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240180/HG00238.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243136/HG04212.final.cram
# EAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239417/NA18975.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242768/HG00743.final.cram

# prepare all samples
allsamples="ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240096/NA19923.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240132/HG00116.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242406/HG02658.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241757/HG00737.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242732/HG02455.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240124/HG00108.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243039/HG02597.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239497/NA18546.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241760/HG01047.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242191/HG01885.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240180/HG00238.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3243136/HG04212.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239417/NA18975.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242768/HG00743.final.cram"

for ibam in $allsamples
do
        bash download_slice.sh \
                25242393 \
                25390445 \
                $ibam \
                1_25257119_C_G
done

#############################################################################################################
# 1_25409878_C_T ##############################################################################

# marker HOM gt2
# DEL HOM 0
# AFR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240096/NA19923.final.cram
# EUR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240132/HG00116.final.cram
# SAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242405/HG02657.final.cram
# EAS ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239527/NA18631.final.cram
# AMR ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241757/HG00737.final.cram

# marker HET gt1
# DEL HET 1
# AFR
# EUR
# SAS
# EAS
# AMR

# marker HOM gt0
# DEL HOM >1
# AFR
# EUR
# SAS
# EAS
# AMR

# prepare all samples
allsamples="ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240096/NA19923.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3240132/HG00116.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3242405/HG02657.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR323/ERR3239527/NA18631.final.cram 
ftp://ftp.sra.ebi.ac.uk/vol1/run/ERR324/ERR3241757/HG00737.final.cram"

for ibam in $allsamples
do
        bash download_slice.sh \
                25242393 \
                25390445 \
                $ibam \
                1_25409878_C_T
done
