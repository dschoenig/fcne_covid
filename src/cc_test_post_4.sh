#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=48G
#SBATCH --time=8:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=tp4

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

Rscript test_post.R amz 4 dis fac
