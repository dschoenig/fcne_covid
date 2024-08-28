#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=48G
#SBATCH --time=8:00:00
#SBATCH --array=1-50
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=padegcf1

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

Rscript 4_evaluate_predictions.R AMZ deg cf1 $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT
