#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=10:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cf_ten_amz

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 4_counterfactual_ten.R 4 amz fac itpa
Rscript 4_counterfactual_ten.R 4 amz fac it
Rscript 4_counterfactual_ten.R 4 amz fac pa
Rscript 4_counterfactual_ten.R 4 amz cf1 itpa
Rscript 4_counterfactual_ten.R 4 amz cf1 it
Rscript 4_counterfactual_ten.R 4 amz cf1 pa

