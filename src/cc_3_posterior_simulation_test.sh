#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=36G
#SBATCH --time=01:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=AMZ_post_1

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
# Rscript 3_posterior_simulation_test.R AMZ dis 1 $SLURM_CPUS_PER_TASK
# Rscript 3_posterior_simulation_test.R AMZ dis 2 $SLURM_CPUS_PER_TASK
# Rscript 3_posterior_simulation_test.R AMZ dis 3 $SLURM_CPUS_PER_TASK
# Rscript 3_posterior_simulation_test.R AMZ dis 4 $SLURM_CPUS_PER_TASK
# Rscript 3_posterior_simulation_test.R AMZ dis 5 $SLURM_CPUS_PER_TASK
Rscript 3_posterior_simulation_test.R AMZ dis 8 $SLURM_CPUS_PER_TASK

