#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=8192
#SBATCH --job-name=gen
#SBATCH --error=./slurm/job_%J.err
#SBATCH --output=./slurm/job_%J.out

module load R
Rscript 2-generate.R 2500
