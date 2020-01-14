#!/bin/bash
#SBATCH --array=0-749
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16384
#SBATCH --job-name=col
#SBATCH --error=./slurm/job_%J.err
#SBATCH --output=./slurm/job_%J.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jaydennord@gmail.com

module load R
Rscript 3-collect.R $SLURM_ARRAY_TASK_ID
