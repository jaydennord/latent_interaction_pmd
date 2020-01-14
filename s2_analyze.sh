#!/bin/bash
#SBATCH --array=0-749
#SBATCH --time=48:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16384
#SBATCH --job-name=ana
#SBATCH --error=./slurm/job_%A_%a.err
#SBATCH --output=./slurm/job_%A_%a.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jaydennord@gmail.com

chmod +x ./slurm/slurm$SLURM_ARRAY_TASK_ID
./slurm/slurm$SLURM_ARRAY_TASK_ID

module load R
Rscript 3-collect3.R $SLURM_ARRAY_TASK_ID

#sed -e 's/~\/mplus/rm/g' -e 's/\.inp/\.*/g' -i ./slurm/slurm$SLURM_ARRAY_TASK_ID
#./slurm/slurm$SLURM_ARRAY_TASK_ID

