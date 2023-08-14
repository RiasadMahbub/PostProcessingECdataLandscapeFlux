#!/bin/bash

#SBATCH --job-name=LEfixRun1
#SBATCH --output=EfixRun1output.slurm
#SBATCH --partition agpu06
#SBATCH --nodes=1
#SBATCH --tasks-per-node=64
#SBATCH --time=6:00:00

cd $SLURM_SUBMIT_DIR

module load gcc/8.3.1 mkl/19.0.5 python/3.11-anaconda
. /share/apps/bin/conda-3.11.sh
conda activate pandas-3.11

# Run the Python script
python LEfixWay3Ratioslurm.py