#!/bin/bash
#SBATCH -A pnumber             ## account (unchanged)
#SBATCH -p normal                # Queue
#SBATCH -t 48:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH -n 1                 ## number of cores
#SBATCH --mem=32GB               # Memory per node in GB needed for a job
#SBATCH --mail-user=qmail # Designate email address for job communications
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="minimization"       # Name of job#

module purge all
module load R/3.6.3

cd /home/quser/project_dir/urban/code/spatial-demand/main_demand/estimation/r-files/
Rscript estimate_deltas_rho.R 12060
