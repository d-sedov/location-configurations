#!/bin/bash
#SBATCH -A pnumber             ## account (unchanged)
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH -n 4                 ## number of cores
#SBATCH --mem=32GB               # Memory per node in GB needed for a job
#SBATCH --mail-user=qmail # Designate email address for job communications
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="logit-cross-section"       # Name of job#

module purge all
module load stata/15

echo 'CDing to code directory...'
cd /home/quser/project_dir/urban/code/spatial-demand/restaurants-direct/estimation/do-log-files

echo 'Running Stata...'
stata-mp -b do 2-logit-cross-section-test.do 
