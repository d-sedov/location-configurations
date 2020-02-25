#!/bin/bash
#SBATCH -A pnumber             ## account (unchanged)
#SBATCH -p short                # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=8G               # Memory per node in GB needed for a job
#SBATCH --mail-user=qmail # Designate email address for job communications
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="regressions"       # Name of job#

module purge all
module load stata/15

cd /home/quser/project_dir/urban/code/reduced-form

stata -b do 

