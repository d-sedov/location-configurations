#!/bin/bash
#SBATCH -A pnumber             
#SBATCH -p normal                
#SBATCH -t 12:00:00             
#SBATCH -N 1                  
#SBATCH -n 1                
#SBATCH --mem=16GB        
#SBATCH --mail-user=qmail
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --array=1-12%12
#SBATCH --error=arrayJob_%A_%a.err
#SBATCH --output=arrayJob_%A_%a.out
#SBATCH --job-name="minimization-large"     

module purge all
module load R/3.6.3

input_file=/home/quser/project_dir/urban/data/output/spatial-demand/main_demand/large_cbsa_list.csv

cd /home/quser/project_dir/urban/code/spatial-demand/main_demand/estimation/r-files/
Rscript estimate_deltas_rho.R $(sed -n ${SLURM_ARRAY_TASK_ID}p $input_file)
