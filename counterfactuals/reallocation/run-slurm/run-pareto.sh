#!/bin/bash
#SBATCH -A pnumber               
#SBATCH -p short
#SBATCH -t 04:00:00           
#SBATCH -N 1                 
#SBATCH -n 1                
#SBATCH --mem=2GB          
#SBATCH --mail-user=qmail 
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --array=1-12%12
#SBATCH --error=arrayJob_%A_%a.err
#SBATCH --output=arrayJob_%A_%a.out
#SBATCH --job-name="reallocation-pareto"

module purge all
module load R/3.6.3

input_file=/home/quser/project_dir/urban/data/output/counterfactuals/reallocation/pareto_list_rho.csv

cd /home/quser/project_dir/urban/code/counterfactuals/reallocation/r-files/
Rscript reallocation_pareto.R $(sed -n ${SLURM_ARRAY_TASK_ID}p $input_file | sed 's/,/ /g')
