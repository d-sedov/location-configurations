#!/bin/bash
#SBATCH -A pnumber               
#SBATCH -p normal
#SBATCH -t 08:00:00           
#SBATCH -N 1                 
#SBATCH -n 1                
#SBATCH --mem=2GB          
#SBATCH --mail-user=qmail 
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --array=1-22%22
#SBATCH --error=arrayJob_%A_%a.err
#SBATCH --output=arrayJob_%A_%a.out
#SBATCH --job-name="add-average"

module purge all
module load R/3.6.3

input_file=/home/quser/project_dir/urban/data/output/spatial-demand/main_demand/medium_cbsa_list.csv

cd /home/quser/project_dir/urban/code/counterfactuals/number/r-files/
Rscript add_average_counterfactual.R $(sed -n ${SLURM_ARRAY_TASK_ID}p $input_file | sed 's/,/ /g')
