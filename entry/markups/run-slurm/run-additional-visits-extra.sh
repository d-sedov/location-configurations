#!/bin/bash
#SBATCH -A pnumber               
#SBATCH -p normal
#SBATCH -t 48:00:00           
#SBATCH -N 1                 
#SBATCH -n 1                
#SBATCH --mem=32GB          
#SBATCH --mail-user=qmail 
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --array=1-24%24
#SBATCH --error=arrayJob_%A_%a.err
#SBATCH --output=arrayJob_%A_%a.out
#SBATCH --job-name="additional-extra"

module purge all
module load R/3.6.3

input_file=/home/quser/project_dir/urban/data/output/entry/markups/markup_list_extra.csv

cd /home/quser/project_dir/urban/code/entry/markups/r-files/
Rscript visits_area.R $(sed -n ${SLURM_ARRAY_TASK_ID}p $input_file | sed 's/,/ /g')
