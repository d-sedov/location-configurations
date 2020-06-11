#!/bin/bash
#SBATCH -A pnumber               
#SBATCH -p short               
#SBATCH -t 00:30:00           
#SBATCH -N 1                 
#SBATCH -n 1                
#SBATCH --mem=32GB          
#SBATCH --mail-user=qmail 
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --array=1-35%35
#SBATCH --error=arrayJob_%A_%a.err
#SBATCH --output=arrayJob_%A_%a.out
#SBATCH --job-name="additional-large"

module purge all
module load R/3.6.3

input_file=/home/quser/project_dir/urban/data/output/entry/markups/markup_list_large.csv

cd /home/quser/project_dir/urban/code/entry/markups/r-files/
Rscript visits_area.R $(sed -n ${SLURM_ARRAY_TASK_ID}p $input_file | sed 's/,/ /g')
