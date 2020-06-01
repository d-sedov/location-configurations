#!/bin/bash
#SBATCH -A pnumber               
#SBATCH -p short
#SBATCH -t 02:30:00           
#SBATCH -N 1                 
#SBATCH -n 1                
#SBATCH --mem=56GB          
#SBATCH --mail-user=qmail 
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="additional-one"

module purge all
module load R/3.6.3

cd /home/quser/project_dir/urban/code/entry/markups/r-files/
Rscript visits_area_special.R 16980 -0.263392546198898 0.001935573144382255 99
