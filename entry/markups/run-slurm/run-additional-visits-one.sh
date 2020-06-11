#!/bin/bash
#SBATCH -A pnumber               
#SBATCH -p short
#SBATCH -t 01:00:00           
#SBATCH -N 1                 
#SBATCH -n 1                
#SBATCH --mem=64GB          
#SBATCH --mail-user=qmail 
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="additional-one"

module purge all
module load R/3.6.3

cd /home/quser/project_dir/urban/code/entry/markups/r-files/
Rscript visits_area.R 40380 -0.143054401976553 0.506527882787679
