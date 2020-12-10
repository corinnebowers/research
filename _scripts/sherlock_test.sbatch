#!/bin/bash
#SBATCH --job-name=test
#SBATCH --mail-type=ALL
#SBATCH --mail-user=<cbowers>@stanford.edu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=1GB
#SBATCH --time=1:00
#SBATCH --output=sherlock_test.log

# --ntasks-per-node will be used in UQ_test.R to specify the number
# of cores to use on the machine.

# load modules
ml R/3.6.1

# execute script
Rscript sherlock_test.R