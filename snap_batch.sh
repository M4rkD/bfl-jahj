#!/bin/bash --login
###
# project code
#SBATCH --account=scw1433
#job name
#SBATCH --job-name=snap
#job stdout file
#SBATCH --output=snap.out.%J
#job stderr file
#SBATCH --error=snap.err.%J
#maximum job time in D-HH:MM
#SBATCH --time=0-00:05
#number of parallel processes (tasks) you are requesting - maps to MPI processes
#SBATCH --ntasks=2
#memory per process in MB 
#SBATCH --mem-per-cpu=800
#tasks to run per node (change for hybrid OpenMP/MPI) 
###

#now run normal batch commands 
module load R/3.5.3 compiler/intel/2018/2 udunits gdal proj/4.9.3 szip
export CXX='icpc -std=c++11' UDUNITS2_LIBS=/apps/libraries/udunits/2.2.26/el7/AVX512/intel-2018/lib R_HOME=/apps/languages/R/3.5.3/el7/AVX512/intel-2018/lib64/R R_LIBS_USER=/scratch/s.j.a.h.jones/R_sf

time Rscript snap_parallel.R
