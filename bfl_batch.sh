#!/bin/bash --login
###
# project code
#SBATCH --account=scw1000
#job name
#SBATCH --job-name=bflFR
#job stdout file
#SBATCH --output=bflFR.out.%J
#job stderr file
#SBATCH --error=bflFR.err.%J
#maximum job time in D-HH:MM
#SBATCH --time=3-00:00
#number of parallel processes (tasks) you are requesting - maps to MPI processes
#SBATCH --ntasks=40
#memory per process in MB 
#SBATCH --mem-per-cpu=16000 
#tasks to run per node (change for hybrid OpenMP/MPI) 
###

#now run normal batch commands 
module load R/3.5.3 compiler/intel/2018/2 udunits gdal proj/4.9.3 szip geos
export CXX='icpc -std=c++11' UDUNITS2_LIBS=/apps/libraries/udunits/2.2.26/el7/AVX512/intel-2018/lib R_HOME=/apps/languages/R/3.5.3/el7/AVX512/intel-2018/lib64/R R_LIBS_USER=/scratch/$USER/R_sf

# required for compilation of lwgeom
#export LIBRARY_PATH=/apps/libraries/geos/3.7.0/el7/AVX512/intel-2018/lib


time Rscript bfl_sunbird.R FR_five_basins.shp FR_five_basins_barriers.shp 2192 $SLURM_NTASKS
