module use /tools/acfl/modulefiles
module use /tools/Libs/modulefiles

module purge
module load acfl/23.10
module load openmpi/acfl/4.1.6
module load armpl/23.10.0
module list
echo "Done loading environment."