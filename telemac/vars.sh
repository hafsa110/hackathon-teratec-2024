# Top-level dirs
export BUILDDIR=$(pwd)

# Load environnement
#source env.sh

# Build variables
export CC=gcc
export CXX=g++
export FC=gfortran
export N=8

export METISDIR=$BUILDDIR/METIS
export HDF5DIR=$BUILDDIR/HDF5
export MEDDIR=$BUILDDIR/MED
export AEDDIR=$BUILDDIR/AED
export GOTMDIR=$BUILDDIR/GOTM

export TELDIR=$BUILDDIR/telemac-mascaret-v8p5r0
