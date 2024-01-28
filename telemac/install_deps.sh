#!/usr/bin/bash

# Top-level dirs
export ROOTDIR=$(pwd)
export ARCHDIR=$ROOTDIR/archives
export BUILDDIR=$ROOTDIR/build

# Purge & setup build dir
rm -rf $BUILDDIR
mkdir -p $BUILDDIR

cp env.sh $BUILDDIR
cp vars.sh $BUILDDIR

cd $BUILDDIR

# Load environnement
source $BUILDDIR/env.sh

# Install Python libs
pip3 install -r ./requirements.txt

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

# Compile METIS
mkdir -p $METISDIR
cd $METISDIR
tar xvf $ARCHDIR/metis-5.1.0.tar.gz
cd metis-5.1.0
#cmake -D CMAKE_INSTALL_PREFIX=$METISDIR .
make config prefix=$METISDIR cc=$CC
make -j $N
make install
cd $BUILDDIR

# Compile HDF5
mkdir -p $HDF5DIR
cd $HDF5DIR
tar xvf $ARCHDIR/hdf5-1.10.3.tar.gz
cd hdf5-1.10.3
./configure --prefix=$HDF5DIR
make -j $N
make install
cd $BUILDDIR

# Compile MED
mkdir -p $MEDDIR
cd $MEDDIR
tar xvf $ARCHDIR/med-4.1.1.tar.gz
cd med-4.1.1
./configure CFLAGS=-O2 CXXFLAGS=-O2 FFLAGS=-O2 --disable-python --with-hdf5=$HDF5DIR --prefix=$MEDDIR
make -j $N
make install
cd $BUILDDIR

# Compile AED
mkdir -p $AEDDIR
cd $AEDDIR
tar xvf $ARCHDIR/aed2_1.2.tgz
cd aed2_1.2/
make shared
cp -r ./include/ ..
mkdir -p ../lib
cp libaed2.so ../lib/
cp *.mod ../include/
cd $BUILDDIR

# Compile telemac
# Consult README