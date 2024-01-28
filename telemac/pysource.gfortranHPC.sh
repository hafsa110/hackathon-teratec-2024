# This file is a template for a Linux environment file
# running "source pysource.template.sh" will position all
# the necessary environment variables for telemac
# To adapt to your installation replace word <word> by their local value
###
### TELEMAC settings -----------------------------------------------------------
###
source vars.sh

# Path to telemac root dir
export HOMETEL=$TELDIR
# Adding python scripts to PATH
export PATH=$HOMETEL/scripts/python3:.:$PATH
# Configuration file
export SYSTELCFG=$HOMETEL/configs/systel.cfg
# Name of the configuration to use
export USETELCFG=gfortranHPC
# Path to this file
export SOURCEFILE=$HOMETEL/../pysource.gfortranHPC.sh
### Python
# To force python to flush its output
export PYTHONUNBUFFERED='true'
### API
export PYTHONPATH=$HOMETEL/scripts/python3:$PYTHONPATH
export LD_LIBRARY_PATH=$HOMETEL/builds/$USETELCFG/lib:$HOMETEL/builds/$USETELCFG/wrap_api/lib:$LD_LIBRARY_PATH
export PYTHONPATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$PYTHONPATH
###
### EXTERNAL LIBRARIES -----------------------------------------------------------
###
### METIS -------------------------------------------------------------
export METISHOME=$METISDIR
export LD_LIBRARY_PATH=$METISHOME/lib:$LD_LIBRARY_PATH
### HDF5 -------------------------------------------------------------
export HDF5HOME=$HDF5DIR
export LD_LIBRARY_PATH=$HDF5HOME/lib:$LD_LIBRARY_PATH
### MED -------------------------------------------------------------
export MEDHOME=$MEDDIR
export LD_LIBRARY_PATH=$MEDHOME/lib:$LD_LIBRARY_PATH
export PATH=$MEDHOME/bin:$PATH
### AED -------------------------------------------------------------
export AEDHOME=$AEDDIR
export LD_LIBRARY_PATH=$AEDHOME/lib:$LD_LIBRARY_PATH
export PATH=$AEDHOME/bin:$PATH
