#!/bin/bash
################################################################################
# Script Name: ELMFIRE Runner
# 
# Description: 
# This script sets up, executes, and post-processes ELMFIRE simulations for 
# various input configurations. ELMFIRE is a hypothetical wildfire modeling tool.
# 
# Dependencies:
# - External file: ../functions/functions.sh must exist and be source-able.
# - External file: run_elmfire.sh must exist and be executable.
# - External Python script (optional): plot_generator.py
# - The following directories and files should be present:
#   ./inputs, ./outputs, ./misc, ./toa, ./fuel_models.csv, ./configs.csv
# 
# Variables:
# - DOMAINSIZE: Specifies the height and width of the domain in meters.
# - SIMULATION_TSTOP: Specifies the simulation stop time in seconds.
# - SCRATCH, INPUTS, OUTPUTS, MISC, TOA: Specify paths for temporary and 
#   output data.
# - ELMFIRE_VER: Denotes the version of the ELMFIRE tool being used. 
#   Defaults to '2023.06' if not set externally.
# - NUM_CPUS: Specifies the number of CPUs to be used for parallel 
#   computation. Defaults to 36.
# - LHS: Specifies the file containing input configurations to be used 
#   for simulations.
# 
# Functions:
# - process_line(): Processes each row (line) of the input configuration file 
#   and invokes the ELMFIRE run script with the required parameters.
# 
# Outputs:
# - Generates output in the ./outputs directory.
# - (Optional) Generates plots via an external Python script, plot_generator.py,
#   using data from the TOA directory and saves them to the outputs directory.
# 
# Usage:
# - Ensure all dependent files and directories are in place.
# - Modify the values of the configurable variables as needed.
# - Execute the script: ./<script_name>.sh
# 
# Note: 
# - GNU parallel can be used to run simulations in parallel, but the 
#   provided script contains commented-out parallel execution code for 
#   demonstration and a sequential execution alternative. To use parallel 
#   execution, uncomment the relevant lines and comment out the sequential 
#   execution lines.
# - Python plotting is also commented out. Uncomment if the plot_generator.py 
#   script is available and set up.
# 
# Created on: [8/15/23]
# Last modified: [Last Modified Date]
# Author: [Mason Lee] (adapted from Chris Lautenburger)
# Organization: [NASA GISS]
# Version: 1.0
################################################################################
# Source .bashrc to load environment variables
source ~/.bashrc

# Check if ELMFIRE_BASE_DIR is set
if [ -z "$ELMFIRE_BASE_DIR" ]; then
  echo "ELMFIRE_BASE_DIR is not set. Please set it in your .bashrc file."
  exit 1
else
  echo "ELMFIRE_BASE_DIR is: $ELMFIRE_BASE_DIR"
fi

# Set the path to run_elmfire.sh
RUN_ELMFIRE_SCRIPT="$ELMFIRE_BASE_DIR/tutorials/mc_ds/run_elmfire.sh"
echo "Expected location of run_elmfire.sh: $RUN_ELMFIRE_SCRIPT"

# Check if the script exists
if [ ! -f "$RUN_ELMFIRE_SCRIPT" ]; then
  echo "run_elmfire.sh not found at the expected location: $RUN_ELMFIRE_SCRIPT"
  exit 1
fi

# Export RUN_ELMFIRE_SCRIPT so it's available in process_line
export RUN_ELMFIRE_SCRIPT

#CELLSIZE=30.0 # Grid size in meters
DOMAINSIZE=12000.0 # Height and width of domain in meters
SIMULATION_TSTOP=347400.0 # Simulation stop time (seconds)

. ../functions/functions.sh

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs
MISC=./misc
TOA=./toa

ELMFIRE_VER=${ELMFIRE_VER:-2023.06}

rm -f -r $SCRATCH $OUTPUTS $TOA
mkdir $SCRATCH $OUTPUTS $TOA

################################################################33
process_line() {
  row="$1"
  RUN_NUMBER_POS=-1

  IFS=',' read -ra PARAM_VALUES <<< "$row"
  RUN_NUMBER=${PARAM_VALUES[$RUN_NUMBER_POS]}

  #Set the sampling configuration to identify runs
  SCONFIG='sobol-large'

  echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  echo "RUN NUMBER: $RUN_NUMBER"
  echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

  #If this is define other than . you need to export the path to PATH
  echo "deploying Using Script"
  # dos2unix $ELMFIRE_BASE_DIR/run_elmfire.sh 
  # $ELMFIRE_BASE_DIR/run_elmfire.sh $SCONFIG "${PARAM_VALUES[@]}" 
  
  # Ensure the script is executable
  if [ -f "$RUN_ELMFIRE_SCRIPT" ]; then
      echo "Expected location of run_elmfire.sh: $RUN_ELMFIRE_SCRIPT"
      chmod +x "$RUN_ELMFIRE_SCRIPT"
  else
      echo "Error: run_elmfire.sh not found at $RUN_ELMFIRE_SCRIPT"
  fi

  # Now run the script
  $RUN_ELMFIRE_SCRIPT "$SCONFIG" "${PARAM_VALUES[@]}"


  #echo "Running Using Slurm"
  #sbatch ./run_elmfire.sh $INPUTS $RUN_NUMBER $CROWN_FIRE_MODEL "${PARAM_VALUES[@]}"  ## Make sure to uncomment the run_elmfire.sh sbatch configs at the top 
}
## end of function
################################################################

## Export the function
export -f process_line

# Find the number of CPUs
##NUM_CPUS=$(lscpu -p | egrep -v '^#' | wc -l)
NUM_CPUS=20
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
echo "Number of CPU No Par:: ${NUM_CPUS} :: Running LHS"
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

### Files to Be Uploaded to Run this
##  ./fuel_models.csv 
##  ./run_elmfire.sh
##  ./large_lhs.csv
#############################################################

# Define paths and files
# The Debug has to do with the spatial resolution of the file.
#LHS=./large_lhs_debug.csv
#LHS=./large_lhs.csv
#LHS=./sobol_seq_inputs.csv
LHS=./rem_sobol_runs.csv
#LHS=./rem_sobol_runs.csv
#LHS=./baseline_runs.csv

echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
echo "Running LHS: $LHS"
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

# Use GNU parallel to run the jobs in parallel
echo 'running ELMFIRE using GNU parallel '
tail -n +2 "$LHS" | parallel -j "$NUM_CPUS" process_line

# One line experimental debugging run
#sed -n 2p "$LHS" | while read -r line; do process_line "$line"; done

###################
## Same thing as above but no parallelizaiton
#echo "running ELMFIRE sequentially"
# while IFS= read -r line
# do
#     echo "Read line: $line"  # debug line
#     process_line "$line"
# done < <(tail -n +2 "$LHS")

###################

echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
echo "Finshed Runing LHS"
echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

############################################################
directory=$TOA
background_img_path="./inputs/asp.tif"
saved_directory="./outputs"




# call Python script
echo "Generating Plot"
#python3 plot_generator.py $directory $background_img_path $saved_directory
############################################################
exit 0
