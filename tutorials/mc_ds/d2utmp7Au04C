#!/bin/bash

## SBATCH --job-name=elmfire_run
## SBATCH --output=elmfire_run_%j.out
## SBATCH --ntasks=1
## SBATCH --cpus-per-task=64
## SBATCH --time=01:00:00
## SBATCH --mem-per-cpu=4G

################################################
### RUN_ELMFIRE.sh function header:


: <<COMMENT 
   Here is a detailed description of the wildfire simulation script:

    Overview:
    This bash script runs the wildfire simulation model ELMFIRE to generate spatially explicit
    wildfire spread simulations and outputs. It takes a set of input parameters and data,
    configures and executes the ELMFIRE model, and processes the outputs.

    Inputs:
    - Input directory (INPUTS): This contains all the input data needed to run ELMFIRE,
      including terrain/fuels maps, weather data, ignition location etc.
    
    - Run number (RUN_NUMBER): A unique identifier for each run.
   
   
    Hardcoded Parameters (manually change):
      - Domain size (DOMAINSIZE): Sets the size of the computational domain.
      - Simulation duration (seconds) (SIMULATION_TSTOP): Controls the length of the simulation.

   ###############

   The script also takes in arrays of parameter values that can be used to run Monte Carlo
   simulations sampling across a range of values.

    
      In your script RUN_ELMFIRE.sh, data is taken from a CSV file using the PARAM_VALUES array and 
      positions defined for specific parameters. Let me explain how this works step by step:

      Parameter Values in CSV:
      The CSV file contains rows of parameter values that correspond to different simulation runs. 
      Each row represents a set of input values for running the wildfire simulation. These parameter 
      values determine how the simulation is configured, including factors like wind speed, crown 
      fire model, spotting parameters, and more.

      Defining Positions:
      To extract these parameter values, you use an array called PARAM_VALUES. In this array, 
      each element corresponds to a parameter value from a specific column in the CSV file. The 
      positions of these elements in the array are important because they determine which 
      parameter value from the CSV corresponds to each position. For example, if the first
      element of PARAM_VALUES corresponds to the wind speed, the second to the wind direction, 
      and so on, you need to define these positions correctly.

      #IMPORTANT TO NOTE THAT THE CSV CONFIGURATION CAN BE CHANGED BUT THE INDEXING MUCH CHANGE ACCORDINLY

      The spotting configurations are the first parameters of the CSV:
      {pos: 1, param: CROWN_FIRE_SPOTTING_PERCENT_MIN}
      {pos: 2, param: CROWN_FIRE_SPOTTING_PERCENT_MAX}
      {pos: 3, param: ENABLE_SURFACE_FIRE_SPOTTING}
      {pos: 4, param: GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN}
      {pos: 5, param: GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX}
      {pos: 6, param: CRITICAL_SPOTTING_FIRELINE_INTENSITY}
      {pos: 7, param: SPOTTING_DISTRIBUTION_TYPE}
      {pos: 8, param: MEAN_SPOTTING_DIST_MIN}
      {pos: 9, param: MEAN_SPOTTING_DIST_MAX}
      {pos: 10, param: NORMALIZED_SPOTTING_DIST_VARIANCE_MIN}
      {pos: 11, param: NORMALIZED_SPOTTING_DIST_VARIANCE_MAX}
      {pos: 12, param: SPOT_WS_EXP_LO}
      {pos: 13, param: SPOT_WS_EXP_HI}
      {pos: 14, param: SPOT_FLIN_EXP_LO}
      {pos: 15, param: SPOT_FLIN_EXP_HI}
      {pos: 16, param: NEMBERS_MIN}
      {pos: 17, param: NEMBERS_MAX_LO}
      {pos: 18, param: NEMBERS_MAX_HI}
      {pos: 19, param: PIGN_MIN}
      {pos: 20, param: PIGN_MAX}

      ### End of Spotting Configurations
      {pos: 21, param: CROWN_FIRE_MODEL}
      {pos: 22, param: INPUT_DIR}
      {pos: 23, param: WIND_SPEED}
      {pos: 24, param: WIND_DIRECTION}
      {pos: 25, param: CC}
      {pos: 26, param: CBH}
      {pos: 27, param: CBD}
      {pos: 28, param: CH}
      {pos: 29, param: M1}
      {pos: 30, param: M10}
      {pos: 31, param: M100}
      {pos: 32, param: Run Number}

      Here's how you define these positions:
         CROWN_FIRE_MODEL_POS=-12
         INPUT_DIR_POS=-11
         WIND_SPEED_POS=-10
         WIND_DIRECTION_POS=-9
         CC_POS=-8
         CBH_POS=-7
         CBD_POS=-6
         CH_POS=-5
         M1_POS=-4
         M10_POS=-3
         M100_POS=-2
         RUN_NUMBER_POS=-1 

    Workflow:
    - Set up directory structure and copy inputs
      - Create scratch, misc and output directories
      - Copy fuel model lookup table
      - Copy input data into scratch directory
    
    - Configure ELMFIRE input file
      - Set domain bounds, grid cell size, spatial reference etc based on input terrain data
      - Set the run-specific parameters like crown model, wind, moisture etc.
      - Set output directory to run-specific output dir
      - Set spotting parameters
    
    - Execute ELMFIRE model
      - Run ELMFIRE using MPI or single core
      - Run using Parallel Processing or Sequentially
      - Save input file copy to outputs
    
    - Process outputs
      - Convert native BIL outputs to GeoTIFF
      - Move outputs to run-specific output directory
      - Rename time of arrival outputs with run identifier
      ##
         Main Output Directory ($OUTPUTS):
            This directory contains subdirectories for each individual simulation run. Each subdirectory is named based on the specific parameters of that run. The main output directory might also contain other related information or scripts.

         Simulation Run Subdirectories ($OUTPUTS/<RUN_NAME>):
            Each simulation run is saved in its own subdirectory, where <RUN_NAME> is a name constructed based on the input parameters. These subdirectories contain various files and data generated during the simulation. The subdirectory structure might look like:
      
         outputs/
            ├── run_name_1/
               ├── coeffs.csv
               ├── crown_fire_0000001.tif
               ├── fire_size_stats.csv
               ├── flin_0000001_0347445.tif
               ├── run_name_1-elmfire.data
               ├── time_of_arrival_0000001_0347445.tif
               ├── timings_gs611-hammer.giss.nasa.gov.csv
               ├── toa_0001_0000001.bin
               └── vs_0000001_0347445.tif

            ....
            
            ├── run_name_n/

         
         ## The toa is folder is used in some post processing functions like
         ## IOU.py to calculate the burned areas
         toa/
          ├── run_name_1_time_of_arrival.tif
          ├── run_name_1-elmfire.data

          ...
          ├── run_name_n_time_of_arrival.tif
          ├── run_name_n-elmfire.data

            
      ##


    - Post-processing
      - Extract wall clock run time
      - Append to copied input file in outputs
    
    This allows running multiple simulations in sequence, configured with different parameters
    each time. The output data is organized by run name/number for easy comparison.
COMMENT


################################################
function echo_values {
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo 'Echo Values: ' 
   echo "Run: $1"    
   echo "Output Run: $2"  
   echo "Output : $3"    
   echo "MISC: $4"  
   echo "Scratch: $5"
   echo "Inputs: $6"

   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

   echo "***************************"
   echo "Parameters"
   #cat $5/elmfire.data
}

function process_output_files {
   # Postprocess
   for f in $1/*.bil; do
      gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f $1/`basename $f | cut -d. -f1`.tif
   done

   # Define array of file extensions
   file_extensions=("tif" "csv" "bin" "data" "bil" "hdr" )

   # Loop over each file extension and move the files from ./outputs to the current run's directory
   for ext in "${file_extensions[@]}"; do
      mv ./outputs/*.${ext} $2 2>/dev/null
   done

   # Get the absolute path of the TOA folder
   TOA=$(realpath $3)

   # Loop over each file in $OUTPUTS_RUN
   for file in $2/time_of_arrival_*.{tif,bil,hdr}; do
      # Extract the filename
      filename=$(basename -- "$file")
      # Rename and move the file
      mv "$file" "$TOA/${4}_$filename"
   done
}



function replace_line_fp {
   MATCH_PATTERN=$1
   NEW_VALUE="$2"
   FILE_PATH=$3
   IS_STRING=$4

   LINE=`grep -n "$MATCH_PATTERN" $FILE_PATH | cut -d: -f1`
   sed -i "$LINE d" $FILE_PATH
   if [ "$IS_STRING" = "yes" ]; then
      sed -i "$LINE i $MATCH_PATTERN = '$NEW_VALUE'" $FILE_PATH
   else
      sed -i "$LINE i $MATCH_PATTERN = $NEW_VALUE" $FILE_PATH
   fi
}

function delete_line_fp {
   MATCH_PATTERN=$1
   FILE_PATH=$2

   LINE=`grep -n "$MATCH_PATTERN" $FILE_PATH | cut -d: -f1`
   sed -i "$LINE d" $FILE_PATH
}

function add_line_fp {
    # Define inputs
    local FILE_PATH=$1
    local MATCH_PATTERN=$2
    local NEW_VALUE=$3

    # Add the line to the bottom of the file
    echo "$MATCH_PATTERN = $NEW_VALUE" >> $FILE_PATH
}

function remove_MC_params {
   FILE_PATH=$1
   START_LINE=$(grep -n '^&MONTE_CARLO' $FILE_PATH | head -n 1 | cut -d: -f1)
   END_LINE=$(grep -n '^/' $FILE_PATH | awk -v START_LINE="$START_LINE" '$1 > START_LINE {print $1; exit}')

   if [ -z "$END_LINE" ]; then
     echo "Could not find the end of the MONTE_CARLO block"
     exit 1
   fi

   END_LINE=${END_LINE%:*}  # Remove everything after the last colon

   echo "START_LINE: $START_LINE"
   echo "END_LINE: $END_LINE"

   echo "Removing old MONTE_CARLO block from line $START_LINE to $END_LINE"
   sed -i "${START_LINE},${END_LINE}d" $FILE_PATH
   echo "MONTE_CARLO block removed successfully"
}

function add_MC_params {
   FILE_PATH=$1
   PARAMS=("${!2}")  # Use indirect parameter expansion to get array passed as argument
   TMP_FILE=$(mktemp)
   echo "Adding new MONTE_CARLO block to $FILE_PATH"

   # Find the line where TIME_CONTROL block ends
   INSERT_LINE=$(grep -n '^/' $FILE_PATH | awk -v START_LINE=$(grep -n '^&TIME_CONTROL' $FILE_PATH | head -n 1 | cut -d: -f1) '$1 > START_LINE {print $1; exit}')
   INSERT_LINE=${INSERT_LINE%:*}  # Remove everything after the last colon

   if [ -z "$INSERT_LINE" ]; then
     echo "Could not find the end of the TIME_CONTROL block"
     exit 1
   fi
   echo "Line:  $INSERT_LINE"

   # Prepare MONTE_CARLO block text
   MONTE_CARLO_BLOCK="&MONTE_CARLO"
   for PARAM in "${PARAMS[@]}"; do
      MONTE_CARLO_BLOCK+="\n$PARAM"
   done
   MONTE_CARLO_BLOCK+="\n/"

   # Insert new MONTE_CARLO block after TIME_CONTROL block
   sed "${INSERT_LINE}r /dev/stdin" $FILE_PATH < <(echo -e "$MONTE_CARLO_BLOCK") > "$TMP_FILE" && mv "$TMP_FILE" "$FILE_PATH"
   
   echo "MONTE_CARLO block added successfully"
}

function add_paran {
    PATTERN=$1
    FILE_PATH=$2

    sed -i -r "s/($PATTERN[[:space:]]*=[[:space:]]*)([^[:space:]]*)/\1'\2'/" $FILE_PATH
}

function process_wallclock {
    # Define inputs
    local INPUTS=$1
    local TOA=$2
    local OUTPUTS=$3
    local RUN_NAME=$4

    # Save the current working directory
    local curr_dir=$(pwd)

    # Copy the elmfire.data file to TOA folder and rename it
    cp $INPUTS/elmfire.data $TOA/$RUN_NAME.data

    # Change directory to the OUTPUTS/RUN_NAME
    cd $OUTPUTS/$RUN_NAME

    # Extract 'Wall clock time (s)' value from fire_size_stats.csv
    wall_clock_value=$(awk -F',' 'NR==2{print $6}' fire_size_stats.csv)

    # Print the wall clock time
    echo "Wall Clock Time: $wall_clock_value"

    # Navigate back to the original directory
    cd $curr_dir

    # Add the wall clock time to the bottom of the copied data file
    add_line_fp $TOA/$RUN_NAME.data "WALL_CLOCK" $wall_clock_value
}

. ../functions/functions.sh

## end of function definitions
################################################

SCONFIG=$1
shift 1
PARAM_VALUES=("$@")
echo "INPUT DIRECTORY ORIG: $INPUTS"

###############################

ELMFIRE_VER=${ELMFIRE_VER:-2023.06}
DOMAINSIZE=12000.0 # Height and width of domain in meters
SIMULATION_TSTOP=347400.0
OUTPUTS=./outputs
TOA=./toa
MISC=./misc
SPOTTING_PARAMS=("CROWN_FIRE_SPOTTING_PERCENT_MIN" "CROWN_FIRE_SPOTTING_PERCENT_MAX" "ENABLE_SURFACE_FIRE_SPOTTING" "GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN" "GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX" "CRITICAL_SPOTTING_FIRELINE_INTENSITY" "SPOTTING_DISTRIBUTION_TYPE" "MEAN_SPOTTING_DIST_MIN" "MEAN_SPOTTING_DIST_MAX" "NORMALIZED_SPOTTING_DIST_VARIANCE_MIN" "NORMALIZED_SPOTTING_DIST_VARIANCE_MAX" "SPOT_WS_EXP_LO" "SPOT_WS_EXP_HI" "SPOT_FLIN_EXP_LO" "SPOT_FLIN_EXP_HI" "NEMBERS_MIN" "NEMBERS_MAX_LO" "NEMBERS_MAX_HI" "PIGN_MIN" "PIGN_MAX")

################################
## Extract PARAM_VALUES

# Assuming PARAM_VALUES is a list containing the parameter values in the same order as the column list
# Define the positions with negative indices
CROWN_FIRE_MODEL_POS=-12
INPUT_DIR_POS=-11
WIND_SPEED_POS=-10
WIND_DIRECTION_POS=-9
CC_POS=-8
CBH_POS=-7
CBD_POS=-6
CH_POS=-5
M1_POS=-4
M10_POS=-3
M100_POS=-2
RUN_NUMBER_POS=-1 # Assuming RUN_NUMBER is at position 0

# Update and add new variables from PARAM_VALUES
input_dir=${PARAM_VALUES[$INPUT_DIR_POS]}
INPUTS=./$input_dir

WIND_SPEED=${PARAM_VALUES[$WIND_SPEED_POS]}
WIND_DIRECTION=${PARAM_VALUES[$WIND_DIRECTION_POS]}
CM=${PARAM_VALUES[$CROWN_FIRE_MODEL_POS]}
CC=${PARAM_VALUES[$CC_POS]}
CBH=${PARAM_VALUES[$CBH_POS]}
CBD=${PARAM_VALUES[$CBD_POS]}
CH=${PARAM_VALUES[$CH_POS]}
M1=${PARAM_VALUES[$M1_POS]}  # Updated M1 variable
M10=${PARAM_VALUES[$M10_POS]}  # Updated M10 variable
M100=${PARAM_VALUES[$M100_POS]}  # Updated M100 variable
RUN_NUMBER==${PARAM_VALUES[$RUN_NUMBER_POS]}
RUN_NUMBER="${RUN_NUMBER#=}"

# Echo the updated variables
echo "INPUTS: $INPUTS"
echo "WIND_SPEED: $WIND_SPEED"
echo "WIND_DIRECTION: $WIND_DIRECTION"
echo "CROWN_FIRE_MODEL: $CM"
echo "CC: $CC"
echo "CBH: $CBH"
echo "CBD: $CBD"
echo "CH: $CH"
echo "M1: $M1"
echo "M10: $M10"
echo "M100: $M100"
echo "RUN_NUMBER: $RUN_NUMBER"
echo "SAMPLING-CONFIGURATION: $SCONFIG"


RUN_NUMBER="$((10#${RUN_NUMBER}))"
SCRATCH="./scratch/$(echo $RUN_NUMBER | tr -dc '0-9')"
###############################################


# Check if $MISC and $SCRATCH directories exist before removing them
rm -f -r  $SCRATCH 
mkdir $SCRATCH $MISC

# Check if the directory is correctly named
dir_name=$(basename $SCRATCH)
if [[ $dir_name != $RUN_NUMBER ]]; then
    # Directory incorrectly named, rename it
    correct_dir_name=$(echo $dir_name | tr -dc '0-9')
    mv "./scratch/$dir_name" "./scratch/$correct_dir_name"
    SCRATCH="./scratch/$correct_dir_name"
fi

# Set CROWN_FIRE_MODEL with an input option or default to 2 if not provided
echo "Debug: CROWING = $CM , INPUTS=$INPUTS"

######################################################################
## Set Up Scratch and Input Directories

# Define the source and destination file paths
source_file="./fuel_models.csv"
destination_dir="./misc"

# Check if the 'fuel_models.csv' file exists in the destination directory
if [ ! -f "$destination_dir/fuel_models.csv" ]; then
    # If the file does not exist in the destination directory, copy it from the source
    cp "$source_file" "$destination_dir"
    echo "File 'fuel_models.csv' copied to './misc' directory."
else
    echo "File 'fuel_models.csv' already exists in './misc' directory."
fi

ls $INPUTS
chmod -R 777 ./scratch
chmod -R 777 $SCRATCH
chmod -R 777 $INPUTS

mkdir $SCRATCH/${input_dir}
cp -r "$INPUTS" "$SCRATCH"
INPUTS=$SCRATCH/${input_dir}

echo "Debug 1 : $INPUTS"

#setup_inputs $INPUTS
chmod +r ./misc/fuel_models.csv
chmod +r ./fuel_models.csv

#####################################################################################

# Copy Data File: Copies a file named "elmfire.data.in" to the directory specified by the environment variable $SCRATCH and renames it to "elmfire.data". The comment indicates that different files are created for each run to avoid conflicts when using shared files.
cp ./elmfire.data.in $SCRATCH/elmfire.data ## We need to create different files for each run because they are using shared files!!!!
#cat $SCRATCH/elmfire.data

# Replace Lines: Several lines in the copied "elmfire.data" file are replaced with new values using the replace_line_fp function. This function takes three arguments: the parameter name, the new value, and the file path. The comments indicate that some lines need to be replaced with values from different environment variables ("$INPUTS", "$CM", "$MISC", etc.).
replace_line_fp FUELS_AND_TOPOGRAPHY_DIRECTORY "$INPUTS" $SCRATCH/elmfire.data yes
replace_line_fp WEATHER_DIRECTORY "$INPUTS" $SCRATCH/elmfire.data yes
replace_line_fp CROWN_FIRE_MODEL $CM $SCRATCH/elmfire.data no 

### SEPERATE MEMORY FOR PROCESSES USING GNU PARALLEL (Use the Same Input File)
replace_line_fp MISCELLANEOUS_INPUTS_DIRECTORY  "$MISC" $SCRATCH/elmfire.data yes
echo "Debug 2 : $INPUTS"


SCRATCH_TEMP="${SCRATCH}"
replace_line_fp SCRATCH "$SCRATCH_TEMP" $SCRATCH/elmfire.data no
add_paran SCRATCH $SCRATCH/elmfire.data

echo 
python3 python_add_mc.py --file_path $SCRATCH/elmfire.data --CH $CH --CBD $CBD --CC $CC --CBH $CBH --WS $WIND_SPEED --WD $WIND_DIRECTION --M1 $M1 --M10 $M10 --M100 $M100
echo "Debug: after remove_MC_params"

echo "Setting up inputs in $INPUTS"   
# Calculate Bounds: Extracts spatial bounds (xmin, xmax, ymin, ymax) of a GeoTIFF file using gdalinfo and manipulates the output to store these values in the corresponding variables.
XMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`
XMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`

# Print bounds
echo "xmin: $XMIN"
echo "xmax: $XMAX" 
echo "ymin: $YMIN"
echo "ymax: $YMAX"

# Calculate Center: Calculates the center point (XCEN, YCEN) of the spatial bounds.
XCEN=`echo "0.5*($XMIN + $XMAX)" | bc`
YCEN=`echo "0.5*($YMIN + $YMAX)" | bc`  

# Get spatial reference system
A_SRS=`gdalsrsinfo $INPUTS/fbfm40.tif | grep PROJ.4 | cut -d: -f2 | xargs`

# Get Cell Size: Extracts the pixel size (CELLSIZE) of the GeoTIFF file.
CELLSIZE=`gdalinfo $INPUTS/fbfm40.tif | grep 'Pixel Size' | cut -d'(' -f2 | cut -d, -f1` # Grid size in meters

# Update Input File: Uses the replace_line function to update several lines in the input file with the calculated values (XCEN, YCEN, CELLSIZE, A_SRS, etc.).
replace_line COMPUTATIONAL_DOMAIN_XLLCORNER $XMIN no
replace_line COMPUTATIONAL_DOMAIN_YLLCORNER $YMIN no
replace_line COMPUTATIONAL_DOMAIN_CELLSIZE $CELLSIZE no
replace_line A_SRS "$A_SRS" yes
replace_line 'X_IGN(1)' $XCEN no
replace_line 'Y_IGN(1)' $YCEN no

TR="$CELLSIZE $CELLSIZE"
TE="$XMIN $YMIN $XMAX $YMAX"  

echo $XMIN $XMAX $YMIN $YMAX $XCEN $YCEN $CELLSIZE $A_SRS
echo "Debug: after setup_inputs"

# Set Run Name and Output Directory: Creates a run name and output directory based on various input parameters and environment variables. The mkdir -p command creates the output directory if it doesn't exist.
RUN_NUMBER=$(echo "$RUN_NUMBER" | tr -d '\r')
RUN_NAME="inputs=${input_dir}_WIND=${WIND_CONFIG}_SPOTTING=${ENABLE_SPOTTING}_S-CONFIG=${RUN_NUMBER}-${SCONFIG}_CROWN-FIRE-MODEL=${CM}"

echo "Debug: RUN_NAME= $RUN_NAME"
OUTPUTS_RUN="${OUTPUTS}/${RUN_NAME}"

echo "Debug: OUTPUTS_RUN=$OUTPUTS_RUN"
export $RUN_NAME
mkdir -p $OUTPUTS_RUN

echo_values $RUN_NAME $OUTPUTS_RUN $OUTPUTS $MISC $SCRATCH $INPUTS
echo "Debug: after echo_values"

replace_line_fp SIMULATION_TSTOP $SIMULATION_TSTOP $SCRATCH/elmfire.data no
replace_line_fp DTDUMP $SIMULATION_TSTOP $SCRATCH/elmfire.data no


# Replace Line for Spotting Parameters: Loops through the SPOTTING_PARAMS array and replaces corresponding lines in the "elmfire.data" file with parameter values. This appears to be done in the context of enabling spotting configurations for simulations.
echo "***************************"
echo "${!SPOTTING_PARAMS[@]}"
echo "$PARAM_VALUES"
ls $INPUTS
echo "***************************"
echo $PARAM_VALUES
for i in "${!SPOTTING_PARAMS[@]}"; do
    echo $i ${SPOTTING_PARAMS[$i]} ${PARAM_VALUES[$i]}
    replace_line_fp ${SPOTTING_PARAMS[$i]} ${PARAM_VALUES[$i]} $SCRATCH/elmfire.data no #This should not do the other stuff
    #echo "Debug: after replace_line_fp for: ${SPOTTING_PARAMS[$i]} ${PARAM_VALUES[$i]} $INPUTS/elmfire.data no"
    #echo "Param vals: ${PARAM_VALUES[$i]}"
done


# Enable Spotting: Sets the spotting configuration to be enabled and specifies the distribution type.
echo "Debug: CM=$CM"
replace_line_fp ENABLE_SPOTTING $ENABLE_SPOTTING $SCRATCH/elmfire.data no
replace_line_fp SPOTTING_DISTRIBUTION_TYPE 'LOGNORMAL' $SCRATCH/elmfire.data yes
echo "Debug: after replace_line_fp"

# Change Output Directory: Updates the output directory in the "elmfire.data" file.
echo "Changing Output Directory: $OUTPUTS_RUN"
replace_line_fp OUTPUTS_DIRECTORY $OUTPUTS_RUN $SCRATCH/elmfire.data yes

cat $SCRATCH/elmfire.data

#####################################################################################
# Rest of function to run ELMFIRE
# Execute ELMFIRE (this runs the script and the inputs stay the same)

echo "***************************"
echo "Running ELMFIRE"
echo $INPUTS

# Get the number of sockets and cores per socket on the system
SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
let "NP = SOCKETS * CORES_PER_SOCKET"

echo "*********************** MPI ***************************"
echo 
#   # Print the number of sockets, cores per socket, and total cores
echo "Number of sockets: $SOCKETS"
echo "Number of cores per socket: $CORES_PER_SOCKET"
echo "Total number of cores (processes for MPI): $NP"
echo "Crown Fire Model: ${CM}"
echo "Cell Size:  ${CELLSIZE}"
echo "Run Number: ${RUN_NUMBER}"
echo "Scratch: ${SCRATCH}"
echo "Misc: ${MISC} , Inputs: ${INPUTS} , Input Dir: ${input_dir}"
echo "WIND_CONFIG: $WIND_CONFIG , ENABLE_SPOTTING: $ENABLE_SPOTTING , Crown Fire Model: $CM"
echo "RUN_NUMBER: $RUN_NUMBER"
echo "RUN_NAME: $RUN_NAME"
echo "OUTPUTS_RUN: $OUTPUTS_RUN"
echo "OUTPUTS: $OUTPUTS"

#ls . 
echo '&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
echo 'Launching ELMFIRE'
set +x

## Launch ELMFIRE using MPI
#echo ' Launch ELMFIRE using MPI '
#mpirun --mca btl tcp,self --map-by core --bind-to core --oversubscribe -np $NP elmfire_$ELMFIRE_VER $INPUTS/elmfire.data >& $OUTPUTS/elmfire.out


## Launch ELMFIRE using Single Core 
echo ' Launch ELMFIRE using Single Core '
elmfire_$ELMFIRE_VER $SCRATCH/elmfire.data $OUTPUTS_RUN
set -x

#Copy Parameter Data to the output directories
cp $SCRATCH/elmfire.data $OUTPUTS_RUN/${RUN_NAME}-elmfire.data
cp $SCRATCH/elmfire.data $TOA/${RUN_NAME}-elmfire.data
echo "***************************"

TOA=$(realpath $TOA)
#process_output_files $OUTPUTS $OUTPUTS_RUN $TOA $RUN_NAME #remain commented right now
    

## Calculate IOU
output_directory="./outputs"
VIIRS_PERIM_FP="./LargeFires_2020.gpkg"
START_DATE="2020-09-05"
END_DATE="2020-09-09"
FIRE_CSV_FP="./fire_csv.csv"
FIRE_FP="
TOA=$(realpath $TOA)
chmod u+rwx "$FIRE_CSV_FP"



python3 iou.py --output_directory="$output_directory" --fire_fp="$FIRE_FP" --viirs_perim_fp="$VIIRS_PERIM_FP" --start_date="$START_DATE" --end_date="$END_DATE" --run_name="$RUN_NAME" --fire_csv_fp="$FIRE_CSV_FP"


