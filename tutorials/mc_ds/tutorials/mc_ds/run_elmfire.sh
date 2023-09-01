#!/bin/bash

# Command-line arguments
INPUTS=./$1
input_dir=$1
RUN_NUMBER=$2
CM=$3

################################################
function echo_values {
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
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
   cat $6/elmfire.data
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


# Arrays to iterate over
input_dir_list=("inputs-flat" "inputs-0.5deg" "inputs-0.25deg" "inputs-wn" "inputs-6km" "inputs" "inputs-fbfm-most-common")
WIND_VALUES=("ORIGINAL" "PERTURBED") #
ENABLE_SPOTTING_VALUES=(".TRUE." ".FALSE.")
CROWN_FIRE_MODEL_VALUES=("0" "2")
#SPOTTING_CONFIGS=("low" "medium" "high")
SPOTTING_CONFIGS=("low" "medium")

# Spotting params
LOW_SPOTTING_PARAMS=("0.1" "0.1" ".TRUE." "0.1" "0.1" "4000.0" "'LOGNORMAL'" "5.0" "15.0" "150.0" "400.0" "0.4" "0.7" "0.2" "0.4" "1" "1" "1" "20.0" "70.0")
MEDIUM_SPOTTING_PARAMS=("0.2" "0.4" ".TRUE." "0.2" "0.4" "2500.0" "'LOGNORMAL'" "150.0" "1000.0" "250.0" "600.0" "0.4" "0.7" "0.2" "0.4" "1" "1" "1" "40.0" "85.0")
HIGH_SPOTTING_PARAMS=("0.3" "0.6" ".TRUE." "0.3" "0.6" "1000.0" "'LOGNORMAL'" "1000.0" "2000.0" "250.0" "600.0" "0.4" "0.7" "0.2" "0.4" "1" "1" "1" "100.0" "100.0")
SPOTTING_PARAMS=("CROWN_FIRE_SPOTTING_PERCENT_MIN" "CROWN_FIRE_SPOTTING_PERCENT_MAX" "ENABLE_SURFACE_FIRE_SPOTTING" "GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN" "GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX" "CRITICAL_SPOTTING_FIRELINE_INTENSITY" "SPOTTING_DISTRIBUTION_TYPE" "MEAN_SPOTTING_DIST_MIN" "MEAN_SPOTTING_DIST_MAX" "NORMALIZED_SPOTTING_DIST_VARIANCE_MIN" "NORMALIZED_SPOTTING_DIST_VARIANCE_MAX" "SPOT_WS_EXP_LO" "SPOT_WS_EXP_HI" "SPOT_FLIN_EXP_LO" "SPOT_FLIN_EXP_HI" "NEMBERS_MIN" "NEMBERS_MAX_LO" "NEMBERS_MAX_HI" "PIGN_MIN" "PIGN_MAX")

MC_ONLY_PARAMS=("METEOROLOGY_BAND_START = 1"
                "METEOROLOGY_BAND_STOP = 1"
                "METEOROLOGY_BAND_SKIP_INTERVAL = 1"
                "NUM_METEOROLOGY_TIMES = 97"
                "RANDOM_IGNITIONS = .FALSE."
                "USE_IGNITION_MASK = .TRUE."
                "USE_ERC = .FALSE."
                "ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL = .TRUE."
                "NUM_ENSEMBLE_MEMBERS = 1"
                "SEED = 2021"
                "WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN = 0"
                "WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX = 30"
                "WIND_SPEED_FLUCTUATION_INTENSITY_MIN = 0.3"
                "WIND_SPEED_FLUCTUATION_INTENSITY_MAX = 0.50"
                "NUM_RASTERS_TO_PERTURB = 2"
                "RASTER_TO_PERTURB(1) = 'WS'"
                "SPATIAL_PERTURBATION(1) = 'GLOBAL'"
                "TEMPORAL_PERTURBATION(1) = 'STATIC'"
                "PDF_TYPE(1) = 'UNIFORM'"
                "PDF_LOWER_LIMIT(1) = 22.0"
                "PDF_UPPER_LIMIT(1) = 23.0"
                "RASTER_TO_PERTURB(2) = 'WD'"
                "SPATIAL_PERTURBATION(2) = 'GLOBAL'"
                "TEMPORAL_PERTURBATION(2) = 'STATIC'"
                "PDF_TYPE(2) = 'UNIFORM'"
                "PDF_LOWER_LIMIT(2) = -90.0"
                "PDF_UPPER_LIMIT(2) = -89.5")

ORIG_PARAMS=("NUM_METEOROLOGY_TIMES = 97")

###############################################


DOMAINSIZE=12000.0 # Height and width of domain in meters
SIMULATION_TSTOP=347400.0

. ../functions/functions.sh
OUTPUTS=./outputs
TOA=./toa

# Run the command
SCRATCH="./SCRATCH_$RUN_NUMBER"
MISC="./MISC_$RUN_NUMBER"

mkdir $SCRATCH $MISC


CM=0
#Defaults
WIND_CONFIG="ORIGINAL"
ENABLE_SPOTTING=".TRUE."
    # Set CROWN_FIRE_MODEL with an input option or default to 2 if not provided

echo "Debug: CROWING = $CM , INPUTS=$INPUTS"

#setup_inputs $INPUTS
cp ./elmfire.data.in $INPUTS/elmfire.data

replace_line_fp FUELS_AND_TOPOGRAPHY_DIRECTORY "$INPUTS" $INPUTS/elmfire.data yes
replace_line_fp WEATHER_DIRECTORY "$INPUTS" $INPUTS/elmfire.data yes

echo "Setting up inputs in $INPUTS"   

XMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`
XMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`

# Print bounds
echo "xmin: $XMIN"
echo "xmax: $XMAX" 
echo "ymin: $YMIN"
echo "ymax: $YMAX"

# Calculate center
XCEN=`echo "0.5*($XMIN + $XMAX)" | bc`
YCEN=`echo "0.5*($YMIN + $YMAX)" | bc`  

# Get spatial reference system
A_SRS=`gdalsrsinfo $INPUTS/fbfm40.tif | grep PROJ.4 | cut -d: -f2 | xargs`

# Get cell size
CELLSIZE=`gdalinfo $INPUTS/fbfm40.tif | grep 'Pixel Size' | cut -d'(' -f2 | cut -d, -f1`  

# Update input file
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

RUN_NAME="inputs=${input_dir}_WIND=${WIND_CONFIG}_SPOTTING=${ENABLE_SPOTTING}_S-CONFIG=${RUN_NUMBER}-lhs_CROWN-FIRE-MODEL=${CM}"
echo "Debug: RUN_NAME=$RUN_NAME"

OUTPUTS_RUN="./outputs/${RUN_NAME}"
echo "Debug: OUTPUTS_RUN=$OUTPUTS_RUN"

export $RUN_NAME
echo "Debug: after export"

mkdir -p $OUTPUTS_RUN
echo "Debug: after mkdir 1"
echo "Debug: after replace_line_fp"

# Spotting config
echo "***************************"
echo "${!SPOTTING_PARAMS[@]}"
echo "$PARAM_VALUES"
ls $INPUTS
echo "***************************"

for i in "${!SPOTTING_PARAMS[@]}"; do
    echo ${SPOTTING_PARAMS[$i]} ${PARAM_VALUES[$i]} 
    replace_line_fp ${SPOTTING_PARAMS[$i]} ${PARAM_VALUES[$i]} $INPUTS/elmfire.data no
    #echo "Debug: after replace_line_fp for: ${SPOTTING_PARAMS[$i]} ${PARAM_VALUES[$i]} $INPUTS/elmfire.data no"
    #echo "Param vals: ${PARAM_VALUES[$i]}"
done

ENABLE_SPOTTING=.TRUE.
CM=0
echo "Debug: CM=$CM"

replace_line_fp ENABLE_SPOTTING $ENABLE_SPOTTING $INPUTS/elmfire.data no
replace_line_fp SPOTTING_DISTRIBUTION_TYPE 'LOGNORMAL' $INPUTS/elmfire.data yes
replace_line_fp CROWN_FIRE_MODEL $CM $INPUTS/elmfire.data no

# Crown fire model
#echo "Debug: CROWN_FIRE_MODEL=$CROWN_FIRE_MODEL_TRUE"
echo "Debug: after replace_line_fp"

echo_values $RUN_NAME $OUTPUTS_RUN $OUTPUTS $MISC $SCRATCH $INPUTS
echo "Debug: after echo_values"

# Rest of function to run ELMFIRE
# Execute ELMFIRE (this runs the script and the inputs stay the same)
echo "***************************"
echo "Running ELMFIRE"


echo $INPUTS

#   # Get the number of sockets and cores per socket on the system
SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
let "NP = SOCKETS * CORES_PER_SOCKET"

echo "*********************** MPI ***************************"
echo $INPUTS

#   # Print the number of sockets, cores per socket, and total cores
echo "Number of sockets: $SOCKETS"
echo "Number of cores per socket: $CORES_PER_SOCKET"
echo "Total number of cores (processes for MPI): $NP"
echo "Crown Fire Model: ${CROWN_FIRE_MODEL}"

# Launch ELMFIRE using MPI
###!mpirun --mca btl tcp,self --map-by core --bind-to core --oversubscribe -np $NP elmfire_$ELMFIRE_VER $INPUTS/elmfire.data >& $OUTPUTS/elmfire.out
elmfire_$ELMFIRE_VER $INPUTS/elmfire.data $OUTPUTS
cp $INPUTS/elmfire.data $TOA/${RUN_NAME}-elmfire.data
echo "***************************"

TOA=$(realpath $TOA)
process_output_files $OUTPUTS $OUTPUTS_RUN $TOA $RUN_NAME
