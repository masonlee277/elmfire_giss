#!/bin/bash

# Path to the reference GeoTIFF
REFERENCE_TIF="/mnt/c/Users/mnlee2/Desktop/linux_shell/elmfire/tutorials/mc_ds/inputs-camp/asp.tif"

# Folder containing the GeoTIFFs that need to be remapped
TARGET_FOLDER="/mnt/c/Users/mnlee2/Desktopdata/saved_outputs/camp_fire_1/toa"

# Output folder for the remapped GeoTIFFs (change as needed)
OUTPUT_FOLDER="/mnt/c/Users/mnlee2/Desktopdata/saved_outputs/camp_fire_1/remapped"

# Create output folder if it doesn't exist
mkdir -p "$OUTPUT_FOLDER"

# Get extent from reference tif
EXTENT=$(gdalinfo $REFERENCE_TIF | grep "Lower Left\|Upper Right" | sed "s/Lower Left  //g;s/Upper Right //g;s/).*//g" | tr -d '(),')
XRES=$(gdalinfo $REFERENCE_TIF | grep "Pixel Size" | sed "s/Pixel Size = //g;s/,/ /g" | awk '{print $1}')
YRES=$(gdalinfo $REFERENCE_TIF | grep "Pixel Size" | sed "s/Pixel Size = //g;s/,/ /g" | awk '{print $2}')

# Resampling method (change if needed)
RESAMPLING_METHOD="nearest"

# Loop through GeoTIFF files in the target folder
for file in "$TARGET_FOLDER"/*.tif; do
  # Define output file name
  OUTPUT_FILE="$OUTPUT_FOLDER/$(basename $file)"
  
  # Perform resampling to match the reference GeoTIFF's extent
  gdalwarp -te $EXTENT -tr $XRES $YRES -r $RESAMPLING_METHOD "$file" "$OUTPUT_FILE"
  
  echo "Remapped $file to $OUTPUT_FILE"
done

echo "Remapping complete."


#####################################################################
#### MOVE OUTPUTS TO TOA FOLDER FOR CORRECT PROCESSING

# Set the TOA variable with the desired path
# TOA="/mnt/c/Users/mnlee2/Desktop/data/saved_outputs/camp_fire_1/toa"
# SOURCE_DIR="/mnt/c/Users/mnlee2/Desktop/data/saved_outputs/camp_fire_1/outputs"

# echo "Starting the process..."

# # Check if TOA directory exists; if not, create it
# if [ ! -d "$TOA" ]; then
#   echo "Creating directory $TOA..."
#   mkdir -p "$TOA"
# else
#   echo "$TOA already exists."
# fi

# # Loop through each subdirectory in SOURCE_DIR
# for SUBDIR in "$SOURCE_DIR"/*; do
#   if [ -d "$SUBDIR" ]; then
#     # Extract the subdirectory name
#     SUBDIR_NAME=$(basename "$SUBDIR")
#     echo "Processing $SUBDIR_NAME..."

#     # Construct the new name for the time_of_arrival file
#     TIME_OF_ARRIVAL_NEW_NAME="${SUBDIR_NAME}_time_of_arrival.tif"
#     echo "Renaming time_of_arrival file to $TIME_OF_ARRIVAL_NEW_NAME..."

#     # Find the time_of_arrival file and copy & rename it to TOA directory
#     find "$SUBDIR" -name 'time_of_arrival*.tif' -exec cp {} "$TOA/$TIME_OF_ARRIVAL_NEW_NAME" \;
#     echo "Copied time_of_arrival file to $TOA/$TIME_OF_ARRIVAL_NEW_NAME."

#     # Find and copy the corresponding .data file
#     find "$SUBDIR" -name '*.data' -exec cp {} "$TOA" \;
#     echo "Copied .data file to $TOA."
#   fi
# done

# echo "Process completed successfully!"



# echo '##############################################################'
# echo 'gathering data'

# FUEL_WX_IGN_CLI=$ELMFIRE_BASE_DIR/cloudfire/fuel_wx_ign.py
# OUTPUT_DIR='./sample-historical'
# NAME='camp-fire'

# rm -rf $OUTPUT_DIR
# mkdir $OUTPUT_DIR
# $FUEL_WX_IGN_CLI \
#    --name=$NAME --outdir=$OUTPUT_DIR \
#    --center_lon=-121.5 --center_lat=40.0 \
#    --do_fuel=True \
#    --fuel_source='landfire' --fuel_version='1.4.0' \
#    --do_wx=True \
#    --wx_type='historical' --wx_start_time="2020-09-05 12:00" --wx_num_hours=96 \
#    --do_ignition=True \
#    --point_ignition=True --ignition_lon=-119.31 --ignition_lat=37.2 --ignition_radius=7000. # in meters



# ## CAMP-FIRE CALL
# $FUEL_WX_IGN_CLI \
#    --name=$NAME --outdir=$OUTPUT_DIR \
#    --center_lon=-121.4657 --center_lat=39.80628\
#    --do_fuel=True \
#    --fuel_source='landfire' --fuel_version='1.4.0' \
#    --do_wx=True \
#    --wx_type='historical' --wx_start_time="2018-08-05 08:00" --wx_num_hours=96 \
#    --do_ignition=True \
#    --point_ignition=True --ignition_lon=-121.437 --ignition_lat=39.8102 --ignition_radius=100000. # in meters


# # echo 'taring'
# FILE_PATH=$OUTPUT_DIR/$NAME.tar
# echo $FILE_PATH
# tar -xvf $FILE_PATH -C $OUTPUT_DIR



# echo '##############################################################'
echo 'finished processing'

##############################################################
function add_line_fp {
    # Define inputs
    local FILE_PATH=$1
    local MATCH_PATTERN=$2
    local NEW_VALUE=$3

    # Add the line to the bottom of the file
    echo "$MATCH_PATTERN = $NEW_VALUE" >> $FILE_PATH
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



INPUTS="./inputs"
TOA="./toa"
OUTPUTS="./outputs"
RUN_NAME='inputs=inputs_0.25deg_WIND=PERTURBED_SPOTTING=.TRUE._S-CONFIG=low_CROWN-FIRE-MODEL=2_time_of_arrival_0000001_0347541'
#process_wallclock $INPUTS $TOA $OUTPUTS $RUN_NAME

################################################
# variables
#directory="./toa_3"
directory="/mnt/c/Users/mnlee2/Desktop/data/saved_outputs/sobol_inputs_2/toa"

background_img_path="./inputs/asp.tif"
saved_directory="./outputs"
#fire_csv="./fire_csv.csv"
fire_csv="./best_IOU2.csv"

# call Python script
#echo "Running"
#python3 plot_generator.py $directory $background_img_path $saved_directory $fire_csv

##########################

wind_speed="./inputs/ws.tif"
wind_direction="./inputs/wd.tif"
fire_perimeter="./toa_dump/lhs_v1.tif"
asp_fp="./inputs/asp.tif"
save_path="./gifs/"
#python3 spread_animation.py $wind_speed $wind_direction $fire_perimeter $asp_fp $save_path --skip_bands 2

##########################

input_files="./inputs"
output_files="./deg_outputs"
#python3 degree_resolution.py -i $input_files -o $output_files

##########################


in_dir="/deg_outputs/inputs-500M"
out_dir="./deg_outputs/"
#python3 plot_inputs.py -i $in_dir -o $out_dir

in_dir="./deg_outputs/1000MD_test"
#python3 plot_inputs.py -i $in_dir -o $out_dir

#########################

output_directory="./outputs"
VIIRS_PERIM_FP="./LargeFires_2020.gpkg"
START_DATE="2020-09-05"
END_DATE="2020-09-09"
FIRE_CSV_FP="./fire_csv.csv"


# # # Get the absolute path of the TOA folder
# TOA='./toa_spotting'
# TOA=$(realpath $TOA)
# echo $TOA
# #ls .

# ## MAKE SURE FIRE_CSV IS CREATED WITH THE COLUMN NAMES
# ##Use ls -t to get a list of files sorted by modification time (newest first)
# ##Then loop over each file
# for file_path in $(ls -t $TOA/*.tif); do
#     # Update FIRE_FP to the current file_path
#     FIRE_FP="$file_path"
#     echo FILE PATH : ${FIRE_FP}

#     # Update RUN_NAME to the filename
#     RUN_NAME=$(basename -- "$file_path")

#     echo RUN NAME : ${RUN_NAME}
#     # Call iou.py with updated parameters
#     python3 iou.py --output_directory="$output_directory" --fire_fp="$FIRE_FP" --viirs_perim_fp="$VIIRS_PERIM_FP" --start_date="$START_DATE" --end_date="$END_DATE" --run_name="$RUN_NAME" --fire_csv_fp="$FIRE_CSV_FP"
# done


####################################################################################
### Parallel IOU calculation:


output_directory="./outputs"
VIIRS_PERIM_FP="./LargeFires_2020.gpkg"
START_DATE="2020-09-05"
END_DATE="2020-09-09"
FIRE_CSV_FP="./fire_csv.csv"
TOA="/mnt/c/Users/mnlee2/Desktop/data/saved_outputs/sobol_inputs_2/toa_rem"

# Change the permissions to allow read, write, and execute for the owner
chmod u+rwx "$FIRE_CSV_FP"

##
TOA=$(realpath $TOA)
echo $TOA 
echo "Starting Parallel IOU calculation"

## MAKE SURE FIRE_CSV IS CREATED WITH THE COLUMN NAMES 
# Get list of file paths sorted by modification time
files=$(ls -t $TOA/*.tif)

# Loop over files in parallel
NUM_CPUS=6
#parallel -j $NUM_CPUS python3 iou.py --output_directory="{1}" --fire_fp="{2}" --viirs_perim_fp="{3}" --start_date="{4}" --end_date="{5}" --run_name="$(basename {2})" --fire_csv_fp="{6}" ::: "$output_directory" ::: $files ::: "$VIIRS_PERIM_FP" ::: "$START_DATE" ::: "$END_DATE" ::: "$FIRE_CSV_FP"



#####################################################################################
#!/bin/bash

# # Define the path to the CSV file
# LHS='./lhs.csv'

# # Define the parameters
# SPOTTING_PARAMS=("CROWN_FIRE_SPOTTING_PERCENT_MIN" "CROWN_FIRE_SPOTTING_PERCENT_MAX" "ENABLE_SURFACE_FIRE_SPOTTING" "GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN" "GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX" "CRITICAL_SPOTTING_FIRELINE_INTENSITY" "SPOTTING_DISTRIBUTION_TYPE" "MEAN_SPOTTING_DIST_MIN" "MEAN_SPOTTING_DIST_MAX" "NORMALIZED_SPOTTING_DIST_VARIANCE_MIN" "NORMALIZED_SPOTTING_DIST_VARIANCE_MAX" "SPOT_WS_EXP_LO" "SPOT_WS_EXP_HI" "SPOT_FLIN_EXP_LO" "SPOT_FLIN_EXP_HI" "NEMBERS_MIN" "NEMBERS_MAX_LO" "NEMBERS_MAX_HI" "PIGN_MIN" "PIGN_MAX")

# # Read the CSV file line by line
# IFS=$'\n'
# for row in $(tail -n +2 "$LHS"); do
#     # Initialize the SPOTTING_CONFIG variable
#     SPOTTING_CONFIG=row

#     echo $row
#     # Run the command
#     #run_elmfire_lhs $input_dir $RUN_NUMBER "${SPOTTING_CONFIG[@]}"
# done




# input_directory="/mnt/c/Users/mnlee2/Desktop/data/saved_outputs/sobol_inputs_2/outputs"

# # Check if the provided directory exists
# if [ ! -d "$input_directory" ]; then
#     echo "Error: Directory $input_directory does not exist."
#     exit 1
# fi

# INPUTS=./inputs
# A_SRS=`gdalsrsinfo $INPUTS/fbfm40.tif | grep PROJ.4 | cut -d: -f2 | xargs`

# # Iterate over all subdirectories in the specified directory
# for subdir in "$input_directory"/*; do
#     if [ -d "$subdir" ]; then
#         # Iterate over all .bin files in the current subdirectory
#         for binfile in "$subdir"/*.bin; do
#             # If .bin file is found
#             if [ -f "$binfile" ]; then
#                 echo "Found .bin file: $(basename "$binfile")"

#                 # Remove the file extension to use for the .tif filename
#                 base=$(basename "$binfile" .bin)
#                 tiffile="${subdir}/${base}.tif"

#                 # Use gdal_translate to convert .bin to .tif with compression
#                 gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" "$binfile" "$tiffile"

#                 # Check the status of the conversion
#                 if [ $? -eq 0 ]; then
#                     echo "Converted $(basename "$binfile") to $(basename "$tiffile") successfully."
#                 else
#                     echo "Failed to convert $(basename "$binfile")."
#                 fi
#             fi
#         done
#     fi
# done

# echo "Conversion process completed."