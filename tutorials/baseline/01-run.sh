#!/bin/bash

# Begin specifying inputs

#CELLSIZE=30.0 # Grid size in meters
DOMAINSIZE=12000.0 # Height and width of domain in meters
#SIMULATION_TSTOP=22200.0 # Simulation stop time (seconds)
SIMULATION_TSTOP=347400.0

. ../functions/functions.sh

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs
MISC=./misc

rm -f -r $SCRATCH $OUTPUTS 
mkdir $SCRATCH $OUTPUTS 

XMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`
XMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`
echo "xmin: $XMIN"
echo "xmax: $XMAX"
echo "ymin: $YMIN"
echo "ymax: $YMAX"
echo "Extracted geospatial metadata from the fbfm40.tif file."

# Calculate the center of the domain
XCEN=`echo "0.5*($XMIN + $XMAX)" | bc`
YCEN=`echo "0.5*($YMIN + $YMAX)" | bc`
echo "Calculated the center of the domain as X: $XCEN and Y: $YCEN."

# Extract the spatial reference system
A_SRS=`gdalsrsinfo $INPUTS/fbfm40.tif | grep PROJ.4 | cut -d: -f2 | xargs` 

# Extract the pixel size, i.e., the grid size in meters
CELLSIZE=`gdalinfo $INPUTS/fbfm40.tif | grep 'Pixel Size' | cut -d'(' -f2 | cut -d, -f1`
echo "Extracted spatial reference system as: $A_SRS and cell size as: $CELLSIZE."

#gdalwarp -multi -dstnodata -9999 -tr 300 300 $INPUTS/adj.tif $SCRATCH/dummy.tif
#gdal_calc.py -A $SCRATCH/dummy.tif --NoDataValue=-9999 --type=Float32 --outfile="$SCRATCH/float.tif" --calc="A*0.0"


# Set parameters in the elmfire.data file by calling a function replace_line defined in functions.sh
#  This line replaces the value of the line containing "COMPUTATIONAL_DOMAIN_XLLCORNER" in the "elmfire.data" 
#  file with the value stored in the $XMIN variable. The "no" at the end specifies that the replacement should 
#  be done without using regular expressions.
replace_line COMPUTATIONAL_DOMAIN_XLLCORNER $XMIN no
replace_line COMPUTATIONAL_DOMAIN_YLLCORNER $YMIN no
replace_line COMPUTATIONAL_DOMAIN_CELLSIZE $CELLSIZE no
replace_line SIMULATION_TSTOP $SIMULATION_TSTOP no
replace_line DTDUMP $SIMULATION_TSTOP no
replace_line A_SRS "$A_SRS" yes
replace_line 'X_IGN(1)' $XCEN no
replace_line 'Y_IGN(1)' $YCEN no

# End inputs specification

ELMFIRE_VER=${ELMFIRE_VER:-2023.06}

# XMIN=`echo "0.0 - 0.5 * $DOMAINSIZE" | bc -l`
# XMAX=`echo "0.0 + 0.5 * $DOMAINSIZE" | bc -l`
# YMIN=$XMIN
# YMAX=$XMAX

TR="$CELLSIZE $CELLSIZE"
TE="$XMIN $YMIN $XMAX $YMAX"

echo "***************************"
echo "Parameters"
cat ./inputs/elmfire.data

# Execute ELMFIRE
echo "***************************"
elmfire_$ELMFIRE_VER ./inputs/elmfire.data

# Postprocess
for f in ./outputs/*.bil; do
   gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f ./outputs/`basename $f | cut -d. -f1`.tif
done
gdal_contour -i 3600 `ls ./outputs/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp

# Clean up and exit:
#rm -f -r ./outputs/*.csv ./outputs/*.bil ./outputs/*.hdr $SCRATCH $MISC

# Copy elmfire.data to the output folder
cp ./inputs/elmfire.data ./outputs/elmfire.data

exit 0
