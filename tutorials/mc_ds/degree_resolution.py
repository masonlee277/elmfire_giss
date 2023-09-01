import os
import subprocess
from pathlib import Path
from osgeo import gdal
import argparse
import shutil

def main(input_dir_path, output_dir_path):

    """
    Process and rescale GeoTiff files from the input directory to various resolutions and projections.

    Inputs:
        input_dir_path (str): Path to the input directory containing GeoTiff files to be processed.
        output_dir_path (str): Path to the output directory where the rescaled GeoTiff files will be saved.

    Outputs:
        None

    Functionality:
        - The function takes input and output directory paths as arguments.
        - It defines different resolutions and projections for rescaling the GeoTiff files.
        - Creates output directories for each resolution inside the output directory.
        - Lists all GeoTiff files in the input directory.
        - Processes each file for different resolutions and projections.
        - Saves the rescaled files in the corresponding output directories.
        - Prints information about the process, such as input and output raster sizes.
        - Removes temporary files generated during processing.

    Parameters:
        - input_dir_path: Path to the input directory containing GeoTiff files (str).
        - output_dir_path: Path to the output directory for saving the rescaled files (str).

    Example Usage:
        main('./input_data/', './output_data/')
    """

    # Set input and output directories
    input_dir = Path(input_dir_path)
    output_dir = Path(output_dir_path)

    # Define resolutions
    resolutions = {
        #'6_km': {'res': '6000 6000', 'proj': 'EPSG:32611'},
        'inputs-500M': {'res': '500 500', 'proj': 'EPSG:32611'},
        'inputs-1000M': {'res': '1000 1000', 'proj': 'EPSG:32611'},
        #'0.25_km': {'res': '20000 20000', 'proj': 'EPSG:32611'},
        #'0.5_km': {'res': '40000 40000', 'proj': 'EPSG:32611'},
        #'2.5_2_deg': {'res': '2.5 2.0', 'proj': 'EPSG:4326'},
        #'mean': {'res': '70000 70000', 'proj': 'EPSG:32611'},

    }

    # Create output directories for each resolution
    for res_name in resolutions.keys():
        (output_dir / res_name).mkdir(parents=True, exist_ok=True)

    # List all geotiff files in the input directory
    geotiff_files = list(input_dir.glob('*.tif'))

    # Loop through each file
    for res_name, res_info in resolutions.items():
        for geotiff_file in geotiff_files:
            print(f"Processing file {geotiff_file.name}...")

            # Open the raster and get its metadata
            dataset = gdal.Open(str(geotiff_file), gdal.GA_ReadOnly)
            input_size = dataset.RasterXSize, dataset.RasterYSize
            print(f"Input raster size is {input_size}")

            print(f"Processing resolution {res_name}...")

            # Create output file path for each resolution
            output_file = output_dir / res_name / geotiff_file.name
            
            # If resolution is in degrees, process differently
            if '_deg' in res_name:
                print(f"Processing degree resolution {res_info['res']} and CRS {res_info['proj']}...")
                # Use gdalwarp without -tr option
                command = [
                    'gdalwarp', '-overwrite', '-s_srs', 'EPSG:32611', '-t_srs', res_info['proj'],
                    '-r', 'med', '-of', 'GTiff',
                    '-co', 'COMPRESS=DEFLATE', '-co', 'ZLEVEL=9',
                    str(geotiff_file), str(output_file)
                ]
                subprocess.run(command, check=True)
            else:
                resampling_method = 'max' if 'ignition_mask.tif' in str(geotiff_file) else 'med' ## Med 

                # For other resolutions, keep the existing process
                print(f"Rescaling to resolution {res_info['res']} and CRS {res_info['proj']} using {resampling_method} resampling...")
                command = [
                    'gdalwarp', '-overwrite', '-s_srs', 'EPSG:32611', '-t_srs', res_info['proj'],
                    '-r', resampling_method, '-of', 'GTiff',
                    '-tr', *res_info['res'].split(), 
                    '-co', 'COMPRESS=DEFLATE', '-co', 'ZLEVEL=9',
                    str(geotiff_file), str(output_file)
                ]
                subprocess.run(command, check=True)

            # Project back to the input CRS without changing resolution
            print("Projecting back to the input CRS without changing resolution...")
            temp_file = output_dir / f"temp_{geotiff_file.name}"
            command = [
                'gdalwarp', '-overwrite', '-s_srs', res_info['proj'], '-t_srs', 'EPSG:32611',
                '-r', 'med', '-of', 'GTiff',
                str(output_file), str(temp_file)
            ]
            subprocess.run(command, check=True)

            ##Resize to the original size while keeping original metadata
            print(f"Resizing to the original size {input_size} while keeping original metadata...")
            command = [
                'gdal_translate', '-outsize', str(input_size[0]), str(input_size[1]), 
                '-r', 'nearest',
                '-co', 'COMPRESS=DEFLATE', '-co', 'ZLEVEL=9',
                str(temp_file), str(output_file)
            ]
            subprocess.run(command, check=True)

            # Confirming the file size of output
            output_dataset = gdal.Open(str(output_file), gdal.GA_ReadOnly)
            print(f"Output raster size is {output_dataset.RasterXSize, output_dataset.RasterYSize}")

            # Remove the temporary file
            print("Removing temporary file...")
            temp_file.unlink()
            print(f"######################################")

    print(f"Processed {len(geotiff_files)} geotiff files.")


import os
from osgeo import gdal
import numpy as np

def calc_mean(input_dir_path, output_dir_path):

    """
    Calculate the mean value of each GeoTiff file in the input directory and create new GeoTiff files
    with the mean value as the pixel value in the output directory.

    Inputs:
    input_dir_path (str): Path to the input directory containing GeoTiff files.
    output_dir_path (str): Path to the output directory where the GeoTiff files with mean values will be saved.

    Outputs:
        None

    Functionality:
        - The function takes input and output directory paths as arguments.
        - It creates a 'mean' subfolder inside the output directory for saving the processed GeoTiff files.
        - If the 'mean' subfolder already exists, it deletes its contents and recreates it.
        - Lists all GeoTiff files in the input directory.
        - Processes each file, calculates its mean value, and creates a new GeoTiff file with the mean value as pixel value.
        - Prints information about the process, such as input and output raster sizes.

    Parameters:
        - input_dir_path: Path to the input directory containing GeoTiff files (str).
        - output_dir_path: Path to the output directory for saving the GeoTiff files with mean values (str).

    Example Usage:
        calc_mean('./input_data/', './output_data/')
    """

    # Set input and output directories
    input_dir = input_dir_path
    output_dir = output_dir_path
    output_dir = os.path.join(output_dir_path, 'mean')
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    else:
        # If 'mean' subfolder already exists, delete its contents
        shutil.rmtree(output_dir)
        os.makedirs(output_dir)

    # List all geotiff files in the input directory
    geotiff_files = [os.path.join(input_dir, f) for f in os.listdir(input_dir) if f.endswith('.tif')]

    # Loop through each file
    for geotiff_file in geotiff_files:
        #print(f"Processing file {geotiff_file}...")

        # Open the raster and get its metadata
        dataset = gdal.Open(geotiff_file, gdal.GA_ReadOnly)
        band = dataset.GetRasterBand(1)
        data = band.ReadAsArray()
        input_size = dataset.RasterXSize, dataset.RasterYSize

        # Calculate the mean value
        mean_value = np.mean(data)

        # Create a new raster with the mean value
        mean_raster = np.full_like(data, mean_value)

        # Create the output file
        driver = gdal.GetDriverByName('GTiff')
        output_file = os.path.join(output_dir, os.path.basename(geotiff_file))
        output = driver.Create(output_file, input_size[0], input_size[1], 1, gdal.GDT_Float32)

        # Set the geotransform
        output.SetGeoTransform(dataset.GetGeoTransform())

        # Set the projection
        output.SetProjection(dataset.GetProjection())

        # Write the array data to the raster band
        output_band = output.GetRasterBand(1)
        output_band.WriteArray(mean_raster)
        output_band.FlushCache()

        # Close the output raster dataset
        output = None

        # Open the created file again and compress it with DEFLATE method
        dataset_to_compress = gdal.Open(output_file, gdal.GA_Update)
        driver = gdal.GetDriverByName('GTiff')
        dst_filename = os.path.join(output_dir, os.path.basename(geotiff_file))
        dst_ds = driver.CreateCopy(dst_filename, dataset_to_compress, 0, ['COMPRESS=DEFLATE', 'ZLEVEL=9'])
        
        # Close datasets
        dst_ds = None
        dataset_to_compress = None
        print(f"Compressed file created at {dst_filename}")

        print(f"Mean value: {mean_value}, {os.path.basename(geotiff_file)}, Input raster size is {input_size}, Output raster size is {mean_raster.shape}")

    # Move the original 'ignition_mask.tif' from input_dir_path to the output_dir_path
    input_ignition_mask_path = os.path.join(input_dir_path, 'ignition_mask.tif')
    output_ignition_mask_path = os.path.join(output_dir_path, 'mean', 'ignition_mask.tif')
    
    shutil.copy(input_ignition_mask_path, output_ignition_mask_path)

    print("Original 'ignition_mask.tif' replaced in the output directory.")
    print(f"Processed {len(geotiff_files)} geotiff files.")




if __name__ == "__main__":
    """
    This script processes and rescales GeoTiff files from the input directory to various resolutions and projections.

    The script performs the following tasks:
    1. Takes input and output directory paths as arguments.
    2. Defines different resolutions and projections for rescaling the GeoTiff files.
    3. Creates output directories for each resolution inside the output directory.
    4. Lists all GeoTiff files in the input directory.
    5. Processes each file for different resolutions and projections.
    6. Saves the rescaled files in the corresponding output directories.
    7. Prints information about the process, such as input and output raster sizes.
    8. Removes temporary files generated during processing.

    The script contains two main functions:

    1. `main(input_dir_path, output_dir_path)`:
    - Takes input and output directory paths as arguments.
    - Defines resolutions and projections for rescaling.
    - Creates output directories for each resolution.
    - Processes GeoTiff files for each resolution and projection.
    - Saves the rescaled files in the output directories.
    - Handles both degree and non-degree resolutions.

    2. `calc_mean(input_dir_path, output_dir_path)`:
    - Calculates the mean value of each GeoTiff file in the input directory.
    - Creates new GeoTiff files with the mean value as the pixel value.
    - Creates a 'mean' subfolder inside the output directory.
    - Deletes the 'mean' subfolder if it already exists and recreates it.
    - Moves the original 'ignition_mask.tif' to the output directory.

    Command-line arguments for the script:
    - -i or --input: Path to the input directory containing GeoTiff files.
    - -o or --output: Path to the output directory where the processed files will be saved.

    Example Usage:
    python script_name.py -i ./input_data/ -o ./output_data/
    """
    
    parser = argparse.ArgumentParser(description="Rescale and reproject GeoTIFF files.")
    parser.add_argument("-i", "--input", type=str, help="Path to the input directory.")
    parser.add_argument("-o", "--output", type=str, help="Path to the output directory.")
    args = parser.parse_args()
    main(args.input, args.output)
    #calc_mean(args.input, args.output)
