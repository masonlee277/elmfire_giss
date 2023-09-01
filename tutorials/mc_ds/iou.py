from pyproj import Transformer
import os
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
import pandas as pd 

import numpy as np
from matplotlib.backends.backend_agg import FigureCanvasAgg
from copy import copy
from copy import deepcopy

import rasterio
from rasterio.features import shapes
from shapely.geometry import shape
import geopandas as gpd

import subprocess
import sys
import argparse
import regex as re

##########################################################
## Configure ():

######################################
# Open The VIIRS perimeters
#######################################
#
########################################
def calculate_area(geom, crs):
    """
    Calculate the area of a geometry in the provided coordinate reference system (CRS).

    Args:
        geom (shapely.geometry): The geometry to calculate the area for.
        crs (str): The CRS of the geometry.

    Returns:
        float: The calculated area in square meters.
    """
    # Convert the geometry to a GeoDataFrame in the provided CRS
    geo_df = gpd.GeoDataFrame([1], geometry=[geom], crs=crs)

    # Convert the CRS to a geographic one (EPSG:4326 is a common geographic CRS)
    geo_df = geo_df.to_crs("EPSG:32611")

    # The area method gives areas in square degrees, so we need to convert it to square meters
    # 1 square degree is approximately 12365000 square meters at the equator
    return geo_df.geometry.area[0]

def plot_to_array(fig):
    """
    Convert a matplotlib figure into a numpy array representation.

    Args:
        fig (matplotlib.figure.Figure): The figure to convert.

    Returns:
        numpy.ndarray: The numpy array representation of the figure.
    """
    canvas = FigureCanvasAgg(fig)
    canvas.draw()
    buf = canvas.buffer_rgba()
    return np.asarray(buf)

def map_to_classes(grayscaled_cropped):
    """
    Map grayscale images to class labels based on predefined thresholds.

    Args:
        grayscaled_cropped (list of numpy.ndarray): List of grayscale images.

    Returns:
        list of numpy.ndarray: List of class labels images.
    """
    class_labels = []
    thresholds = [25, 50, 200]  # Thresholds for class boundaries

    for image in grayscaled_cropped:
        classes = np.digitize(image, thresholds, right=True)
        class_labels.append(classes)
    return class_labels


def plot_class_labels(class_labels_cropped):
    """
    Plot images with class labels using a color map.

    Args:
        class_labels_cropped (list of numpy.ndarray): List of class label images.

    Returns:
        None
    """
    for image in class_labels_cropped:
        plt.imshow(image, cmap='viridis')
        print(np.unique(image))
        plt.axis('off')
        plt.show()

def calculate_iou(class_labels_cropped):
    """
    Calculate Intersection over Union (IoU) scores for a list of class label images.

    Args:
        class_labels_cropped (list of numpy.ndarray): List of class label images.

    Returns:
        list of float: List of IoU scores.
    """
    ious = []
    for image in class_labels_cropped:
        mask_union = (image != 3)  # Mask for background (class 1)
        mask_intersection = (image == 1)  # Mask for intersection

        intersection = mask_intersection.sum()
        union = mask_union.sum()

        #print(intersection, union)

        iou = intersection / union
        ious.append(iou)
    return ious

def convert_to_grayscale(cropped):
    """
    Convert a list of RGB images to grayscale.

    Args:
        cropped (list of numpy.ndarray): List of RGB images.

    Returns:
        list of numpy.ndarray: List of grayscale images.
    """

    grayscales = []
    for image in cropped:
        grayscale = np.dot(image[..., :3], [0.2989, 0.5870, 0.1140])  # only take the RGB values, exclude Alpha.
        grayscales.append(grayscale)
    return grayscales


def count_seconds(start_date, start_time, end_date, end_time):
    """
    Calculate the difference in seconds between two datetime strings.

    Args:
        start_date (str or datetime.datetime): Start date string or datetime object.
        start_time (str): Start time string.
        end_date (str or datetime.datetime): End date string or datetime object.
        end_time (str): End time string.

    Returns:
        float: The difference in seconds.
    """
    # Format for date and time
    format_str = '%Y-%m-%d %H%M'

    # Check if start_date and end_date are already datetime objects, if not convert them
    if isinstance(start_date, datetime):
        start = start_date
    else:
        # The time string should be padded with zeros if it is not four digits
        start = f"{start_date} {start_time.zfill(4)}"
        start = datetime.strptime(start, format_str)

    if isinstance(end_date, datetime):
        end = end_date
    else:
        # The time string should be padded with zeros if it is not four digits
        end = f"{end_date} {end_time.zfill(4)}"
        end = datetime.strptime(end, format_str)

    # Calculate the difference in seconds
    difference = end - start
    return difference.total_seconds()
############################################################

def append_row_to_csv(row, csv_filepath):

    """
    Append a row to a CSV file.

    Args:
        row (pd.Series): The row to append.
        csv_filepath (str): Path to the CSV file.

    Returns:
        None
    """
    # Check if file exists and is not empty
    if not os.path.isfile(csv_filepath) or os.stat(csv_filepath).st_size == 0:
        row.to_csv(csv_filepath, mode='a', header=True, index=False)
    else:
        row.to_csv(csv_filepath, mode='a', header=False, index=False)


#############################################################
def create_row(df, inputs, wind_config, spotting, crown_fire_model, run_name, wall_clock):
    """
    Create a new DataFrame row for summarizing run results.

    Args:
        df (pd.DataFrame): The original DataFrame to extract data from.
        inputs (str): Inputs value for the row.
        wind_config (str): Wind configuration value for the row.
        spotting (str): Spotting value for the row.
        crown_fire_model (str): Crown fire model value for the row.
        run_name (str): Description value for the row.
        wall_clock (float): Wall clock time value for the row.

    Returns:
        pd.DataFrame: A new DataFrame containing the created row.
    """
    
    # Assuming the DataFrame has at least 8 rows
    assert len(df) >= 8, "Dataframe should have at least 8 rows"

    # Create a dictionary with column names as keys and corresponding values

    data = {
        'Description': run_name,
        'INPUTS': inputs,
        'WIND_CONFIG': wind_config,
        'ENABLE_SPOTTING': spotting,
        'CROWN_FIRE_MODEL': crown_fire_model,
        'Wall clock time': wall_clock, # Placeholder value, please replace
        **{f'Day {i + 1} Fire Area': round(df.loc[i, 'area_spread'],3) for i in range(8)},
        **{f'Day {i + 1} VIIRS Area': round(df.loc[i, 'area_viirs'],3) for i in range(8)},
        **{f'Day {i + 1} IOU': round(df.loc[i, 'iou'],3) for i in range(8)},
        'Comments on anything interesting': 'None' # Placeholder value, please replace
    }

    # Convert the dictionary to a DataFrame row
    row_df = pd.DataFrame(data, index=[0])

    return row_df

################################################
def read_fire_perimeters(viirs_perim_fp, bounds, fire_id):
    """
    Read fire perimeter data from a GeoPackage file, filter it spatially, and reproject it.

    Args:
        viirs_perim_fp (str): Path to the GeoPackage file containing fire perimeter data.
        bounds (list): List of bounding box coordinates [minx, miny, maxx, maxy].
        fire_id (int): Fire ID to filter the data.

    Returns:
        geopandas.GeoDataFrame: Filtered and reprojected fire perimeter data.
        pyproj.crs.CRS: Coordinate reference system (CRS) of the GeoDataFrame.
    """
        
    from shapely.geometry import box
    import geopandas as gpd

    # Load GeoDataFrame from file
    df = gpd.read_file(viirs_perim_fp, driver='GPKG')
    
    bbox = box(*bounds)  # the * operator unpacks the list

    # Perform the spatial filtering
    df_filtered = df.loc[df['fireID'] == fire_id]
    df_filtered = df_filtered.reset_index(drop=True)
    
    gdf_viirs_perim = df_filtered
    gdf_viirs_perim = gdf_viirs_perim.to_crs("EPSG:32611")
    crs = gdf_viirs_perim.crs

    # Ensure 'time' is in datetime format
    gdf_viirs_perim['time'] = pd.to_datetime(gdf_viirs_perim['time'])

    # Get the unique dates from the gdf_viirs_perim
    # Extract date and hour from 'time' column
    gdf_viirs_perim['date'] = gdf_viirs_perim['time'].dt.date.astype(str)
    gdf_viirs_perim['hour'] = gdf_viirs_perim['time'].dt.hour.astype(str)

    return gdf_viirs_perim, crs

#############################################################

def parse_filename(filename):
    """
    Parse the parameters from a filename and extract relevant values.

    Args:
        filename (str): The input filename.

    Returns:
        tuple: A tuple containing parsed values (inputs, wind_config, spotting, crown_fire_model).
    """
    filename = os.path.basename(filename)

    print(f'parsing filename: {filename}')
    # The first split separates the filename into the parameters and the rest of the filename
    parameters, *rest = filename.split('_time_of_arrival')
    
    # Split parameters into segments based on underscores
    segments = parameters.split('_')

    # Default values
    inputs = wind_config = spotting = crown_fire_model = None

    # The first segment will always be the 'inputs' value
    for segment in segments: print(segment)

    inputs = segments[0].split('=')[1]
    for segment in segments:
        if 'WIND' in segment:
            wind_config = segment.split('=')[1]
        elif 'SPOTTING' in segment:
            spotting = segment.split('=')[1]
        elif 'S-CONFIG' in segment:
            spotting = segment.split('=')[1]
        elif 'CROWN-FIRE-MODEL' in segment:
            crown_fire_model = segment.split('=')[1]

    print(f'params: {inputs, wind_config, spotting, crown_fire_model}')
    return inputs, wind_config, spotting, crown_fire_model

#############################################################
def calculate_and_plot(df, fire_fp, start_date, end_date):
    """
    Filters a dataframe by start and end date, calculates IOU over time between 
    VIIRS hotspots and simulated fire perimeters, and generates plots.
    
    Args:
        dataframe (pandas.DataFrame): DataFrame containing VIIRS hotspot data
        fire_perimeter_filepath (str): File path to GeoTIFF of simulated fire perimeter
        start_date (str): Start date in YYYY-MM-DD format
        end_date (str): End date in YYYY-MM-DD format
        
    Returns:
        times (list): List of datetime objects representing plot times 
        ious (list): List of IOU values for each time
        area_spread (list): List of areas of simulated fire at each time
        area_viirs (list): List of areas of VIIRS hotspots at each time
    """

    # Filter dates based on range
    if isinstance(start_date, str):
        start_date = datetime.strptime(start_date, '%Y-%m-%d')

    # Constants
    buffer_size = 25000
    unique_hours = df['hour'].unique()

    # Variables to store results
    numpy_arrays = []
    area_viirs = []
    area_spread = []
    times = []

    # Open the fire perimeter data
    with rasterio.open(fire_fp) as fire_src:
        fire = fire_src.read(1)
        pixel_size_x, pixel_size_y = fire_src.res
        pixel_area = abs(pixel_size_x * pixel_size_y)
        nodata = fire_src.nodata
        #fire = np.ma.masked_equal(fire, nodata)
        xmin, ymin, xmax, ymax = fire_src.bounds
        xmin -= buffer_size
        ymin -= buffer_size
        xmax += buffer_size
        ymax += buffer_size

    # Filter dates based on range
    all_dates = df['date'].unique().astype(str)
    start_date = start_date.strftime('%Y-%m-%d')  # Convert start_date to a string
    start_date = datetime.strptime(start_date, '%Y-%m-%d').date()  # Convert start_date to a date object

    end_date = datetime.strptime(end_date, '%Y-%m-%d').date()
    filtered_dates = [date for date in all_dates if start_date <= datetime.strptime(date, '%Y-%m-%d').date() <= end_date]

    # Loop over each date
    for date in filtered_dates:
        gdf_date = df[df['date'] == date].to_crs("EPSG:32611")

        for hour in unique_hours:
            gdf_time = gdf_date[gdf_date['hour'] == hour].to_crs("EPSG:32611")
            area_v = float(gdf_time.geometry.area / 1000000)
            area_viirs.append(area_v)

            start_time = '0000'
            end_time = str(copy(hour)) + '00'
            max_val = count_seconds(start_date, start_time, date, end_time)

            # Plot fire_masked on first row
            image = np.where(fire < max_val, 1, 0).astype(np.float32)
            image = np.where(fire == nodata, 0, image)

            fire_masked = np.ma.masked_where(fire > max_val, fire)
            positive_pixel_count = (image == 1).sum()
            area_f = float(positive_pixel_count * pixel_area / 1000000)
            area_spread.append(area_f)

            binary_mask = image
            times.append(gdf_time['time'])

            fig, ax = plt.subplots(figsize=(10, 8))
            ax.imshow(binary_mask, cmap='binary', extent=(xmin, xmax, ymin, ymax))
            ax.set_title(f"{date} - VIIRS Acquisition Time: {hour}", fontsize=36)
            gdf_time.plot(ax=ax, color='red', alpha=0.5)
            ax.set_xlim(xmin, xmax)
            ax.set_ylim(ymin, ymax)
            
            #plt.show()
            plt.close(fig)

            # Convert the plot to a numpy array
            numpy_array = plot_to_array(deepcopy(fig))
            numpy_arrays.append(numpy_array)

    # Convert plots to grayscale, map to classes, and calculate IOU
    grayscaled_cropped = convert_to_grayscale([x[120:650, 210:800] for x in numpy_arrays])
    
    ######################
    # Calculate the number of images
    num_images = len(grayscaled_cropped)

    # Set the figure size based on the number of images
    fig_length = num_images * 3  # Adjust the factor (3) as needed
    fig_width = 8

    # Create the figure with subplots
    fig, axs = plt.subplots(1, num_images, figsize=(fig_length, fig_width))

    # Iterate over each image and plot it
    for i, image in enumerate(grayscaled_cropped):
        axs[i].imshow(image, cmap='gray')
        axs[i].axis('off')

    # Show the figure
    plt.show()
    ######################

    class_labels_cropped = map_to_classes(grayscaled_cropped)
    ious = calculate_iou(class_labels_cropped)

    min_len = min(len(ious), len(area_viirs), len(times), len(area_spread))
    ious = ious[:min_len]
    area_viirs = area_viirs[:min_len]
    times = times[:min_len]
    area_spread = area_spread[:min_len]

    print("Length of ious:", len(ious))
    print("Length of area_virrs:", len(area_viirs))
    print("Length of times:", len(times))

    return times, ious, area_spread, area_viirs


#################################################
import os
def get_wall_clock(file_path):
    # Replace .tif with .data
    base = os.path.splitext(file_path)[0]
    data_file_path = base + '.data'

    # Open the file
    with open(data_file_path, 'r') as f:
        lines = f.readlines()

    # Get the last line
    last_line = lines[-1].strip()

    # Check if the last line starts with "WALL_CLOCK"
    if last_line.contains("WALL_CLOCK"):
        # Extract the wall clock value
        wall_clock_value = float(last_line.split('=')[1].strip())
        return wall_clock_value

    else:
        print('Failed to Find Wall Clock')
        return None


#################################################
def main(args):
    """
    Main function that runs IOU calculation and plotting on input data.
    
    Args:
        args (argparse.Namespace): Arguments including:
            output_directory (str): Path to directory to write output CSV and plots
            fire_fp (str): File path to GeoTIFF containing simulated fire perimeter
            viirs_perim_fp (str): File path to shapefile containing VIIRS fire hotspot perimeters
            start_date (str): Start date string in YYYY-MM-DD format to filter data
            end_date (str): End date string in YYYY-MM-DD format to filter data 
            fire_csv_fp (str): File path to CSV log containing info on run simulations
            run_name (str): Name of the simulation run being processed
    
    The function first prints out the key parameters for debugging.
    
    It reads the existing fire_csv to check if the run_name has already been processed.
    If so, it exits early.
    
    The VIIRS perimeter shapefile is read into a dataframe and projected to EPSG 32611.
    
    The calculate_and_plot() function is called to filter the VIIRS dataframe, compute 
    IOU over time, and generate plots.
    
    The IOU and area results, along with parsed data from the run_name and wall clock 
    time, are used to create a new row for the fire_csv.
    
    This row is appended to the CSV and the dataframe of results is written to a CSV.
    """

    print("Starting processing IOU...")

    directory = args.output_directory
    fire_fp = args.fire_fp
    VIIRS_PERIM_FP = args.viirs_perim_fp
    start_date = args.start_date
    end_date = args.end_date
    fire_csv_fp = args.fire_csv_fp
    run_name = args.run_name
    run_name = os.path.basename(run_name)

    skipped_files = []

    # print(f"Output directory: {directory}")
    # print(f"Fire perimeter file path: {fire_fp}")
    # print(f"VIIRS perimeter file path: {VIIRS_PERIM_FP}")
    # print(f"Fire CSV file path: {fire_csv_fp}")
    # print(f"Start date: {start_date}")
    # print(f"End date: {end_date}")


    fire_csv = pd.read_csv(fire_csv_fp)
    #print(fire_csv['Description'])
    #run_number = re.search(r"(\d{7})(?=\.tif)")
    pattern = r'S-CONFIG=(\d+)'
    run_number = re.search(pattern,run_name)
    if run_number is not None:
        run_number = run_number.group(1)
        print(f"Run number found: {run_number}")

    else:
        print("No run number found in run_name")
        #breaks 
        return

    # Check if any description contains the run number
    if fire_csv['Description'].astype(str).str.contains(r'\b{}\b'.format(run_number)).any():
        print('********************************')
        print(f'ALREADY PROCESSED THIS FILE: {run_number, run_name}')
        print('********************************')

        skipped_files.append(run_number)
        return
    else:
        print('********************************')
        print(f'CONTINUING TO PROCESS FILE: {run_name}')
        print('********************************')

    #Must Set Manually
    bounds = [-119.48573401,  36.99620848, -119.12688445,   37.50860408]
    fire_id = 3763

    print("Reading fire perimeters...")
    df, crs = read_fire_perimeters(VIIRS_PERIM_FP, bounds, fire_id)

    print("Calculating and plotting data...")
    times, ious, area_spread, area_viirs = calculate_and_plot(df, fire_fp, start_date, end_date)

    print("Creating dataframe...")
    spread_df = pd.DataFrame({
        'time': times,
        'iou': ious,
        'area_spread': area_spread,
        'area_viirs': area_viirs,
    })

    inputs, wind_config, spotting, crown_fire_model = parse_filename(run_name)
    
    #Extract Wall Clock:
    try:
        wall_clock = get_wall_clock(fire_fp)
    except: 
        wall_clock = None
        print('extracting wallclock failed')

    # Use the function to create a new row
    new_row = create_row(spread_df, inputs, wind_config, spotting, crown_fire_model,run_name,wall_clock)
    
    #Append the Row to Working CSV
    append_row_to_csv(new_row, fire_csv_fp)

    #print(spread_df.head)
    print(f"Writing to csv file at {os.path.join(directory, 'spread_df.csv')}...")
    spread_df.to_csv(os.path.join(directory, 'spread_df.csv'), index=False)
    print("Processing completed.")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Python script to process and analyse wildfire data")
    parser.add_argument('--output_directory', type=str, required=True, help="Output directory for the csv file")
    parser.add_argument('--fire_fp', type=str, required=True, help="Path to the fire perimeter file")
    parser.add_argument('--viirs_perim_fp', type=str, required=True, help="Path to the VIIRS perimeter file")
    parser.add_argument('--start_date', type=str, required=True, help="Start date in 'YYYY-MM-DD' format")
    parser.add_argument('--end_date', type=str, required=True, help="End date in 'YYYY-MM-DD' format")
    parser.add_argument('--run_name', type=str, required=True, help="End date in 'YYYY-MM-DD' format")
    parser.add_argument('--fire_csv_fp', type=str, required=True, help="End date in 'YYYY-MM-DD' format")

    args = parser.parse_args()
    main(args)
