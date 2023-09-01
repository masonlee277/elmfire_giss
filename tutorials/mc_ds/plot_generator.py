import subprocess
import sys

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# # Example usage:
# install('rasterio')
# install('matplotlib')
# install('cartopy')
# install('geopandas')

import argparse
import pandas as pd
import geopandas as gpd
import cartopy.crs as ccrs
import numpy as np
import matplotlib.ticker as mticker
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import numpy as np
from PIL import Image  # Import the PIL module to open images


import os
import glob
import rasterio
import matplotlib.pyplot as plt
from matplotlib.image import imread

import cartopy
import cartopy.crs as ccrs
import matplotlib.ticker as mticker
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from pyproj import Proj, transform
import matplotlib.patches as mpatches

green_patch = mpatches.Patch(color='green', label='predicted fire_spread') #set the label for the plot
inProj = Proj(init='epsg:32611')  # The original CRS of your geotiff, replace with the correct one


def plot_geotiffs(directory, background_img_path, saved_directory, fire_perim_fp, fire_table_fp=None):
    
    """
    Plots geotiffs in a grid layout using matplotlib and cartopy.
    
    Parameters:
    - directory (str): Directory containing the geotiff files.
    - background_img_path (str): Path to the background image.
    - saved_directory (str): Directory to save the output plot.
    - fire_perim_fp (str): Filepath to the fire perimeter file.
    - fire_table_fp (str, optional): Filepath to the fire table CSV. Defaults to None.
    
    Returns:
    None. Saves a plot to the specified directory.
    """
    
    print(f"Plotting geotiffs from directory: {directory}")
    print(f"Using background image: {background_img_path}")
    print(f"Saving output to directory: {saved_directory}")
    print(f"Fire Perimeter : {fire_perim_fp}")

    geotiffs = glob.glob(os.path.join(directory, "*.tif"))
    print(f"Number of geotiffs: {len(geotiffs)}")

    with rasterio.open(background_img_path) as src:
        background_img = src.read(1)
        background_img = background_img[::10, ::10]
        print(src.meta)
        outProj = Proj(init='epsg:4326')  # The geographic coordinates CRS

    
    # Load GeoDataFrame from file
    df = gpd.read_file(fire_perim_fp, driver='GPKG')

    # Open the PNG image
    image_path = './rescaled_fire_perim.png'
    image = Image.open(image_path)

    # Convert the image to a numpy array
    image_array = np.array(image)

    # Apply the threshold
    # Create a binary mask for the perimeter
    perimeter_mask = (image_array > 0)

    # Apply a thicker edge by dilating the perimeter mask
    from scipy.ndimage import binary_dilation
    thicker_perimeter_mask = binary_dilation(perimeter_mask, iterations=3)

    # Set the perimeter pixels to white in the original image
    image_array[thicker_perimeter_mask] = 255

    # Invert the image
    inverted_image = 255 - image_array
    image = inverted_image
    image = np.array(image)
    image = np.where(image == 0, 255, np.nan)
    image = image[::10, ::10]
    print(f'FIRE PERIM SHAPE: {image.shape}')
    #bbox = box(*bounds)  # the * operator unpacks the list

    # Perform the spatial filtering

    bounds = [-119.48573401,  36.99620848, -119.12688445,   37.50860408]
    fire_id = 3763

    df_filtered = df.loc[df['fireID'] == fire_id]
    df_filtered = df_filtered.reset_index(drop=True)


    gdf_viirs_perim = df_filtered
    gdf_viirs_perim = gdf_viirs_perim.to_crs("EPSG:32611")
    gdf_viirs_perim['time'] = pd.to_datetime(gdf_viirs_perim['time'])

    # Get the unique dates from the gdf_viirs_perim
    # Extract date and hour from 'time' column
    gdf_viirs_perim['date'] = gdf_viirs_perim['time'].dt.date.astype(str)
    gdf_viirs_perim['hour'] = gdf_viirs_perim['time'].dt.hour.astype(str)
    

    date='2020-09-08'
    gdf_date = gdf_viirs_perim[gdf_viirs_perim['date'] == date].to_crs("EPSG:32611")
    print(gdf_date.head())
    print(gdf_date.shape)
    #gdf_date = gdf_date.iloc[0]

    fire_table = None
    if fire_table_fp is not None:
        fire_table = pd.read_csv(fire_table_fp)
        fire_table = fire_table.set_index('Description')
        print(f'Successfully Loaded Fire Table: {fire_table.shape}')

    grouped_fire_table = fire_table.groupby('INPUTS')
    print(f"Plotting for {fire_table.shape[0]} images")
    # Compute total rows needed for the figure.
    total_rows = sum(len(group) // 4 + (len(group) % 4 > 0) for _, group in grouped_fire_table)
    # total_rows = 2
    fig, axs = plt.subplots(total_rows, 4, figsize=(16, 4 * total_rows), dpi=300, subplot_kw={'projection': ccrs.PlateCarree()})
    axs = axs.flatten()  # Flatten the axes array for easier indexing.

    plot_idx = 0  # Keep track of which subplot we're currently working with.
    for input_class, group in grouped_fire_table:
        print(f"Plotting for INPUTS = {input_class}")

        for i, (index, row) in enumerate(group.iterrows()):
            geotiff_name = index
            geotiff_path = os.path.join(directory, geotiff_name)
            row_index = fire_table.index.get_loc(geotiff_name)

            with rasterio.open(geotiff_path) as src:
                geotiff = src.read(1)

            #if i > 1: break
            axs[plot_idx].imshow(background_img, extent=(0, 1, 0, 1))
            #gdf_date.plot(ax=axs[plot_idx], color='red', alpha=0.5)
            geotiff[geotiff == 0] = None
            axs[plot_idx].imshow(geotiff, extent=(0, 1, 0, 1), alpha=0.5, label='predicted fire_spread')
            axs[plot_idx].imshow(image, extent=(0, 1, 0, 1))
            iou= row['Day 8 IOU']
            axs[plot_idx].set_title(f'Experiment ID: {row_index}, Area: {row["Day 8 Fire Area"]} Sq Km', fontsize=10, rotation=0, ha='center')
            axs[plot_idx].text(0.5, -0.1, f'IOU: -{iou}', ha='center', va='center', transform=axs[plot_idx].transAxes)
            

            subplot_height = axs[plot_idx].get_ylim()[1] - axs[plot_idx].get_ylim()[0]
            max_fontsize = subplot_height * 0.8  # Adjust the scaling factor as needed

            fontsize = min(10, max_fontsize)  # Limit the fontsize to a maximum of 10
            fontsize = 3.5
            print(f'cur image: {i}')
            #axs[plot_idx].text(1.01, 0.5, f'Run Parameters: Crown-{row["CROWN_FIRE_MODEL"]}, Spotting-{row["ENABLE_SPOTTING"]}, Winds-{row["WIND_CONFIG"]}, Inputs-{input_class}',
            #                    va='center', rotation='vertical', transform=axs[plot_idx].transAxes,
            #                    fontsize=fontsize)

            # Add gridlines
            gl = axs[plot_idx].gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                                         linewidth=2, color='gray', alpha=0.9, linestyle='--')

            gl.xformatter = LONGITUDE_FORMATTER
            gl.yformatter = LATITUDE_FORMATTER
            gl.top_labels = False
            gl.right_labels = False

            # Add scale
            axs[plot_idx].add_feature(cartopy.feature.BORDERS, edgecolor='black')
            fig.text(0, 0.5, 'AREA', va='center', rotation='vertical')

            plot_idx += 1  # Move to the next subplot.
        #break
    # Remove remaining unused subplots.
    for i in range(plot_idx, total_rows * 4):
        fig.delaxes(axs[i])

    fig.tight_layout()
    print(saved_directory)
    sp=os.path.join(saved_directory, "geotiffs_grid2.png")
    print(sp)
    plt.savefig(sp, dpi=300)



#This needs the fire_csv to be correct. Do not forget that
if __name__ == "__main__":
    # This code plots geoTIFF fire spread prediction images alongside metadata.  
    # It loads a fire perimeter polygon, filters to a specific fire ID and date.
    # It reads in geoTIFFs and plots them with the perimeter over a background map.  
    # The subplots are arranged in a grid and annotated with metadata from a CSV.
    # Finally it saves the resulting figure with the grid of geoTIFF subplots.
    
    parser = argparse.ArgumentParser()
    parser.add_argument("directory", help="Input directory of geotiffs.")
    parser.add_argument("background_img_path", help="File path of the background image.")
    parser.add_argument("saved_directory", help="Directory to save the output figure.")
    parser.add_argument("fire_csv", help="Directory to access fire data figure.")
    parser.add_argument("--fire_perim_fp", default="./LargeFires_2020.gpkg",
                        help="File path of the fire perimeter.")
    args = parser.parse_args()

    plot_geotiffs(args.directory, args.background_img_path, args.saved_directory, args.fire_perim_fp, args.fire_csv)
    
    print('Python finished running')