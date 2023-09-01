import argparse
import os
import math
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
import rasterio
from rasterio.plot import show

from matplotlib import colors
from mpl_toolkits.axes_grid1 import make_axes_locatable

landfire_dict = {
    "adj": "Spread rate adjustment factor (-)",
    "cbd": "Canopy bulk density (kg/m³)",
    "cbh": "Canopy base height (m)",
    "cc": "Canopy cover (%)",
    "ch": "Canopy height (m)",
    "fmc": "Foliar moisture content (%)",
    "m1": "1-hour fuel moisture (%)",
    "m10": "10-hour fuel moisture (%)",
    "m100": "100-hour fuel moisture (%)",
    "mlh": "Live herbaceous fuel moisture (%)",
    "mlw": "Live woody fuel moisture (%)",
    "waf": "Wind adjustment factor (-)",
    "wd": "Wind direction (degrees)",
    "ws": "Wind speed (mph)",
    "dem": "Digital Elevation Map (Feet)",
    "asp": "Aspect (degrees clockwise)"
}

def plot_geotiffs(input_dir_path, output_dir_path):
    print(input_dir_path)
    # Set the input and output directories
    input_dir = Path(input_dir_path)
    output_dir = Path(output_dir_path)

    # Create the output directory if it does not exist
    output_dir.mkdir(parents=True, exist_ok=True)

    # List all geotiff files in the input directory
    geotiff_files = list(input_dir.glob('*.tif'))

    # Compute grid size (square root of number of files, rounded up)
    grid_size = math.ceil(math.sqrt(len(geotiff_files)))

    # Create figure and axes
    fig, axs = plt.subplots(grid_size, grid_size, figsize=(30, 30))

    # Make the subplots square
    plt.subplots_adjust(wspace=0, hspace=0)

    # Set the figure title as the folder name
    fig.suptitle(input_dir.name, fontsize=35)

    # Loop through each file
    for ax, geotiff_file in zip(axs.flatten(), geotiff_files):
        # Open the raster
        dataset = rasterio.open(geotiff_file)

        # Create a divider for the existing axes instance
        divider = make_axes_locatable(ax)
        cax = divider.append_axes("right", size="5%", pad=0.05)
        
        # Plot the raster and colorbar
        im = ax.imshow(dataset.read(1), cmap='viridis')
        fig.colorbar(im, cax=cax, orientation='vertical')

        # Set the title as the filename and its corresponding unit from the dictionary
        ax.set_title(f"{geotiff_file.stem}: {landfire_dict.get(geotiff_file.stem, '')}, shape: {np.shape(dataset.read(1))}")
        
    # Hide unused subplots
    for ax in axs.flatten()[len(geotiff_files):]:
        ax.axis('off')

    plt.tight_layout()
    # Save the plot to the output directory
    plt.savefig(output_dir / f"{input_dir.name}_grid_plot.png")


if __name__ == "__main__":
    """
    Create a square gridded figure of GeoTIFFs in a specified folder.

    This script reads a directory containing GeoTIFF files, creates a square grid layout of subplots, and plots each GeoTIFF image along with its corresponding metadata. The resulting composite figure is saved to an output directory.

    Args:
        input_dir_path (str): Path to the input directory containing GeoTIFF files.
        output_dir_path (str): Path to the output directory where the generated figure will be saved.

    Outputs:
        None

    Functionality:
        - Reads GeoTIFF files from the input directory.
        - Computes the grid size for the subplots based on the number of files.
        - Creates a figure with a square grid layout and adjusts subplot spacing.
        - Plots each GeoTIFF image on a subplot along with a colorbar.
        - Sets the title of each subplot as the filename and its corresponding unit (if available) from a predefined dictionary.
        - Saves the composite figure to the specified output directory.

    Example Usage:
        python script.py -i input_directory -o output_directory
    """
    parser = argparse.ArgumentParser(description="Create a square gridded figure of GeoTIFFs in a folder.")
    parser.add_argument("-i", "--input", type=str, help="Path to the input directory.")
    parser.add_argument("-o", "--output", type=str, help="Path to the output directory.")
    args = parser.parse_args()
    plot_geotiffs(args.input, args.output)
