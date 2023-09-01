import numpy as np
import matplotlib.pyplot as plt
import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
import os
import imageio
import shutil
import argparse
import glob

def open_geotiff(filepath, dst_crs, transform, width, height, fire=False):
    """
    Opens a GeoTiff file and reprojects it to a new coordinate reference system (CRS) and size.

    Inputs:
        filepath (str): Path to the GeoTiff file to be opened.
        dst_crs (str or CRS): Target coordinate reference system for reprojection.
        transform (Affine): Target transformation matrix for reprojection.
        width (int): Width of the reprojected image.
        height (int): Height of the reprojected image.
        fire (bool, optional): Flag to indicate if the GeoTiff represents fire data. Default is False.

    Outputs:
        image (numpy.ndarray): Numpy array representing the reprojected image data.
        nodata (float or None): Nodata value from the original GeoTiff file, or None if 'fire' is False.

    Functionality:
        - The function opens the GeoTiff file specified by 'filepath'.
        - It reprojects the GeoTiff data to the target CRS and size using bilinear resampling.
        - If 'fire' is True, it processes the GeoTiff data representing fire, handling nodata values.
        - The function returns the reprojected image data and nodata value (if 'fire' is True).

    Parameters:
        - filepath: Path to the GeoTiff file to be opened (str).
        - dst_crs: Target CRS for reprojection (str or rasterio.crs.CRS).
        - transform: Target transformation matrix for reprojection (rasterio.transform.Affine).
        - width: Width of the reprojected image (int).
        - height: Height of the reprojected image (int).
        - fire: Flag to indicate if the GeoTiff represents fire data (bool, optional).

    Example Usage:
        image_data, nodata_value = open_geotiff('input.tif', 'EPSG:4326', transform, 512, 512, fire=True)
    """
    
    with rasterio.open(filepath) as src:
        kwargs = src.meta.copy()
        kwargs.update({
            'crs': dst_crs,
            'transform': transform,
            'width': width,
            'height': height
        })

        
        image = np.empty((src.count, height, width), dtype=rasterio.float32)
        
        reproject(
            source=rasterio.band(src, np.arange(1, src.count + 1)),
            destination=image,
            src_transform=src.transform,
            src_crs=src.crs,
            dst_transform=transform,
            dst_crs=dst_crs,
            resampling=Resampling.bilinear)
        nodata=None
        if fire:
          print('fire image')
          nodata = src.nodata
          image = np.where(image == nodata, 0, image)
          nodata_mask = np.where(image == nodata, 1, 0)

          # Plot the mask
          #plt.imshow(np.squeeze(nodata_mask), cmap='gray')
          #plt.title('Nodata mask')
          #plt.show()

    return image, nodata

def plot_wind_vector(wind_speed_file, wind_dir_file, fire_file, asp_fp, save_path, skip_bands=10):
    """
    Plots wind vectors over a fire animation using quiver plots.
    
    Inputs:
        wind_speed_file (str): Path to the GeoTiff file containing wind speed data.
        wind_dir_file (str): Path to the GeoTiff file containing wind direction data.
        fire_file (str): Path to the GeoTiff file containing fire data.
        asp_fp (str): Path to the GeoTiff file containing aspect (asp) data.
        save_path (str): Path to the directory where the resulting animation frames will be saved.
        skip_bands (int, optional): Number of time steps to skip while creating the animation. Default is 10.
        
    Outputs:
        None
        
    Functionality:
        - The function reads wind speed, wind direction, fire, and aspect data from respective GeoTiff files.
        - It sets up a grid of quiver plots to visualize wind vectors for different time steps.
        - Fire animation frames with wind vectors are saved to the specified 'save_path' directory.
        - The final animation is created using ImageIO and saved to 'save_path'.
        - The function also displays intermediate visualizations of wind vectors over the fire animation.

    Parameters:
        - wind_speed_file: GeoTiff file containing wind speed data (e.g., 'wind_speed.tif').
        - wind_dir_file: GeoTiff file containing wind direction data (e.g., 'wind_direction.tif').
        - fire_file: GeoTiff file containing fire data (e.g., 'fire_data.tif').
        - asp_fp: GeoTiff file containing aspect data (e.g., 'aspect.tif').
        - save_path: Directory path to save animation frames and the final animation (e.g., './animations/').
        - skip_bands: Number of time steps to skip while creating the animation. Default is 10 (int).

    Example Usage:
        plot_wind_vector('wind_speed.tif', 'wind_direction.tif', 'fire_data.tif', 'aspect.tif', './animations/', skip_bands=5)
    """
    
    if not os.path.isdir(save_path):
        os.makedirs(save_path, exist_ok=True)

    # Get target CRS and resolution from first file
    with rasterio.open(wind_speed_file) as src:
        target_transform, target_width, target_height = calculate_default_transform(
            src.crs, src.crs, src.width, src.height, *src.bounds)
        target_crs = src.crs

    ### To Downscale the image (not time skip)
    skip = 4 

    # Open GeoTiff files
    wind_speed,_ = open_geotiff(wind_speed_file, target_crs, target_transform, target_width, target_height)
    wind_dir,_ = open_geotiff(wind_dir_file, target_crs, target_transform, target_width, target_height)
    print(f'wind shape: {np.shape(wind_dir)}')
    fire, nodata = open_geotiff(fire_file, target_crs, target_transform, target_width, target_height, fire=True)
    fire = np.squeeze(fire)

    asp,_ = open_geotiff(asp_fp, target_crs, target_transform, target_width, target_height)
    asp = np.squeeze(asp)

    mask  = np.where(fire > 1 , True, False) # set all values of the where fire is greater than
    cur_fire = np.where(mask, fire, 1)  # 1 is white, 0 is black
    sec = 200000
    cur_fire = np.where((2 < cur_fire) & (cur_fire < sec ), 1, np.nan)  # 1 is white, 0 is black
    
    plt.imshow(asp)
    plt.imshow(cur_fire, cmap='gray')
    plt.show()
    
    
    timesteps = wind_speed.shape[0]
    bands_to_plot = range(0, timesteps, skip_bands)

    # Define subplot grid size
    subplot_side = int(np.ceil(np.sqrt(len(bands_to_plot))))
    #fig, axs = plt.subplots(subplot_side, subplot_side, figsize=(15,15))
    
    simulation_time = np.max(fire)
    timesteps = wind_dir.shape[0]
    dt = simulation_time / timesteps
    
    fire_filename = os.path.basename(fire_file)
    temp_folder_path = 'temp'
    os.makedirs(temp_folder_path, exist_ok=True)
    # Get a list of files in the temp folder
    file_list = os.listdir(temp_folder_path)

    # Loop through the files and remove each one
    for file in file_list:
        file_path = os.path.join(temp_folder_path, file)
        os.remove(file_path)

    figs = []
    for index, band in enumerate(bands_to_plot):
        # Convert wind speed and direction to u and v components for the band
        wind_dir_rad = np.deg2rad(wind_dir[band])  # Convert to radians
        u = -wind_speed[band] * np.sin(wind_dir_rad)  # u component (East-West)
        v = -wind_speed[band] * np.cos(wind_dir_rad)  # v component (North-South)

        # Create a grid for quiver
        x = np.arange(0, u.shape[1], 1)
        y = np.arange(0, u.shape[0], 1)
        X, Y = np.meshgrid(x, y)

        # Downsample data for better visualization
        X = X[::skip, ::skip]
        Y = Y[::skip, ::skip]
        u = u[::skip, ::skip]
        v = v[::skip, ::skip]

        # Calculate subplot index
        ax_row = index // subplot_side
        ax_col = index % subplot_side
        #ax = axs[ax_row, ax_col]

        #Plot Fire:
        sec = band*dt
        cur_fire = np.where(mask, fire, 1)  # 1 is white, 0 is black
        cur_fire = np.where((2 < cur_fire) & (cur_fire < sec ), 1, np.nan)  # 1 is white, 0 is black
      
        # Create a separate figure for the current plot
        fig, ax = plt.subplots()
        
        ax.imshow(asp)
        ax.imshow(cur_fire, cmap='gray')

        # Create plot
        ax.quiver(X, Y, u, v, angles='xy', color='white')
        ax.set_title(fire_filename, loc='center', y=-0.10, fontsize=6)
        ax.set_title(f'Simulation Hour = {round(sec/3600, 1)}')
        # After the plotting code for each band, save the figure
        filename = f'frame_{index}.png'
        filepath = './' + os.path.join(temp_folder_path, filename)
        plt.savefig(filepath)
        figs.append(filepath)
        print(filepath, f'Simulation Hour = {round(sec/3600, 1)}')

        # Close the figure to release its memory
        plt.close(fig)
    
    print(os.listdir(temp_folder_path))
    
    file_path = os.path.join(save_path, 'test_gif.gif')

    # Making sure that all the images are correctly loaded
    images = []
    for fig in figs:
        try:
            images.append(imageio.imread(fig))
        except Exception as e:
            print(f"Error reading image file '{fig}': {e}")
            break

    # If no errors occurred, we save the GIF with looping
    if images:
        try:
            imageio.mimsave(file_path, images, duration=1, loop=0, subrectangles=True)
        except Exception as e:
            print(f"Error saving animation to path '{file_path}': {e}")

    print('completed making gif')


if __name__ == "__main__":

    """
    This script processes GeoTiff files to visualize wind vectors over a fire animation using quiver plots.

    The script performs the following tasks:
    1. Reads wind speed, wind direction, fire, and aspect data from respective GeoTiff files.
    2. Sets up a grid of quiver plots to visualize wind vectors for different time steps.
    3. Creates fire animation frames with wind vectors and saves them to a specified directory.
    4. Uses the ImageIO library to create a final GIF animation from the saved frames.
    5. Displays intermediate visualizations of wind vectors over the fire animation.

    The script contains two main functions:

    1. `open_geotiff(filepath, dst_crs, transform, width, height, fire=False)`:
    - Opens a GeoTiff file and reprojects it to a new CRS and size.
    - Handles nodata values if the file represents fire data.
    - Returns the reprojected image data and nodata value (if applicable).

    2. `plot_wind_vector(wind_speed_file, wind_dir_file, fire_file, asp_fp, save_path, skip_bands=10)`:
    - Reads input GeoTiff files for wind speed, wind direction, fire, and aspect data.
    - Sets up quiver plots for visualizing wind vectors and fire animation.
    - Creates and saves fire animation frames with wind vectors to a specified directory.
    - Combines the saved frames into a GIF animation using ImageIO.
    - Displays visualizations of wind vectors over the fire animation.

    Command-line arguments for the script:
    - wind_speed_file: Path to the GeoTiff file containing wind speed data.
    - wind_dir_file: Path to the GeoTiff file containing wind direction data.
    - fire_file: Path to the GeoTiff file containing fire data.
    - asp_fp: Path to the GeoTiff file containing aspect (asp) data.
    - save_path: Directory path to save animation frames and the final animation.
    - --skip_bands: Number of time steps to skip while creating the animation. Default is 10.

    Example Usage:
    python script_name.py wind_speed.tif wind_direction.tif fire_data.tif aspect.tif ./animations/ --skip_bands=5
    """

    parser = argparse.ArgumentParser(description="Plot wind vector with fire perimeter")
    parser.add_argument("wind_speed_file", help="Path to the wind speed GeoTIFF file")
    parser.add_argument("wind_dir_file", help="Path to the wind direction GeoTIFF file")
    parser.add_argument("fire_file", help="Path to the fire perimeter GeoTIFF file")
    parser.add_argument("asp_fp", help="Path to the aspect GeoTIFF file")
    parser.add_argument("save_path", help="Path to save the output GIF")
    parser.add_argument("--skip_bands", type=int, default=10, help="Number of bands to skip")
    args = parser.parse_args()

    plot_wind_vector(args.wind_speed_file, args.wind_dir_file, args.fire_file, args.asp_fp, args.save_path, args.skip_bands)
