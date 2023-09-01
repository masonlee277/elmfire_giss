import os
import glob
import numpy as np
import rasterio
from rasterio.transform import from_origin
import scipy

def process_asc_files(directory_path: str, ws_original: str, wd_original: str, num_bands: int = 1) -> None:
    """
    Processes ASC files and converts them to GeoTIFF format.

    This function takes a directory containing ASC files for wind speed (_vel.asc) and wind direction (_ang.asc),
    and converts them into multiband GeoTIFF files. The original wind speed and wind direction TIFF files are used
    to obtain metadata for the conversion. The function also allows resizing the arrays and selecting a specific number
    of bands.

    Parameters:
    directory_path (str): The path to the directory containing the ASC files.
    ws_original (str): The path to the original wind speed TIFF file, used to obtain metadata.
    wd_original (str): The path to the original wind direction TIFF file, used to obtain metadata.
    num_bands (int, optional): The number of bands to process. Defaults to 1.

    Returns:
    None: The function writes the processed GeoTIFF files to the specified directory and does not return a value.

    Example:
    process_asc_files('/path/to/directory', '/path/to/ws_original.tif', '/path/to/wd_original.tif', num_bands=5)
    """

    # Find all the _vel.asc and _ang.asc files in the directory
    vel_files = glob.glob(os.path.join(directory_path, "*non_neutral_stability_vel.asc"))
    ang_files = glob.glob(os.path.join(directory_path, "*non_neutral_stability_ang.asc"))

    # Function to convert ASC to numpy array and return the data along with metadata
    def asc_to_np(file_path):
        with rasterio.open(file_path) as src:
            metadata = src.meta
            data = src.read(1)
        return data, metadata

    # Process velocity files
    ws_arrays = [asc_to_np(file)[0] for file in vel_files]
    with rasterio.open(ws_original) as src:
        ws_meta = src.meta
    ws_meta.update(count=len(ws_arrays))
    ws_multiband_array = np.dstack(ws_arrays)
    ws_multiband_array = np.moveaxis(ws_multiband_array, -1, 0)
    ws_multiband_array = scipy.ndimage.zoom(ws_multiband_array, (1, 200/ws_multiband_array.shape[1], 200/ws_multiband_array.shape[2]))
    ws_multiband_array = ws_multiband_array[:num_bands, :, :]
    ws_meta.update(count=ws_multiband_array.shape[0])
    print(ws_multiband_array.shape)
    with rasterio.open(os.path.join(directory_path, 'ws.tif'), 'w', **ws_meta) as dst:
        dst.write(ws_multiband_array)

    # Process angle files
    wd_arrays = [asc_to_np(file)[0] for file in ang_files]
    with rasterio.open(wd_original) as src:
        wd_meta = src.meta
    wd_meta.update(count=len(wd_arrays))
    wd_multiband_array = np.dstack(wd_arrays)
    wd_multiband_array = np.moveaxis(wd_multiband_array, -1, 0)
    wd_multiband_array = scipy.ndimage.zoom(wd_multiband_array, (1, 200/wd_multiband_array.shape[1], 200/wd_multiband_array.shape[2]))
    wd_multiband_array = wd_multiband_array[:num_bands, :, :]
    wd_meta.update(count=wd_multiband_array.shape[0])
    print(wd_multiband_array.shape)
    with rasterio.open(os.path.join(directory_path, 'wd.tif'), 'w', **wd_meta) as dst:
        dst.write(wd_multiband_array)

import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process ASC files and convert them to GeoTIFF format.")
    parser.add_argument("directory_path", help="Directory where the ASC files are located.")
    parser.add_argument("ws_original", help="Path to the original wind speed TIFF file.")
    parser.add_argument("wd_original", help="Path to the original wind direction TIFF file.")
    parser.add_argument("--num_bands", type=int, default=1, help="Number of bands to process (optional, default is 1).")

    args = parser.parse_args()

    process_asc_files(args.directory_path, args.ws_original, args.wd_original, args.num_bands)
