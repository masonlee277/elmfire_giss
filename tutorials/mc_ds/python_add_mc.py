import argparse

def python_add_mc(file_path, CH, CBD, CC, CBH, WS, WD,M1, M10, M100):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    start = None
    end = None

    for i, line in enumerate(lines):
        if line.strip() == '&MONTE_CARLO':
            start = i
        if line.strip() == '/' and start is not None:
            end = i
            break

    assert start is not None and end is not None, "MONTE_CARLO block not found"
    print(f'start: {start} , end: {end}')

    new_block = f"""
&MONTE_CARLO
METEOROLOGY_BAND_START                   = 1
METEOROLOGY_BAND_STOP                    = 1
METEOROLOGY_BAND_SKIP_INTERVAL           = 1
NUM_METEOROLOGY_TIMES                    = 97
EDGEBUFFER                               = 30.
RANDOM_IGNITIONS                         = .FALSE.
USE_IGNITION_MASK                        = .TRUE.
USE_ERC                                  = .FALSE.
ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL      = .TRUE.
NUM_ENSEMBLE_MEMBERS                     = 1
SEED                                     = 2021
WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN = 0
WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX = 0
WIND_SPEED_FLUCTUATION_INTENSITY_MIN     = 0.0
WIND_SPEED_FLUCTUATION_INTENSITY_MAX     = 0.0
NUM_RASTERS_TO_PERTURB                   = 9
RASTER_TO_PERTURB(1)                     = 'CC'
SPATIAL_PERTURBATION(1)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(1)                 = 'STATIC'
PDF_TYPE(1)                              = 'UNIFORM'
PDF_LOWER_LIMIT(1)                       = {CC}
PDF_UPPER_LIMIT(1)                       = {CC}
RASTER_TO_PERTURB(2)                     = 'WS'
SPATIAL_PERTURBATION(2)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(2)                 = 'STATIC'
PDF_TYPE(2)                              = 'UNIFORM'
PDF_LOWER_LIMIT(2)                       = {WS}
PDF_UPPER_LIMIT(2)                       = {WS}
RASTER_TO_PERTURB(3)                     = 'WD'
SPATIAL_PERTURBATION(3)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(3)                 = 'STATIC'
PDF_TYPE(3)                              = 'UNIFORM'
PDF_LOWER_LIMIT(3)                       = {WD}
PDF_UPPER_LIMIT(3)                       = {WD}
RASTER_TO_PERTURB(4)                     = 'CBH'
SPATIAL_PERTURBATION(4)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(4)                 = 'STATIC'
PDF_TYPE(4)                              = 'UNIFORM'
PDF_LOWER_LIMIT(4)                       = {CBH}
PDF_UPPER_LIMIT(4)                       = {CBH}
RASTER_TO_PERTURB(5)                     = 'CBD'
SPATIAL_PERTURBATION(5)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(5)                 = 'STATIC'
PDF_TYPE(5)                              = 'UNIFORM'
PDF_LOWER_LIMIT(5)                       = {CBD}
PDF_UPPER_LIMIT(5)                       = {CBD}
RASTER_TO_PERTURB(6)                     = 'CH'
SPATIAL_PERTURBATION(6)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(6)                 = 'STATIC'
PDF_TYPE(6)                              = 'UNIFORM'
PDF_LOWER_LIMIT(6)                       = {CH}
PDF_UPPER_LIMIT(6)                       = {CH}
RASTER_TO_PERTURB(7)                     = 'M1'
SPATIAL_PERTURBATION(7)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(7)                 = 'STATIC'
PDF_TYPE(7)                              = 'UNIFORM'
PDF_LOWER_LIMIT(7)                       = {M1}
PDF_UPPER_LIMIT(7)                       = {M1}
RASTER_TO_PERTURB(8)                     = 'M10'
SPATIAL_PERTURBATION(8)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(8)                 = 'STATIC'
PDF_TYPE(8)                              = 'UNIFORM'
PDF_LOWER_LIMIT(8)                       = {M10}
PDF_UPPER_LIMIT(8)                       = {M10}
RASTER_TO_PERTURB(9)                     = 'M100'
SPATIAL_PERTURBATION(9)                  = 'GLOBAL'
TEMPORAL_PERTURBATION(9)                 = 'STATIC'
PDF_TYPE(9)                              = 'UNIFORM'
PDF_LOWER_LIMIT(9)                       = {M100}
PDF_UPPER_LIMIT(9)                       = {M100}
/
"""

    lines[start:end+1] = new_block.splitlines(True)

    with open(file_path, 'w') as f:
        f.writelines(lines)

    print('finished adding MC block using python')


def main():
    """
    This script updates the MONTE_CARLO block in a specified input file with new parameter values.
    The MONTE_CARLO block is a section in the input file used for configuring simulation parameters
    for a wildfire modeling application.

    The script takes the following command-line arguments:
    - --file_path: Path to the input file that needs to be modified.
    - --CH, --CBD, --CC, --CBH, --WS, --WD, --M1, --M10, --M100: New parameter values to be set
    within the MONTE_CARLO block.

    The process involves the following steps:
    1. The script reads the content of the input file.
    2. It searches for the start and end markers of the MONTE_CARLO block within the content.
    3. If the block is found, it constructs a new MONTE_CARLO block with the specified parameter
    values.
    4. The new block is inserted in place of the old MONTE_CARLO block.
    5. The modified content is written back to the input file.

    The MONTE_CARLO block is structured with various parameter settings for meteorology, spatial
    perturbations, temporal perturbations, and probability distribution functions. The script
    replaces specific parameter values within this block with the provided values.

    After running this script, the input file will have the MONTE_CARLO block updated with the new
    parameter values. This allows for configuring and running wildfire simulations with different
    sets of parameter values using the same input file.
    """

    parser = argparse.ArgumentParser(description='Update MONTE_CARLO block.')
    parser.add_argument('--file_path', type=str, required=True, help='File path to modify.')
    parser.add_argument('--CH', type=float, required=True, help='Value for CH.')
    parser.add_argument('--CBD', type=float, required=True, help='Value for CBD.')
    parser.add_argument('--CC', type=float, required=True, help='Value for CC.')
    parser.add_argument('--CBH', type=float, required=True, help='Value for CBH.')
    parser.add_argument('--WS', type=float, required=True, help='Value for WS.')
    parser.add_argument('--WD', type=float, required=True, help='Value for WD.')
    parser.add_argument('--M1', type=float, required=True, help='Value for WS.')
    parser.add_argument('--M10', type=float, required=True, help='Value for WD.')
    parser.add_argument('--M100', type=float, required=True, help='Value for WS.')
    args = parser.parse_args()

    python_add_mc(args.file_path, args.CH, args.CBD, args.CC, args.CBH, args.WS, args.WD, args.M1, args.M10, args.M100)

if __name__ == "__main__":
    main()