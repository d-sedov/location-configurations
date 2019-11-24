################################################################################
################################################################################
#
# FILE: split_additional.py
#
# BY: Dmitry Sedov 
#
# CREATED: Sat Nov 16 2019
#
# DESC: This file splits the list of CBGs for which additional requests need to
# be made.
#
# EXEC: python3 split_additional.py
#      
################################################################################
################################################################################


############################# Libraries #######################################

import pandas as pd
import logging
import os
import numpy as np

################################################################################


############################### Parameters #####################################

# Paths
cbgs_rest_coord_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/all/cbgs_rest_coord.csv'
add_folder_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/add_part/'
# Chunk size
chunksize = 4500

################################################################################


############################### Parameters #####################################

def ceildiv(a, b):
        return -(-a // b)

################################################################################


############################### Main code  #####################################

if __name__ == '__main__':

    # Logging settings
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    logger.addHandler(ch)

    # Import additional CBGs
    add_cbgs = pd.read_csv(os.path.join(add_folder_path, 'yelp-redo.csv'),
            usecols = ['cbg'], 
            dtype = {'cbg': pd.Int64Dtype()})

    # Import all CBGs to get coordinates
    all_cbgs = pd.read_csv(cbgs_rest_coord_path,
            usecols = ['cbg', 'lat', 'lon'],
            dtype = {'cbg': pd.Int64Dtype()})

    # Merge to get coordinates for additional CBGs
    add_cbgs = add_cbgs.merge(all_cbgs, on = 'cbg', how = 'left')
    total = add_cbgs.shape[0]

    chunks = ceildiv(total, chunksize)

    # Iterate through chunks of additional CBGs, write to disk.
    for i, chunk in enumerate(np.array_split(add_cbgs, chunks)):
        part_path = os.path.join(add_folder_path, 'to_do', f'chunk{i}.csv')
        logger.info('Writing part {} to file {}.'.format(i, part_path))
        chunk.to_csv(path_or_buf = part_path, header = False, index = False)

################################################################################
