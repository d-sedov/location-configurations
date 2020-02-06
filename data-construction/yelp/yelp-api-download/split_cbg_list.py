################################################################################
################################################################################
#
# FILE: split_cbg_list.py
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Oct 15 2019
#
# DESC: This file splits the list of CBGs with restaurants into parts.
#
# EXEC: python3 split_cbg_list.py
#      
################################################################################
################################################################################


############################# Libraries #######################################

import pandas as pd
import logging

################################################################################


############################### Parameters #####################################

# Paths
cbgs_rest_coord_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/all/cbgs_rest_coord.csv'
do_folder_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/do_part/to_do/'
ta_folder_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/ta_part/to_do/'
ds_folder_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/ds_part/to_do/'
folder_paths = {0 : do_folder_path,
        1 : ta_folder_path,
        2 : ds_folder_path}

# Chunk size
chunksize = 4500

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

    # Iterate through chunks, write them to separate files
    # Omit the quantity column
    for i, chunk in enumerate(pd.read_csv(cbgs_rest_coord_path,
        usecols = ['cbg', 'lat', 'lon'],
        chunksize = chunksize)):
        part_path = folder_paths[i % 3] + 'chunk{}.csv'.format(i) 
        logger.info('Writing part {} to file {}.'.format(i, part_path))
        chunk.to_csv(path_or_buf = part_path, header = False, index = False)

################################################################################
