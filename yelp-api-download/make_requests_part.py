################################################################################
################################################################################
#
# FILE: make_requests_part.py
#
# BY: Dmitry Sedov 
#
# CREATED: Sun Oct 13 2019
#
# DESC: This file contains code that reads input from the command line and 
#       sends Yelp API requests based on this input.
#
# EXEC: python3 make_requests_part.py $api_key $input_file_path $output_file_path $log_file_path
#      
################################################################################
################################################################################


############################## Libraries #######################################  

import yelp_client
import sys
import logging

################################################################################  


############################## Main code #######################################

if __name__ == '__main__':

    # Read inputs from command line
    api_key = sys.argv[1]
    input_file_path = sys.argv[2]
    output_folder_path = sys.argv[3]
    log_file_path = sys.argv[4]

    # Initiate the API client
    searcher = yelp_client.Yelp_Client(api_key)
    searcher.set_input_file_path(input_file_path)
    searcher.set_output_folder_path(output_folder_path)

    # Set the logging options
    logger = searcher.logger
    logger.setLevel(logging.INFO)
    # Write log to file (DEBUG)
    fh = logging.FileHandler(log_file_path)
    fh.setLevel(logging.INFO)
    # Write log to console (ERROR)
    ch = logging.StreamHandler()
    ch.setLevel(logging.ERROR)
    # Create formatter and add it to the handlers
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    fh.setFormatter(formatter)
    ch.setFormatter(formatter)
    # Add the handlers to the logger
    logger.addHandler(fh)
    logger.addHandler(ch)

    # Do the search
    logger.info('Making requests based on input file ' + input_file_path + '.')
    searcher.search_from_file()
    
################################################################################  
