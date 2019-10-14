################################################################################
################################################################################
#
# FILE: 
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Oct 14 2019
#
# DESC: 
#
# EXEC:
#      
################################################################################
################################################################################


############################# Libraries ########################################

import logging # Facilitates logging 
import requests # Facilitates making http requests
import json # Facilitates handling of json objects

################################################################################


######################## Functions and classes #################################

def construct_search_params(part, lat, lon):
    """ This function constructs the search request parameters. """
    params = {}
    params['latitude'] = lat
    params['longitude'] = lon
    params['term'] = 'food'
    params['limit'] = 50
    return params


class Yelp_Client:
    """ This class facilitates making simplified Yelp API requests. """

    def __init__(self, api_key):
        # Initiate instance of a class with API key, logger and urls
        self.api_key = api_key
        self.headers = {'Authorization': 'Bearer %s' % self.api_key}
        self.search_url = 'https://api.yelp.com/v3/businesses/search'
        self.logger = logging.getLogger(__name__)

    def set_output_folder_path(self, output_folder_path):
        # Set the folder path for output files
        self.output_folder_path = output_folder_path

    def set_input_file_path(self, input_file_path):
        # Set the file path for input files
        self.input_file_path = input_file_path

    def search(self, params):
        # Make a single search request with a specified parameter
        resp = requests.get(url = self.search_url,
                params = params,
                headers = self.headers
                )
        return resp

    def search_from_file():
        # Make multiple search requests reading parameters from file
        # Check if input and output locations are set
        if (hasattr(self, 'input_file_path') and
                hasattr(self, 'output_folder_path')):
            with open(input_file_path, 'r') as input_file:
                for line in input_file:
                    line = line.strip().split(',')
                    part = line[0]
                    params = construct_search_params(*line)
                    try:
                        yelp_response = self.search(params).json()
                        output_file_path = (self.output_folder_path +
                                'part' + part + '.json')
                        with open(output_folder_path, 'w+') as output_file:
                            json.dump(output_file, yelp_response)
                    except (SystemError, KeyboardInterrupt):
                        raise
                    except Exception:
                        self.logger.error('Error getting response for part '
                                + part + '.', exc_info = True)
        else:
            self.logger.error('Input or output paths are not set.'
                    + 'No requests made.')

################################################################################


############################# Main code ########################################

if __name__ = '__main__':
    # Do a simple test run if program executed as main code

    # Set parameters
    api_key = '***REMOVED***i'
    download_folder_path = 
    input_file_path = 
    log_file_path =

    # Initiate the yelp client
    searcher = Yelp_Client(api_key = api_key)
    searcher.set_input_file_path(input_file_path)
    searcher.set_output_folder_path(output_folder_path)

    # Set the logging options
    searcher.logger.setLevel(logging.DEBUG)
    # Write log to file (DEBUG)
    fh = logging.FileHandler(log_file_path)
    fh.setLevel(logging.DEBUG)
    # Write log to console (ERROR)
    ch = logging.StreamHandler()
    ch.setLevel(logging.ERROR)
    # Create formatter and add it to the handlers
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    fh.setFormatter(formatter)
    ch.setFormatter(formatter)
    # Add the handlers to the logger
    searcher.logger.addHandler(fh)
    searcher.logger.addHandler(ch)

    # Do the search
    seacher.search_from_file()

################################################################################
