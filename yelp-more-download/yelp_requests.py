################################################################################
################################################################################
#
# FILE: yelp_requests.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Dec 11 2019
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
import get_records # Facilitates reading phones from the restaurant requests table
import sqlalchemy as db
import psycopg2
import pandas as pd
import sys

################################################################################

############################# Constants ########################################

database_fields_level_1 = [
        'id',
        'name',
        'phone',
        'price',
        'review_count',
        'rating',
        'categories',
        'is_closed'
        ]

database_fields_level_2 = [
        'country',
        'state',
        'city',
        'zip_code',
        'address1'
        ]

database_fields_level_3 = [
        'longitude',
        'latitude'
        ]

################################################################################


########################### SQL Statements #####################################

create_status_update_table_statement = """
CREATE TEMPORARY TABLE status_update (row_id INTEGER, request_status VARCHAR, error_message VARCHAR) 
ON COMMIT DROP
"""

insert_status_update_table_statement = """
INSERT INTO status_update (row_id, request_status, error_message) 
VALUES(%s, %s, %s)
"""

update_restaurants_requests_statement = """
UPDATE restaurants_requests
SET 
    phone_request_status = status_update.request_status,
    phone_error_message = status_update.error_message
FROM status_update
WHERE 
    status_update.row_id = restaurants_requests.row_id;
"""

################################################################################


######################## Functions and classes #################################

class Yelp_Match:
    """ This class facilitates requests to Yelp by phone number or address. """

    def __init__(self, api_key, match_type = 'phone'):

        # Make sure the match type is valid
        assert match_type in ('phone', 'address')

        # Initiate instance of a class with API key, logger and urls
        self.api_key = api_key
        self.headers = {'Authorization': 'Bearer %s' % self.api_key}
        if (match_type == 'phone'):
            self.search_url = 'https://api.yelp.com/v3/businesses/search/phone'
        self.logger = logging.getLogger(__name__)

    def set_output_folder_path(self, output_folder_path):
        # Set the folder path for output files
        self.output_folder_path = output_folder_path
        self.logger.debug('Output folder set to ' + output_folder_path)

    def search_by_phone(self, phone):
        # Send the phone number request
        params = {'phone' : phone}
        response = requests.get(url = self.search_url,
                params = params,
                headers = self.headers
                )
        self.json = response.json()
        return self.json

    def json_to_file(self, item_number):
        # Write json response to disk
        output_file_path = (self.output_folder_path +
                'item' + item_number + '.json')
        with open(output_file_path, 'w+') as output_file:
            json.dump(self.json, output_file)
            self.logger.debug('Information for item ' + item_number + ' obtained.')
        return None


def request_phone_save(row):
    """ This function makes a request and parses response based on phone. """

    item_number = row['row_id']
    sg_id = row['sname_place_id']
    phone = row['r_phone_number']
    response = searcher.search_by_phone(phone)
    searcher.json_to_file(str(item_number))

    if (not ('businesses' in response)):
        # If no businesses in response mark result as no success
        values = {}
        values['row_id'] = item_number
        values['sname_place_id'] = sg_id
        values['request_status'] = 'zero'
        values['error_message'] = None
        if 'error' in response:
            # If an error in response, save error message
            values['request_status'] = 'error'
            values['error_message'] = response['error']['description']
        for f in database_fields_level_1:
            values[f] = None
        for f in database_fields_level_2:
            values[f] = None
        for f in database_fields_level_3:
            values[f] = None
        return pd.DataFrame([values])

    if (len(response['businesses']) == 0):
        values = {}
        values['row_id'] = item_number
        values['sname_place_id'] = sg_id
        values['request_status'] = 'zero'
        values['error_message'] = None
        for f in database_fields_level_1:
            values[f] = None
        for f in database_fields_level_2:
            values[f] = None
        for f in database_fields_level_3:
            values[f] = None
        return pd.DataFrame([values])

    # Parse the businesses in responses
    businesses = []
    for b in response['businesses']:
        values = {}
        values['row_id'] = item_number
        values['sname_place_id'] = sg_id
        values['request_status'] = 'success'
        values['error_message'] = None
        for f in database_fields_level_1:
            # Get the level - 1 fields
            if (f == 'price'):
                try:
                    values[f] = len(b[f])
                except KeyError:
                    values[f] = -1
                except:
                    logger.error('Error in price handling. ', exc_info = True) 
            else:
                values[f] = b[f]
        for f in database_fields_level_2:
            # Get the level - 2 fields
            values[f] = b['location'][f]
        for f in database_fields_level_3:
            # Get the level - 3 fields
            values[f] = b['coordinates'][f]
        businesses.append(values)
    return pd.DataFrame(businesses)

################################################################################


############################# Main code ########################################

if __name__ == '__main__':
    
    # Read inputs from command line
    api_key = sys.argv[1]
    n_records = int(sys.argv[2])
    output_folder_path = sys.argv[3]
    log_file_path = sys.argv[4]
    postfix = sys.argv[5]

    # Inititate an instance of a Yelp_Match class
    searcher = Yelp_Match(api_key, match_type = 'phone')
    searcher.set_output_folder_path(output_folder_path)

    # Set the logging options
    logger = searcher.logger
    logger.setLevel(logging.INFO)
    # Write log to file (DEBUG)
    fh = logging.FileHandler(log_file_path)
    fh.setLevel(logging.INFO)
    # Write log to console (ERROR)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    # Create formatter and add it to the handlers
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    fh.setFormatter(formatter)
    ch.setFormatter(formatter)
    # Add the handlers to the logger
    logger.addHandler(fh)
    logger.addHandler(ch)

    logger.info('Creating database connections.')
    # Connect to the database via SQLalchemy
    engine = db.create_engine('postgresql://{user}:{user_pass}@{host}/{dataname1}')
    connection = engine.connect()
    # Connect via psycopg2
    conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')

    logger.info('Getting the requests list.')
    # Construct a pandas table with requests to be made
    requests_table = get_records.get_request_phones(n_records, engine, conn, postfix)

    logger.info('Making requests.')
    result = requests_table.apply(request_phone_save, axis = 1)
    result = pd.concat(result.values)
    logger.info('Requests done.')

    # logger.info('Writing error messages to database.')
    # failures = result.loc[result['request_status'] != 'success'].copy()
    # cur = conn.cursor()
    # cur.execute(create_status_update_table_statement)
    # rows = zip(failures.row_id, failures.request_status, failures.error_message)
    # cur.executemany(insert_status_update_table_statement, rows)
    # cur.execute(update_restaurants_requests_statement)
    # conn.commit()
    # cur.close()
    
    logger.info('Adding to the restaurants database.')
    success = result.loc[result['request_status'] == 'success'].copy()
    success.drop(['request_status', 'error_message'], axis = 1, inplace = True)
    success.reset_index(drop = True, inplace = True)
    success.to_sql('more_yelp_restaurants',
            con = engine,
            index = False,
            if_exists = 'append',
            dtype = {'categories' : db.types.JSON})
    # End database connection
    engine.dispose()

    logger.info('Writing status updates.')
    status_update = result.copy()
    status_update.drop_duplicates(subset = 'row_id', keep = 'first', inplace = True)
    cur = conn.cursor()
    cur.execute(create_status_update_table_statement)
    rows = zip(status_update.row_id, status_update.request_status, status_update.error_message)
    cur.executemany(insert_status_update_table_statement, rows)
    cur.execute(update_restaurants_requests_statement)
    conn.commit()
    conn.close()

    logger.info('Done.')

################################################################################
