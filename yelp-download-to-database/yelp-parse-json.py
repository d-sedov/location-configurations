################################################################################
################################################################################
#
# FILE: yelp-parse-json.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Nov 13 2019
#
# DESC: This file contains the code that parses the json files downloaded from
# Yelp. 
#
# EXEC:
#      
################################################################################
################################################################################


############################# Libraries ########################################

import logging # Facilitates logging
import json # Handling json 
import psycopg2 # Handles the PostgreSQL database connections
import os # OS folder / file interaction
import pandas as pd # Handling the csv files

################################################################################


############################# Constants ########################################

table_create_statement = """
CREATE TABLE yelp_restaurants(
yelp_id TEXT PRIMARY KEY,
name TEXT,
phone TEXT,
price INTEGER,
review_count INTEGER,
rating REAL,
categories JSONB
longitude NUMERIC,
latitude NUMERIC,
country TEXT,
state TEXT,
city TEXT,
zip_code INTEGER,
address1 TEXT
);
"""

log_file_path = '/home/user/projects/urban/code/yelp-download-to-database/logs/yelp-parse-json.log'

all_cbgs_path = os.path.join('/home/user/projects/urban/data/processed/',
        'intermediate_use/yelp_request_locations/all/cbgs_rest_coord.csv')

json_common_path = '/home/user/projects/urban/data/input/Yelp/from_api/'

json_folders = [json_common_path + f for f in ['do_part', 'ds_part', 'ta_part']]

database_fields_level_1 = [
        'id',
        'name',
        'phone',
        'price',
        'review_count',
        'rating',
        'categories'
        ]

database_fields_level_2 = [
        'longitude',
        'latitude',
        'country',
        'state',
        'city',
        'zip_code',
        'address1'
        ]

insert_statement = """
INSERT INTO
    yelp_restaurants({all_fields})
VALUES
    ({all_values})
ON CONFLICT (id)
DO NOTHING
;
"""

################################################################################


######################## Functions and classes #################################

class Yelp_Json:
    """ This class enables importing and parsing the Yelp json files. """
    
    def __init__(self, cbg):
        """ Initiate an instance of Yelp_Json class by supplying the cbg
        number. """
        self.cbg = cbg
        self.logger = logging.getLogger(__name__)
        self.logger.debug('Logging started in class Yelp_Json instance')

    def add_json_file_path(self):
        """ Look for the file corresponding to the cbg number. """
        found = False
        for folder in json_folders:
            try_path = os.path.join(folder, f'part{self.cbg}.json')
            if os.path.exists(try_path):
                found = True
                self.json_file_path = try_path
                break
        # Return True if the file is found.
        if found:
            return True
        else:
            self.logger.warning(f'cbg {self.cbg} does not have a corresponding json file.')
            return False

    def import_json(self):
        """ Import the json file. """
        with open(self.json_file_path, 'r') as json_file:
            self.json = json.load(json_file)
            self.logger.info(f'Json loaded for cbg {self.cbg}.')
        return None

    def insert_to_database(self, cur):
        """ Iterate through the businesses in JSON, insert to database. """
        values = {}
        for b in self.json['businesses']:
            # Iterate through businesses 
            for f in database_fields_level_1:
                # Get the level - 1 fields
                values[f] = b[f]
            for f in database_fields_level_2:
                # Get the level - 2 fields
                values[f] = b['location'][f]
            # Format the insert statement
            (all_fields, all_values) = zip(*values.items())
            all_fields = ','.join([f"'{x}'" for x in all_fields])
            all_values = ','.join(all_values)
            local_insert_statement = insert_statement.format(
                    all_fields = all_fields,
                    all_values = all_values
                    )
            # Execute the insert statement 
            cur.execute(local_insert_statement)
        return None


def initiate_logging():
    # Initiate logging 
    logger = logging.getLogger(__name__)
    # Create the console and file handlers
    ch = logging.StreamHandler()
    fh = logging.FileHandler(log_file_path)
    ch.setLevel(logging.INFO)
    fh.setLevel(logging.WARNING)
    # Set the formatting options
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    fh.setFormatter(formatter)
    # Add the handlers with the set formatters to the logger
    logger.addHandler(ch)
    logger.addHandler(fh)
    return logger

################################################################################


############################## Main code #######################################

if __name__ == '__main__':

    # Initate logging 
    logger = initiate_logging()

    # Open the database connection
    conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')
    cur = conn.cursor()
    logger.info('Database connection created.')
    
    # Import all of the cbgs
    cbgs = pd.read_csv(all_cbgs_path, usecols = ['cbg'])
    total = cbgs.shape[0]

    # Iterate through the cbgs, find the respective json, write to database
    logger.info('Looping through cbgs. {total} cbgs in total.')
    for i, cbg in enumertate(cbgs['cbg']):
        logger.info(f'Considering cbg {cbg} ({i} / {total}.')
        yelp_cbg = Yelp_Json(cbg)
        json_exists = yelp_cbg.json_file_path()
        if json_exists:
            yelp_cbg.import_json()
            yelp.insert_to_database(cur)
            logger.info(f'The cbg {cbg} json imported into the database.')
    logger.info(f'Completed looping through cbgs.')

    logger.info('Commiting additions to the database.')
    conn.commit()
    conn.close()

################################################################################
