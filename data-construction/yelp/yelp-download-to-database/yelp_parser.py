################################################################################
################################################################################
#
# FILE: yelp_parser.py
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
id TEXT PRIMARY KEY,
name TEXT,
phone TEXT,
price INTEGER,
review_count INTEGER,
rating REAL,
categories TEXT,
longitude NUMERIC,
latitude NUMERIC,
country TEXT,
state TEXT,
city TEXT,
zip_code TEXT,
address1 TEXT
);
"""

log_file_path = '/home/user/projects/urban/code/yelp-download-to-database/logs/yelp-parse-json.log'

all_cbgs_path = os.path.join('/home/user/projects/urban/data/processed/',
        'intermediate_use/yelp_request_locations/all/cbgs_rest_coord.csv')

json_common_path = '/home/user/projects/urban/data/input/Yelp/from_api/'

json_folders = [json_common_path + f for f in ['do_part', 'ds_part', 'ta_part']]

additional_folder = json_common_path + 'add_part'

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

def to_str_if_list(x):
    if isinstance(x,list):
        return str(x)
    else:
        return x


class Yelp_Json:
    """ This class enables importing and parsing the Yelp json files. """
    
    def __init__(self, cbg):
        """ Initiate an instance of Yelp_Json class by supplying the cbg
        number. """
        self.cbg = cbg
        self.logger = logging.getLogger(__name__)
        self.logger.debug('Logging started in class Yelp_Json instance.')

    def add_json_file_path(self):
        """ Look for the file corresponding to the cbg number. """
        found = False
        for folder in json_folders:
            try_path = os.path.join(folder, 'part{}.json'.format(self.cbg))
            self.logger.debug(f'Considering path {try_path}.')
            if os.path.exists(try_path):
                found = True
                self.json_file_path = try_path
                break
        # Return True if the file is found.
        if found:
            return True
        else:
            self.logger.warning('cbg {} does not have a corresponding json file.'.format(self.cbg))
            return False

    def add_json_file_path_additional(self):
        found = False
        try_path = os.path.join(additional_folder, 'part{}.json'.format(self.cbg))
        self.logger.debug(f'Considering path {try_path}.')
        if os.path.exists(try_path):
            found = True
            self.json_file_path = try_path
        if found:
            return True
        else:
            self.logger.warning('cbg {} does not have a corresponding json file.'.format(self.cbg))
            return False

    def import_json(self):
        """ Import the json file. """
        with open(self.json_file_path, 'r') as json_file:
            self.json = json.load(json_file)
            self.logger.debug('Json loaded for cbg {}.'.format(self.cbg))
            self.non_empty = 'businesses' in self.json
        return None

    def insert_to_database(self, cur):
        """ Iterate through the businesses in JSON, insert to database. """
        if (not self.non_empty):
            self.logger.warning('No businesses in cbg {}.'.format(self.cbg))
            return None
        # If some businesses are present
        values = {}
        for b in self.json['businesses']:
            # Iterate through businesses 
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
            # Format the insert statement
            values = {key: value for (key, value) in values.items() if value}
            (all_fields, all_values) = zip(*values.items())
            all_values = ', '.join([f'%({x})s' for x in all_fields])
            all_fields = ', '.join(all_fields)
            values = {key: to_str_if_list(value) for (key,value) in values.items()}
            local_insert_statement = insert_statement.format(
                    all_fields = all_fields,
                    all_values = all_values
                    )
            self.logger.debug(local_insert_statement)
            # Execute the insert statement 
            cur.execute(local_insert_statement, values)
        return None


def initiate_logging():
    # Initiate logging 
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.INFO)
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
    logger.debug('Database connection created.')

    # Create a database 
    # cur.execute(table_create_statement)
    
    # Import all of the cbgs, select one
    cbgs = pd.read_csv(all_cbgs_path, usecols = ['cbg'], dtype = {'cbg':
        pd.Int64Dtype()})
    total = cbgs.shape[0]
    cbg = cbgs['cbg'][0]
    
    logger.info(f'Considering cbg {cbg}.')
    yelp_cbg = Yelp_Json(cbg)
    json_exists = yelp_cbg.add_json_file_path()
    if json_exists:
        yelp_cbg.import_json()
        yelp_cbg.insert_to_database(cur)
        logger.info(f'The cbg {cbg} json imported into the database.')

    conn.commit()
    conn.close()

################################################################################
