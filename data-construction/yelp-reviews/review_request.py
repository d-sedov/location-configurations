################################################################################
################################################################################
#
# FILE: review_requests.py
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Apr 9 2020
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


################################################################################


########################### SQL Statements #####################################

create_status_update_table_statement = """
CREATE TEMPORARY TABLE status_update (y_id TEXT, review_request_status VARCHAR, review_error_message VARCHAR) 
ON COMMIT DROP
"""

insert_status_update_table_statement = """
INSERT INTO status_update (y_id, review_request_status, review_error_message) 
VALUES(%s, %s, %s)
"""

update_review_requests_statement = """
UPDATE review_requests
SET 
    review_request_status = status_update.review_request_status,
    review_error_message = status_update.review_error_message
FROM status_update
WHERE 
    status_update.y_id = review_requests.y_id;
"""

################################################################################


######################## Functions and classes #################################

class Yelp_Lookup:
    """ This class facilitates requests to Yelp by phone number or address. """

    def __init__(self, api_key, match_type = 'y_id'):

        # Make sure the match type is valid
        assert match_type in ('y_id')

        # Initiate instance of a class with API key, logger and urls
        self.api_key = api_key
        self.headers = {'Authorization': 'Bearer %s' % self.api_key}
        if (match_type == 'y_id'):
            self.search_url = 'https://api.yelp.com/v3/businesses/{}/reviews'
        self.logger = logging.getLogger(__name__)

    def search_by_id(self, yelp_id):
        # Send the review by yelp id
        formatted_url = self.search_url.format(yelp_id)
        response = requests.get(url = formatted_url,
                headers = self.headers
                )
        self.json = response.json()
        return self.json

def request_id(searcher, row):
    """ This function makes a request and parses response based on y_id. """

    y_id = row['y_id']
    response = searcher.search_by_id(y_id)

    if (not ('reviews' in response)):
        # If no businesses in response mark result as no success
        values = {}
        values['y_id'] = y_id
        values['review_request_status'] = 'zero'
        values['review_error_message'] = None
        values['rating'] = None
        values['time_created'] = None
        if 'error' in response:
            # If an error in response, save error message
            values['review_request_status'] = 'error'
            values['review_error_message'] = response['error']['code']
        return pd.DataFrame([values])

    if (len(response['reviews']) == 0):
        values = {}
        values['y_id'] = y_id
        values['review_request_status'] = 'zero'
        values['review_error_message'] = None
        values['rating'] = None
        values['time_created'] = None
        return pd.DataFrame([values])

    # Parse the reviews in responses
    reviews = []
    for review in response['reviews']:
        values = {}
        values['y_id'] = y_id
        values['review_request_status'] = 'success'
        values['review_error_message'] = None
        values['rating'] = review['rating']
        values['time_created'] = review['time_created']
        reviews.append(values)

    return pd.DataFrame(reviews)

################################################################################


############################# Main code ########################################

if __name__ == '__main__':
    
    # Read inputs from command line
    api_key = sys.argv[1]
    n_records = int(sys.argv[2])
    log_file_path = sys.argv[3]
    postfix = sys.argv[4]

    # Inititate an instance of a Yelp_Match class
    searcher = Yelp_Lookup(api_key, match_type = 'y_id')

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
    requests_table = get_records.get_request_reviews(n_records, engine, conn, postfix)

    logger.info('Making requests.')
    result = requests_table.apply(lambda row: request_id(searcher, row), axis = 1)
    result = pd.concat(result.values)
    logger.info('Requests done.')

    logger.info('Adding to the reviews database.')
    success = result.loc[result['review_request_status'] == 'success'].copy()
    success['time_created'] = pd.to_datetime(success['time_created'])
    success.drop(['review_request_status', 'review_error_message'], axis = 1, inplace = True)
    success.reset_index(drop = True, inplace = True)
    success.to_sql('reviews_yelp',
            con = engine,
            index = False,
            if_exists = 'append')
    # End database connection
    engine.dispose()

    logger.info('Writing status updates.')
    status_update = result.copy()
    status_update.drop_duplicates(subset = 'y_id', keep = 'first', inplace = True)
    cur = conn.cursor()
    cur.execute(create_status_update_table_statement)
    rows = zip(status_update.y_id, status_update.review_request_status, status_update.review_error_message)
    cur.executemany(insert_status_update_table_statement, rows)
    cur.execute(update_review_requests_statement)
    conn.commit()
    conn.close()

    logger.info('Done.')

################################################################################
