################################################################################
################################################################################
#
# FILE: get_addresses.py 
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Jan 16 2020
#
# DESC: This code 
#           1) gets a parameter n_records - how many records to get from the 
#           table.
#           2) queries the database - taking n_records with address_request_status
#           = 'needed'
#           3) marks the same records as address_request_status = 'making'
#           4) returns a table with row_ids, locations of the queried records.
#
# EXEC: 
#      
################################################################################
################################################################################


############################# Libraries ########################################

import logging # Facilitates logging 
import psycopg2 # PostgreSQL interaction
import sqlalchemy # SQL interaction
import pandas as pd # Data tables

################################################################################


############################# Constants ########################################

get_addresses_statement = """
SELECT 
    * 
FROM 
    restaurants_requests
WHERE 
    address_request_status = 'needed' 
LIMIT {n_records}
"""

create_temp_update_table_statement = """
CREATE TEMPORARY TABLE updated_rows (row_id INTEGER, address_request_status VARCHAR) 
ON COMMIT DROP
"""

insert_temp_update_table_statement = """
INSERT INTO updated_rows (row_id, address_request_status) 
VALUES(%s, %s)
"""

update_matching_table_statement = """
UPDATE restaurants_requests
SET 
    address_request_status = updated_rows.address_request_status
FROM updated_rows
WHERE 
    updated_rows.row_id = restaurants_requests.row_id;
"""

################################################################################


######################## Functions and classes #################################

def get_request_addresses(n_records, engine, conn, postfix):
    """ This function returns a dataframe with records for which the requests
    need to be made. """

    # Create a cursor to execute queries
    cur = conn.cursor()

    # Get table with restaurants for which requests are needed
    l_get_addresses_statement = get_addresses_statement.format(n_records = n_records)
    requests_table = pd.read_sql(l_get_addresses_statement, engine)
    # Mark all rows as requests made
    status = 'making:' + postfix
    requests_table['address_request_status'] = status

    # Update the original table
    cur.execute(create_temp_update_table_statement)
    rows = zip(requests_table.row_id, requests_table.address_request_status)
    cur.executemany(insert_temp_update_table_statement, rows)
    cur.execute(update_matching_table_statement)
    conn.commit()
    cur.close()

    return requests_table

################################################################################


############################# Main code ########################################

if __name__ == '__main__':

    pass
    # Connect to the database via SQLalchemy
    # engine = db.create_engine('postgresql://{user}:{user_pass}@{host}/{dataname1}')
    # connection = engine.connect()

    # Connect via psycopg2
    # conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')
    # cur = conn.cursor()

################################################################################
