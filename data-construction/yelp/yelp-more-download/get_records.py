################################################################################
################################################################################
#
# FILE: get_records.py 
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Dec 12 2019
#
# DESC: This code 
#           1) gets a parameter n_records - how many records to get from the 
#           table.
#           2) queries the database - taking n_records with phone_request_status
#           = 'needed'
#           3) marks the same records as phone_request_status = 'making'
#           4) returns a table with row_ids, phones of the queried records.
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

get_phones_statement = """
SELECT 
    * 
FROM 
    restaurants_requests
WHERE 
    phone_request_status = 'needed' 
LIMIT {n_records}
"""

create_temp_update_table_statement = """
CREATE TEMPORARY TABLE updated_rows (row_id INTEGER, phone_request_status VARCHAR) 
ON COMMIT DROP
"""

insert_temp_update_table_statement = """
INSERT INTO updated_rows (row_id, phone_request_status) 
VALUES(%s, %s)
"""

update_matching_table_statement = """
UPDATE restaurants_requests
SET 
    phone_request_status = updated_rows.phone_request_status
FROM updated_rows
WHERE 
    updated_rows.row_id = restaurants_requests.row_id;
"""

################################################################################


######################## Functions and classes #################################

def get_request_phones(n_records, engine, conn, postfix):
    """ This function returns a dataframe with records for which the requests
    need to be made. """

    # Create a cursor to execute queries
    cur = conn.cursor()

    # Get table with restaurants for which requests are needed
    l_get_phones_statement = get_phones_statement.format(n_records = n_records)
    requests_table = pd.read_sql(l_get_phones_statement, engine)
    # Mark all rows as requests made
    status = 'making:' + postfix
    requests_table['phone_request_status'] = status

    # Update the original table
    cur.execute(create_temp_update_table_statement)
    rows = zip(requests_table.row_id, requests_table.phone_request_status)
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
