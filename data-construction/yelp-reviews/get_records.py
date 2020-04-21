################################################################################
################################################################################
#
# FILE: get_review_records.py 
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Apr 9 2020
#
# DESC: This code gets records from review_requests in order to make requests.
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

get_requests_statement = """
SELECT 
    * 
FROM 
    review_requests
WHERE 
    review_request_status = 'needed' 
ORDER BY
    row_id
LIMIT {n_records}
"""

create_temp_update_table_statement = """
CREATE TEMPORARY TABLE updated_rows (y_id TEXT, review_request_status VARCHAR) 
ON COMMIT DROP
"""

insert_temp_update_table_statement = """
INSERT INTO updated_rows (y_id, review_request_status) 
VALUES(%s, %s)
"""

update_matching_table_statement = """
UPDATE review_requests
SET 
    review_request_status = updated_rows.review_request_status
FROM updated_rows
WHERE 
    updated_rows.y_id = review_requests.y_id;
"""

################################################################################


######################## Functions and classes #################################

def get_request_reviews(n_records, engine, conn, postfix):
    """ This function returns a dataframe with records for which the requests
    need to be made. """

    # Create a cursor to execute queries
    cur = conn.cursor()

    # Get table with restaurants for which requests are needed
    l_get_requests_statement = get_requests_statement.format(n_records = n_records)
    requests_table = pd.read_sql(l_get_requests_statement, engine)
    # Mark all rows as requests making
    status = 'making:' + postfix
    requests_table['review_request_status'] = status

    # Update the original table
    cur.execute(create_temp_update_table_statement)
    rows = zip(requests_table.y_id, requests_table.review_request_status)
    cur.executemany(insert_temp_update_table_statement, rows)
    cur.execute(update_matching_table_statement)
    conn.commit()
    cur.close()

    return requests_table

################################################################################


############################# Main code ########################################

if __name__ == '__main__':

    pass

################################################################################
