################################################################################
################################################################################
#
# FILE: full_parsing.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Nov 15 2019
#
# DESC: This file runs the full parsing of data downloaded from Yelp API.
#
# EXEC:
#      
################################################################################
################################################################################


############################# Libraries ########################################

from yelp_parser import *

################################################################################


############################# Main code ########################################

if __name__ == '__main__':
    # Initate logging 
    logger = initiate_logging()

    # Open the database connection
    conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')
    cur = conn.cursor()
    logger.info('Database connection created.')

    # Create a database 
    cur.execute(table_create_statement)
    
    # Import all of the cbgs
    cbgs = pd.read_csv(all_cbgs_path, usecols = ['cbg'], dtype = {'cbg':
        pd.Int64Dtype()})

    total = cbgs.shape[0]

    # Iterate through the cbgs, find the respective json, write to database
    logger.info('Looping through cbgs. {total} cbgs in total.')
    for i, cbg in enumerate(cbgs['cbg']):
        logger.info(f'Considering cbg {cbg} ({i} / {total}).')
        yelp_cbg = Yelp_Json(cbg)
        json_exists = yelp_cbg.add_json_file_path()
        if json_exists:
            yelp_cbg.import_json()
            yelp_cbg.insert_to_database(cur)
            logger.info(f'The cbg {cbg} json imported into the database.')
    logger.info(f'Completed looping through cbgs.')

    logger.info('Commiting additions to the database.')
    conn.commit()
    conn.close()

################################################################################
