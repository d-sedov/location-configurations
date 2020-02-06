################################################################################
################################################################################
#
# FILE: additional_parsing.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Nov 16 2019
#
# DESC: This file runs the additional parsing of data downloaded from Yelp API.
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
    add_folder_path = '/home/user/projects/urban/data/processed/intermediate_use/yelp_request_locations/add_part/'
    additional_cbg_path = add_folder_path + 'yelp-redo.csv'
    # Initate logging 
    logger = initiate_logging()

    # Open the database connection
    conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')
    cur = conn.cursor()
    logger.info('Database connection created.')

    # Import all of the cbgs
    cbgs = pd.read_csv(additional_cbg_path, usecols = ['cbg'], dtype = {'cbg': pd.Int64Dtype()})

    total = cbgs.shape[0]

    # Iterate through the cbgs, find the respective json, write to database
    logger.info('Looping through additional cbgs. {total} cbgs in total.')
    for i, cbg in enumerate(cbgs['cbg']):
        logger.info(f'Considering cbg {cbg} ({i} / {total}).')
        yelp_cbg = Yelp_Json(cbg)
        json_exists = yelp_cbg.add_json_file_path_additional()
        if json_exists:
            logger.info(f'Importing from {yelp_cbg.json_file_path}.')
            yelp_cbg.import_json()
            yelp_cbg.insert_to_database(cur)
            logger.info(f'The cbg {cbg} json imported into the database.')
    logger.info(f'Completed looping through additional cbgs.')

    logger.info('Commiting additions to the database.')
    conn.commit()
    conn.close()

################################################################################

