################################################################################ 
################################################################################
#
# FILE: handle_zip_codes.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Apr 8 2020
#
# DESC: This file creates a csv with zip codes with restaurants.
#
# EXEC:
#      
################################################################################
################################################################################

################################## Libraries ###################################

import pandas as pd
import sqlalchemy as db

import os

################################################################################


################################## Constants ###################################

get_zips_statement = """
SELECT 
    zip_code,
    COUNT(*) AS count 
FROM
    restaurants 
GROUP BY
    zip_code;
"""

output_folder_path = '/home/user/projects/urban/data/input/Rent/loopnet'
to_do_folder_path = '/home/user/projects/urban/data/input/Rent/loopnet/zips_to_do'
update_folder_path = '/home/user/projects/urban/data/input/Rent/loopnet/zips_update'

################################################################################

################################## Functions ###################################

def create_zip_codes_table():

    # Get zip codes
    print('Executing SQL query...')
    engine = db.create_engine('postgresql://{user}:{user_pass}@{host}/{dataname2}')
    connection = engine.connect()
    zip_codes_table = pd.read_sql(get_zips_statement, engine)
    engine.dispose()
    print(f'{zip_codes_table.shape[0]} zip codes in total.')

    zip_codes_table['downloaded'] = 0

    zip_codes_table.sort_values(by = ['count'], ascending = False, inplace = True)

    print('Saving csv to disk.')
    zip_codes_table.to_csv(os.path.join(output_folder_path, 'zip_codes.csv'),
            index = False)
    print('Done.')


def save_zip_part_for_download(n, name):

    zip_codes_table = pd.read_csv(os.path.join(output_folder_path, 'zip_codes.csv'))
    zip_codes_table = zip_codes_table[zip_codes_table['downloaded'] == 0]
    zip_codes_table.sort_values(by = ['count', 'zip_code'], ascending = [False, True], inplace = True)
    to_do = zip_codes_table.head(n)

    to_do.to_csv(os.path.join(to_do_folder_path, name), index = False)
    print('Done.')

    
def update_zip_codes_table(name):

    zip_codes_table = pd.read_csv(os.path.join(output_folder_path, 'zip_codes.csv'))
    zip_codes_table.set_index('zip_code', inplace = True)
    update_table = pd.read_csv(os.path.join(update_folder_path, name))
    update_table.set_index('zip_code', inplace = True)

    zip_codes_table.update(update_table)
    zip_codes_table.reset_index(inplace = True)

    zip_codes_table.to_csv(os.path.join(output_folder_path, 'zip_codes.csv'),
            index = False)


################################################################################


################################## Main code ###################################

if __name__ == '__main__':

    pass

################################################################################
