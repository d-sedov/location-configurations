################################################################################ 
################################################################################
#
# FILE: crexi-rent-download.py
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Mar 26 2020
#
# DESC: This code downloads zip-code level commercial rent data from crexi
#       specifically for areas with no restaurants in SG data.
#
# EXEC:
#      
################################################################################
################################################################################


################################## Libraries ###################################

# Async code
import asyncio
import aiohttp

import json
import os
import time
import zipcodes

import pandas as pd
import sqlalchemy as db

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

output_folder_path = '/home/user/projects/urban/data/input/Rent/Crexi/'

url = 'https://api-lease.crexi.com/assets?'
params = {'types': 'Retail',
          'count': '60',
          'offset': '0',
          'sortDirection': 'Descending',
          'sortOrder': 'Rank'}

################################################################################


################################## Functions ###################################

# Function to get results for a single zip code
async def get_results(session, sem, zip_code):
    # Make 5 attempts
    attempt = 5
    async with sem:
        while attempt != 0:
            try:
                params_local = params
                params_local['zips'] = zip_code
                async with session.get(url, params = params_local) as response:
                    print(zip_code)
                    r = await response.text()
                    r = json.loads(r)
                    file_name = 'zip' + zip_code + '.txt'
                    file_path = os.path.join(output_folder_path, file_name)
                    with open(file_path, 'w') as the_file:
                        json.dump(r, fp = the_file)
                    return 1
            except aiohttp.client_exceptions.ClientConnectorError as exc:
                print('exception at ', zip_code, '!')
                attempt -= 1
    return -1


# Function to run the tasks in the async way
async def run(tasks):
    results = []
    # Bound on the number of simultaneous requests placed
    sem = asyncio.Semaphore(50)
    async with aiohttp.ClientSession() as session:
        async with sem:
            for zip_code in tasks:
                results.append(asyncio.ensure_future(get_results(session, sem, zip_code)))
            return await asyncio.gather(*results)

################################################################################


################################## Main code ###################################

if __name__ == '__main__':

    # Get zip codes
    print('Executing SQL query...')
    engine = db.create_engine('postgresql://{user}:{user_pass}@{host}/{dataname2}')
    connection = engine.connect()
    zip_codes_table = pd.read_sql(get_zips_statement, engine)
    engine.dispose()

    print('Constructing zip code list...')
    rest_zips = set(zip_codes_table['zip_code'].tolist())
    all_zip_codes = {z['zip_code'] for z in zipcodes.list_all()}
    zips_to_request = all_zip_codes - rest_zips
    zips_to_request = list(zips_to_request)
    print(f'List of {len(zips_to_request)} constructed.')

    time.sleep(5.5) 

    print('Running async requests...')
    start_time = time.time()
    loop = asyncio.get_event_loop()
    results = loop.run_until_complete(run(zips_to_request))
    print('--- %s seconds ---' % (time.time() - start_time))

################################################################################
