################################################################################ 
################################################################################
#
# FILE: crexi-rent-download-all.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Apr 1 2020
#
# DESC: This code downloads zip-code level commercial rent data from crexi for
#       all types of spaces (not only retail).
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

output_folder_path = '/home/user/projects/urban/data/input/Rent/Crexi/all'
code_folder_path = '/home/user/projects/urban/code/data-construction/rent-download/crexi'

url = 'https://api-lease.crexi.com/assets?'
params = {'count': '100',
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
            except (KeyboardInterrupt, SystemExit):
                raise
            except aiohttp.client_exceptions.ClientConnectorError as exc:
                print('exception at ', zip_code, '!')
                attempt -= 1
                continue
            except json.decoder.JSONDecodeError as e:
                print(f'json.decoder.JSONDecodeError: {e} at {zip_code}!')
                with open(os.path.join(code_folder_path, 'crexi-download-all.log'), 'a+') as log_file:
                    print(f'json.decoder.JSONDecodeError: {e} at {zip_code}!', file = log_file)
                attempt -= 1
                break
            except Exception as e:
                print(f'{type(e).__name__}: {e} at {zip_code}!')
                with open(os.path.join(code_folder_path, 'crexi-download-all.log'), 'a+') as log_file:
                    print(f'{type(e).__name__}: {e} at {zip_code}!', file = log_file)
                attempt -= 1
                continue
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
    print('Constructing zip code list...')
    all_zip_codes = sorted(list({z['zip_code'] for z in zipcodes.list_all()}))
    print(f'List of {len(all_zip_codes)} constructed.')
    print(f'Writing to {output_folder_path}.')

    time.sleep(5.5) 

    print('Running async requests...')
    start_time = time.time()
    loop = asyncio.get_event_loop()
    results = loop.run_until_complete(run(all_zip_codes))
    print('--- %s seconds ---' % (time.time() - start_time))

################################################################################
