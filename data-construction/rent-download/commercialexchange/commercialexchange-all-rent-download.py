################################################################################ 
################################################################################
#
# FILE: commercialexchange-all-rent-download.py
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Mar 30 2020
#
# DESC: This code downloads [all] rent data from commercialexchange.com.
#
# EXEC: python3 commercialexchange-all-rent-download.py search
#       python3 commercialexchange-all-rent-download.py results
#      
################################################################################
################################################################################


############################### Libraries ######################################

# Async code
import asyncio
import aiohttp

import sys
import json
import os
import time
import random

import pandas as pd

import zipcodes

################################################################################

######################### Constants and settings ###############################

url_search = 'https://api.commercialexchange.com/api/cfra/ee/search/properties/_list?pageSize=200&pageNumber=1&sort=QUALITY%3Adesc&byProperty=true'
url_property = 'https://api.commercialexchange.com/api/cfra/ee/property/{}'

user_agent = 'Moilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.132 Safari/537.36'

output_folder_path_search = '/home/user/projects/urban/data/input/Rent/commercialexchange/all/search'
output_folder_path_results = '/home/user/projects/urban/data/input/Rent/commercialexchange/all/results'

code_folder_path = '/home/user/projects/urban/code/data-construction/rent-download/commercialexchange'

form_data = {'listedStatus':['AVAILABLE_LEASE']
            }

headers_search = {'x-api-key': '***REMOVED***',
                  'user-agent': user_agent,
                  'content-type': 'application/json',
                  'accept': 'application/json'
                 }

headers_property = {'x-api-key': '***REMOVED***',
                    'user-agent': user_agent,
                    'accept': 'application/json'
                   }

################################################################################


################################## Functions ###################################

# Function to search for results for a single zip code
async def get_search(session, sem, zip_code):
    # Make 2 attempts
    attempt = 2
    async with sem:
        while attempt != 0:
            try:
                form_data_local = form_data
                form_data_local['locations'] = [{'postalCode': zip_code}]
                async with session.post(url_search, data = json.dumps(form_data_local), headers = headers_search) as response:
                    print(zip_code)
                    r = await response.text()
                    r = json.loads(r)
                    property_ids = [x['data']['id'] for x in r['results']]
                    file_name = 'zip' + zip_code + '.txt'
                    file_path = os.path.join(output_folder_path_search, file_name)
                    with open(file_path, 'w') as the_file:
                        json.dump(r, fp = the_file)
                    return_row = dict(zip_code = zip_code,
                            total_results = r['total'],
                            page_number = r['pageNumber'],
                            page_size = r['pageSize'],
                            property_ids = property_ids
                            )
                    return return_row
            except (KeyboardInterrupt, SystemExit):
                raise
            except aiohttp.client_exceptions.ClientConnectorError as exc:
                print('exception at ', zip_code, '!')
                attempt -= 1
                continue
            except json.decoder.JSONDecodeError as e:
                print(f'json.decoder.JSONDecodeError: {e} at {zip_code}!')
                with open(os.path.join(code_folder_path, 'commercialexchange-all-search.log'), 'a+') as log_file:
                    print(f'json.decoder.JSONDecodeError: {e} at {zip_code}!', file = log_file)
                attempt -= 1
                break
            except Exception as e:
                print(f'{type(e).__name__}: {e} at {zip_code}!')
                with open(os.path.join(code_folder_path, 'commercialexchange-all-search.log'), 'a+') as log_file:
                    print(f'{type(e).__name__}: {e} at {zip_code}!', file = log_file)
                attempt -= 1
                continue
    return_row = dict(zip_code = zip_code,
            total_results = -1,
            page_number = -1,
            page_size = -1,
            property_ids = []
            )
    return return_row

# Function to get results for a single property_id
async def get_results(session, sem, property_id):
    # Make 5 attempts
    attempt = 5
    async with sem:
        while attempt != 0:
            try:
                url_property_local = url_property.format(property_id)
                async with session.get(url_property_local, headers = headers_property, timeout = 1) as response:
                    print(property_id)
                    r = await response.text()
                    r = json.loads(r)
                    file_name = 'id-' + property_id + '.txt'
                    file_path = os.path.join(output_folder_path_results, file_name)
                    with open(file_path, 'w') as the_file:
                        json.dump(r, fp = the_file)
                    return 1
            except (KeyboardInterrupt, SystemExit):
                raise
            except aiohttp.client_exceptions.ClientConnectorError as exc:
                print('exception at ', property_id, '!')
                attempt -= 1
                continue
            except json.decoder.JSONDecodeError as e:
                print(f'json.decoder.JSONDecodeError: {e} at {property_id}!')
                with open(os.path.join(code_folder_path, 'commercialexchange-all-results.log'), 'a+') as log_file:
                    print(f'json.decoder.JSONDecodeError: {e} at {property_id}!', file = log_file)
                attempt -= 1
                break
            except Exception as e:
                print(f'{type(e).__name__}: {e} at {property_id}!')
                with open(os.path.join(code_folder_path, 'commercialexchange-all-results.log'), 'a+') as log_file:
                    print(f'{type(e).__name__}: {e} at {property_id}!', file = log_file)
                attempt -= 1
                continue
    return -1


# Function to run the tasks in the async way
async def run(func, tasks):
    results = []
    # Bound on the number of simultaneous requests placed
    sem = asyncio.Semaphore(10)
    async with aiohttp.ClientSession() as session:
        async with sem:
            for task in tasks:
                results.append(asyncio.ensure_future(func(session, sem, task)))
            return await asyncio.gather(*results)

################################################################################


################################## Main code ###################################

if __name__ == '__main__':

    part = sys.argv[1]

    if part == 'search':
        print('Working on getting property ids by zip code.')
        time.sleep(3)

        # Construct zip codes
        print('Getting zip codes.')
        all_zip_codes = sorted(list({z['zip_code'] for z in zipcodes.list_all()}))
        print(f'{len(all_zip_codes)} zip codes in total.')

        time.sleep(5)

        print('Running async requests...')
        start_time = time.time()
        loop = asyncio.get_event_loop()
        search_results = loop.run_until_complete(run(get_search, all_zip_codes))
        print('--- %s seconds ---' % (time.time() - start_time))

        # Construct the results summary table
        print('Exporting summary table.')
        search_results = pd.DataFrame(search_results)
        search_results.to_csv(os.path.join(output_folder_path_search, 'commercialexchange-all-search.csv'), index = False)
        print('Done.')
    
    elif part == 'results':

        print('Working on getting property data.')
        time.sleep(3)

        print('Getting property ids.')
        search_results = pd.read_csv(os.path.join(output_folder_path_search, 'commercialexchange-all-search.csv'))
        property_ids = search_results['property_ids'].tolist()
        property_ids = [eval(x) for x in property_ids]
        property_ids = [item for sublist in property_ids for item in sublist]
        print(f'{len(property_ids)} property ids in total.')

        time.sleep(5)

        print('Running async requests...')
        start_time = time.time()
        loop = asyncio.get_event_loop()
        search_results = loop.run_until_complete(run(get_results, property_ids))
        print('--- %s seconds ---' % (time.time() - start_time))

    else:
        print('Doing nothing.')

################################################################################
