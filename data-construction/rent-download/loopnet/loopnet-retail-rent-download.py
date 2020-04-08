################################################################################ 
################################################################################
#
# FILE: loopnet-retail-rent-download.py
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Apr 7 2020
#
# DESC: This code downloads rent data from loopnet.com.
#
# EXEC:
#      
################################################################################
################################################################################


############################### Libraries ######################################

# Async code
import asyncio
import aiohttp

import os
import time
import random

import pandas as pd

import zipcodes

from bs4 import BeautifulSoup

################################################################################

######################### Constants and settings ###############################

url = 'https://www.loopnet.com/zip/{zip_code}_retail-space-for-lease/{page_number}/'

user_agent = 'Moilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.132 Safari/537.36'

output_folder_path = '/home/quser/project_dir/urban/data/input/Rent/loopnet/retail'
code_folder_path = '/home/quser/project_dir/urban/code/data-construction/rent-download/loopnet'

headers = {'User-Agent': user_agent}

################################################################################


################################## Functions ###################################

# Function to get a single zip code html page
async def get_page(session, sem, zip_code, page_number):
    # Make 2 attempts
    attempt = 2
    async with sem:
        while attempt != 0:
            try:
                link_local = url.format(zip_code = zip_code, page_number = page_number)
                async with session.get(link_local, headers = headers) as response:
                    print(zip_code, page_number)
                    r = await response.text()
                    soup = BeautifulSoup(r, 'html.parser')
                    n_results = int(soup.find('div', {'class': 'seoListingsTitle'}).find('b').text.split(' ')[0])
                    pages = [(zip_code, p) for p in range(2, -( -n_results // 25) + 1)]
                    file_name = 'zip-' + zip_code + '-page-' + str(page_number) + '.html'
                    file_path = os.path.join(output_folder_path, file_name)
                    with open(file_path, 'w') as the_file:
                        the_file.write(r)
                    return pages
            except (KeyboardInterrupt, SystemExit):
                raise
            except TypeError:
                raise
            except aiohttp.client_exceptions.ClientConnectorError as exc:
                print(f'ClientConnectorError at {zip_code}, {page_number}!')
                attempt -= 1
                continue
            except aiohttp.ClientResponseError as exc:
                print(f'ClientResponseError at {zip_code}, {page_number}!')
                with open(os.path.join(code_folder_path, 'loopnet-search.log'), 'a+') as log_file:
                    print(f'ClientResponseError at {zip_code}, {page_number}!', file = log_file)
                attempt -= 1
                continue
            except (AttributeError, ValueError) as exc:
                with open(os.path.join(code_folder_path, 'loopnet-search.log'), 'a+') as log_file:
                    print(f'No results at {zip_code}, {page_number}!', file = log_file) 
                break
            except Exception as e:
                print(f'{type(e).__name__}: {e} at {zip_code}, {page_number}!')
                with open(os.path.join(code_folder_path, 'loopnet-search.log'), 'a+') as log_file:
                    print(f'{type(e).__name__}: {e} at {zip_code}, {page_number}!', file = log_file)
                attempt -= 1
                continue
    return []


# Function to run the tasks in the async way
async def run(func, tasks):
    results = []
    # Bound on the number of simultaneous requests placed
    sem = asyncio.Semaphore(10)
    async with aiohttp.ClientSession(raise_for_status = True) as session:
        async with sem:
            for task in tasks:
                results.append(asyncio.ensure_future(func(session, sem, *task)))
            return await asyncio.gather(*results)

################################################################################

################################## Main code ###################################

if __name__ == '__main__':

    print('Working on first pages for each zip code.')
    time.sleep(2)

    # Construct zip codes
    print('Getting zip codes.')
    all_zip_codes = sorted(list({z['zip_code'] for z in zipcodes.list_all()}))
    all_zip_codes = [(z, 1) for z in all_zip_codes]
    print(f'{len(all_zip_codes)} tasks in total.')

    time.sleep(3)

    print('Running async requests...')
    start_time = time.time()
    loop = asyncio.get_event_loop()
    results = loop.run_until_complete(run(get_page, all_zip_codes))
    print('--- %s seconds ---' % (time.time() - start_time))

    print('Running second round.')
    results = [item for sublist in results for item in sublist]
    print(f'{len(results)} tasks in total.')
    print('Done.')

    print('Running async requests...')
    start_time = time.time()
    loop = asyncio.get_event_loop()
    end_results = loop.run_until_complete(run(get_page, results))
    print('--- %s seconds ---' % (time.time() - start_time))

################################################################################
