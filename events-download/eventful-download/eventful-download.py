################################################################################
################################################################################
#
# FILE: eventful-download.py 
#
# BY: Dmitry Sedov 
#
# CREATED: Sat Oct 26 2019
#
# DESC:
#
# EXEC: 
#      
################################################################################
################################################################################


############################# Libraries ########################################

import asyncio # Async code
import aiohttp # Async http requests
import json # Handling json
import requests # Standard requests
from urllib.parse import urlencode # Encoding dictionaries to url strings
import logging # Logging library
import os # OS tools
import time # Time the requests
import pandas # Import the zip codes tab-separated table
import random # Random numbers generation

################################################################################


############################# Constants ########################################

url = 'http://api.eventful.com/json/events/search?'
app_key = 'Db6PVXQNXjP6BPGH'
page_size = 250
# TODO: add US to the location string?
# location = 'United States'
date = '2017010100-2019100100'
category = 'music'
output_path = '/home/user/projects/urban/data/input/Events/eventful/'
page_numbers_path = ('/home/user/projects/urban/data/processed/'
        'intermediate_use/eventful_page_numbers/eventful_page_numbers.csv')
sample_size = 200

################################################################################


######################## Functions and classes #################################

def get_total_items(url):
    """ This function makes a request to determine the total number of
    results. """
    response = requests.get(url).json()
    return response['total_items']


def max_page(total, page_size):
    """ This function determines the number of pages to be requests to obtain
    all results items. """
    max_page = (int(total) // int(page_size)) + 1
    return max_page


async def get_items_from_zip_page(session, sem, url, zip_code, page_number):
    """ This function obtains all events on a specific response page. """
    logger = logging.getLogger(__name__)
    async with sem:
        async with session.get(url) as response:
            logger.info(f'Looking at zip {zip_code}, page {page_number}.')
            r = await response.text()
            # Save page
            file_name = f'zip{zip_code}page{page_number}.json'
            file_path = os.path.join(output_path, file_name)
            with open(file_path, 'w+') as output_file:
                output_file.write(r)
            # If first page was requested, add total number of pages per zip
            # code
            total_pages = json.loads(r)['page_count']
            if (page_number == 1):
                with open(page_numbers_path, 'a+') as page_numbers_file:
                    page_numbers_file.write(f'{zip_code},{total_pages}\n')
            return 1


async def run(tasks):
    """ This function creates a list of tasks to be done asyncronously. """
    results = []
    sem = asyncio.Semaphore(10)
    async with aiohttp.ClientSession() as session:
        async with sem:
            for task in tasks:
                url_to_get, zip_code, page_number = task
                results.append(asyncio.ensure_future(get_items_from_zip_page(session,
                    sem, url_to_get, zip_code, page_number)))
            return await asyncio.gather(*results)

################################################################################

if __name__ == '__main__':

    # Logging setup
    # Log file path
    log_file_path = ('/home/user/projects/urban/code/events-download' 
            + '/eventful-download/logs/eventful_download.log')
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.DEBUG)
    # Log handlers (console and file)
    ch = logging.StreamHandler()
    fh = logging.FileHandler(log_file_path)
    # Log levels
    ch.setLevel(logging.DEBUG)
    fh.setLevel(logging.DEBUG)
    # Log format
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    fh.setFormatter(formatter)
    # Add handlers
    logger.addHandler(fh)
    logger.addHandler(ch)

    # Import zip codes
    zip_codes_path = ('/home/user/projects/urban/data/'
            'input/Misc/zip-codes/US.txt')
    zip_codes_table = pandas.read_table(zip_codes_path,
            header = None,
            names = ['country_code',
                'postal_code',
                'place_name',
                'admin_name1',
                'admin_code1',
                'admin_name2',
                'admin_code2',
                'admin_name3',
                'admin_code3',
                'latitude',
                'longitude',
                'accuracy'
                ],
            dtype = {'postal_code': str}
            )
    zip_codes = zip_codes_table['postal_code'].tolist()
    logger.info(f'Zip codes imported. {len(zip_codes)} zip codes in total')
    
    logger.debug('Selecting {sample_size} zip codes for testing')
    zip_codes = random.sample(zip_codes, sample_size)

    # Loop through all zip codes in the US requesting the first page
    tasks = []
    for zip_code in zip_codes:
        # TODO: add US to the location string?
        params = {'app_key': app_key,
                'page_size': page_size,
                'location': zip_code,
                'date': date,
                'category': category,
                'page_number': 1}
        params = urlencode(params)
        url_to_add = url + params
        logger.debug('Will be requesting url {}'.format(url_to_add))
        task_to_add = [url_to_add, zip_code, 1]
        tasks.append(task_to_add)

    logger.info('Starting the event loop.')
    t0 = time.time()

    loop = asyncio.get_event_loop()
    temp = loop.run_until_complete(run(tasks))

    t1 = time.time()
    per_task = (t1 - t0) / len(tasks)
    logger.info(f'Total time: {t1 - t0}. Time per page: {per_task}.')


    # TODO;
    # Read the page number file
    # For zips with 1 < page_count <= 4, make additional requests
    # For zips with page_count > 4, break by date, make additional requests

################################################################################
