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

################################################################################


############################# Constants ########################################

url = 'http://api.eventful.com/json/events/search?'
app_key = 'Db6PVXQNXjP6BPGH'
page_size = 250
location = 'United States'
date = 'Past'
category = 'music'

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

async def get_results(session, sem, zip_code, part, url, i):

################################################################################


if __name__ == '__main__':

    # Logging setup
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
    params = {'app_key': app_key,
            'page_size': page_size,
            'location': location,
            'date': date,
            'category': category}
    params = urlencode(params)
    logger.debug('Requesting url {}'.format(url + params))
    total_items = get_total_items(url + params)
    pages = max_page(total_items, page_size)
    logger.debug(f'Total items: {total_items}. Total pages to be requested is'
            f' {pages} with {page_size} items per page.')
