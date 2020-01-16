############################# Libraries ########################################

import logging # Facilitates logging 
import psycopg2 # PostgreSQL interaction
import sqlalchemy # SQL interaction
import pandas 
import requests # Facilitates making http requests
import json # Facilitates handling of json objects
import sys

################################################################################

print('logging version ' + logging.__version__)
print('psycopg2 version ' + psycopg2.__version__)
print('sqlalchemy version ' + sqlalchemy.__version__)
print('pandas version ' + pandas.__version__)
print('requests version ' + requests.__version__)
print('json version ' + json.__version__)

