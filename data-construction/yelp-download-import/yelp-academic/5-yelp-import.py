################################################################################
################################################################################
#
# FILE: 5-yelp-import.py
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Oct 10 2019
#
# DESC: This file contains the code that imports the Yelp Academic Dataset into
# PostgreSQL database.
#
# EXEC: python3 5-yelp-import.py
#      
################################################################################
################################################################################


############################## Libraries #######################################  

import psycopg2 # PostgreSQL interaction
import json

################################################################################  


# Create the database connection
connection = psycopg2.connect(database = 'dataname1', user = 'user')
cursor = connection.cursor()

# Create table with Yelp buses location
table_name = 'yelp_locations'
column_types = 'business_id TEXT, name TEXT, city TEXT, state TEXT, ' \
+ 'postal_code TEXT, longitude NUMERIC, latitude NUMERIC'
cursor.execute('DROP TABLE IF EXISTS {};'.format(table_name))
cursor.execute('CREATE TABLE {} ({});'.format(table_name, column_types))
connection.commit()

# Data import
current_line = 0
yelp_data_path = '/home/user/projects/urban/data/input/Yelp/academic/business.json'
with open(yelp_data_path, 'r') as f:
    for line in f:
        bus = json.loads(line)
        bid = bus['business_id'] if 'business_id' in bus else ''
        name = bus['name'] if 'name' in bus else ''
        city = bus['city'] if 'city' in bus else ''
        state = bus['state'] if 'state' in bus else ''
        bzip = bus['postal_code'] if 'postal_code' in bus else ''
        lon = bus['longitude'] if 'longitude' in bus else ''
        lat = bus['latitude'] if 'latitude' in bus else ''
        values = f"$b${bid}$b$, $n${name}$n$, $c${city}$c$, $s${state}$s$, $z${bzip}$z$, {lon}, {lat}"
        cursor.execute(f"INSERT INTO {table_name} VALUES ({values});")
        if (current_line % 1000 == 0):
            print(current_line)
        current_line = current_line + 1

connection.commit()
