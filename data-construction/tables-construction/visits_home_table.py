################################################################################ 
################################################################################
#
# FILE: visits_homes_tables.ipynb
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Feb 10 2020
#
# DESC: This file contains the code that creates tables with visits counts and 
#       device counts by home cbg. 
#       It does so by looping through months 06/2017 - 07/2019, for each month 
#           1. unpacking the respective zip
#           2. importing the zip contents into a temporary table
#           3. appending the temporary tables (with year, month columns) to the
#              'final' visits / homes  tables.
# 
# EXEC: 
#      
################################################################################
################################################################################


# In[ ]:


################################ Libraries #####################################

import os
import zipfile
import gzip
import shutil
import logging
import psycopg2

################################################################################


# In[ ]:


############################# SQL statements ###################################

# Visits-related statements
create_perm_visits_table_statement = """
CREATE TABLE visits (                                                                                                                                                                                                 
    sname_place_id text,
    date_range_start integer,
    date_range_end integer,
    raw_visit_counts integer,
    raw_visitor_counts integer,
    visits_by_day json,
    visitor_home_cbgs json,
    visitor_work_cbgs json,
    distance_from_home integer,
    median_dwell double precision,
    device_type json,
    year integer,
    month integer
);
"""

create_temp_visits_table_statement = """
CREATE TEMPORARY TABLE {temp_visits_table_name} (                                                                                                                                                                                                 
    sname_place_id text,
    location_name text,
    street_address text,
    city text,
    state text,
    zip_code integer,
    brands text,
    date_range_start integer,
    date_range_end integer,
    raw_visit_counts integer,
    raw_visitor_counts integer,
    visits_by_day json,
    visitor_home_cbgs json,
    visitor_work_cbgs json,
    visitor_country_of_origin json,
    distance_from_home integer,
    median_dwell double precision,
    bucketed_dwell_times json,
    related_same_day_brand json,
    related_same_month_brand json,
    popularity_by_hour json,
    popularity_by_day json,
    device_type json
);
"""

copy_visits_table_statement = """
COPY {temp_visits_table_name}
FROM '{visits_path_to_file}'
DELIMITER ',' CSV HEADER
;
"""

append_visits_table_statement = """
INSERT INTO {permanent_visits_table_name} (
    sname_place_id,
    date_range_start,
    date_range_end,
    raw_visit_counts,
    raw_visitor_counts,
    visits_by_day,
    visitor_home_cbgs,
    visitor_work_cbgs,
    distance_from_home,
    median_dwell,
    device_type,
    year,
    month
)
SELECT 
    sname_place_id,
    date_range_start,
    date_range_end,
    raw_visit_counts,
    raw_visitor_counts,
    visits_by_day,
    visitor_home_cbgs,
    visitor_work_cbgs,
    distance_from_home,
    median_dwell,
    device_type,
    {year} AS year,
    {month} AS month
FROM
    {temp_visits_table_name}
;
"""

drop_temp_visits_table_statement = """
DROP TABLE {temp_visits_table_name};
"""

# Home-related statements
create_perm_home_table_statement = """
CREATE TABLE home (                                                                                                                                                                                                 
    year integer,
    month integer,
    state text, 
    census_block_group text,
    number_devices_residing integer
);
"""

copy_perm_home_table_statement = """
COPY {permanent_home_table_name}                                                                                                                            
FROM '{home_path_to_file}'
DELIMITER ',' CSV HEADER
;
"""
################################################################################


# In[ ]:


############################ Constants, settings ###############################

patterns_folder_path = '/home/user/projects/urban/data/input/Patterns'

log_file_path = ('/home/user/projects/urban/code/data-construction/'
                 'tables-construction/logs/visits_home_table.log'
                )

# Logging settings
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
ch = logging.StreamHandler()
ch.setLevel(logging.INFO)
fh = logging.FileHandler(log_file_path)
fh.setLevel(logging.INFO)
# Add and set the formatter for the handlers
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
ch.setFormatter(formatter)
fh.setFormatter(formatter)
# Add the handlers to the logger
logger.addHandler(ch)
logger.addHandler(fh)

################################################################################


# In[ ]:


################################ Functions #####################################

def unpack(year, month):
    
    logger = logging.getLogger(__name__)
    
    # Input file name
    file_name = 'all-us-2-years-PATTERNS-{0}_{1:0=2d}-2019-08-28.zip'.format(
        year, month
    )
    file_path = os.path.join(patterns_folder_path, file_name)
    
    # Create temporary folder
    logger.info(f'Creating temporary folder for year {year}, month {month}.')
    temp_folder_name = '{0}-{1:0=2d}'.format(year, month)
    temp_folder_path = os.path.join(patterns_folder_path, temp_folder_name)
    if not os.path.exists(temp_folder_path):
        os.mkdir(temp_folder_path)
    
    # Unzip to the temporary folder
    logger.info(f'Unzipping.')
    with zipfile.ZipFile(file_path, 'r') as zip_file:
        zip_file.extractall(temp_folder_path)
    
    # Gunzip the file
    logger.info(f'Gunzipping.')
    gz_file_name = 'all-us-2-years-PATTERNS-{0}_{1:0=2d}-2019-08-28.csv.gz'.format(
        year, month
    )
    gz_file_path = os.path.join(temp_folder_path, gz_file_name)
    visits_file_name = 'all-us-2-years-PATTERNS-{0}_{1:0=2d}-2019-08-28.csv'.format(
        year, month
    )
    visits_file_path = os.path.join(temp_folder_path, visits_file_name)
    
    with gzip.open(gz_file_path, 'rb') as f_in:
        with open(visits_file_path, 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)
    
    # Return the tempfolder, visits file, home file paths
    home_file_name = 'home_panel_summary-{0}_{1:0=2d}-2019-08-28.csv'.format(
        year, month
    )
    home_file_path = os.path.join(temp_folder_path, home_file_name)
    
    return temp_folder_path, visits_file_path, home_file_path


def import_visits_home(year, month, visits_file_path, home_file_path):
    
    logger = logging.getLogger(__name__)
    
    # Create connection and cursor
    conn = psycopg2.connect('dbname=dataname2 user={user} password={user_pass}')
    cur = conn.cursor()
    
    # Table names 
    temp_visits_table_name = 'visits_{0}_{1:0=2d}'.format(year, month)
    perm_visits_table_name = 'visits'
    permanent_home_table_name = 'home'
    
    # File paths
    visits_path_to_file = visits_file_path
    home_path_to_file = home_file_path
    
    # Formatted statements
    l_create_temp_visits_table_statement = create_temp_visits_table_statement.format(
        temp_visits_table_name = temp_visits_table_name
    )
    
    l_copy_visits_table_statement = copy_visits_table_statement.format(
        temp_visits_table_name = temp_visits_table_name, 
        visits_path_to_file = visits_path_to_file
    )
    
    l_append_visits_table_statement = append_visits_table_statement.format(
        permanent_visits_table_name = perm_visits_table_name,
        temp_visits_table_name = temp_visits_table_name,
        year = year,
        month = month
    )
    
    l_drop_temp_visits_table_statement = drop_temp_visits_table_statement.format(
        temp_visits_table_name = temp_visits_table_name
    )
    
    l_copy_perm_home_table_statement = copy_perm_home_table_statement.format(
        permanent_home_table_name = permanent_home_table_name,
        home_path_to_file = home_path_to_file
    )
    
    # Execute the visits-related statements
    logger.info('Importing the visits data.')
    cur.execute(l_create_temp_visits_table_statement)
    cur.execute(l_copy_visits_table_statement)
    cur.execute(l_append_visits_table_statement)
    cur.execute(l_drop_temp_visits_table_statement)
    
    # Execute the home-related statements
    logger.info('Importing the home data.')
    cur.execute(l_copy_perm_home_table_statement)
    
    # Commmit and close connection
    conn.commit()
    conn.close()
    
    return

################################################################################


# In[ ]:


# Initiate the home and visits table
conn = psycopg2.connect('dbname=dataname2 user={user} password={user_pass}')
cur = conn.cursor()
cur.execute(create_perm_visits_table_statement)
cur.execute(create_perm_home_table_statement)
conn.commit()
conn.close()


# In[ ]:


# All data vintages
vintages_2017 = [(2017, x) for x in range(6, 13)]
vintages_2018 = [(2018, x) for x in range(1, 13)]                                                                                                                                                                       
vintages_2019 = [(2019, x) for x in range(1, 8)]
vintages = vintages_2017 + vintages_2018 + vintages_2019


# In[ ]:


# Loop through data vintages 
for year, month in vintages:
    
    logger.info(f'Working on year {year}, month {month}.')
    # Unpack
    temp_folder_path, visits_file_path, home_file_path = unpack(year, month)

    # Import into the database
    import_visits_home(year, month, visits_file_path, home_file_path)
    
    # Clean up - delete the folder with extracted data
    logger.info('Cleaning up.')
    shutil.rmtree(temp_folder_path)
    
    logger.info(f'Done working on year {year}, month {month}.')

