################################################################################
################################################################################
#
# FILE: 18-monthly-panel-with-events.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Nov 27 2019
#
# DESC: This code constructs a monthly panel with events dataset by
#       0) Filtered events are used: no events within restaurants are counted
#       1) Executing the spatial join to construct the pairs of colocated POIs (
#       the first element of the pairs is always a restaurant).
#       2) Importing the Patterns datasets of different vintages to PostgreSQL
#       to count visits and co-visits in a given month,
#       3) Selecting events corresponding to a month and performing a spatial
#       join to determine which locations are 'affected' by the events.
#       4) Exporting the resulting dataset to CSV
#
# EXEC: python3 18-monthly-panel-with-events.py
#      
################################################################################
################################################################################


############################# Libraries ########################################

import logging # Facilitates logging 
import psycopg2 # Interaction with the PostgreSQL databases
import pandas as pd # Data frames functionality

################################################################################


############################# Constants  ########################################

# Single colocation distance: for testing purposes
met = 200

# Vintages
vintages_2018 = [('2018', '{0:0=2d}'.format(x)) for x in range(12, 13)] 
vintages_2019 = [('2019', '{0:0=2d}'.format(x)) for x in range(1, 8)] 
vintages_all = vintages_2018 + vintages_2019

# Log file path
log_file_path = '/home/user/projects/urban/code/data-construction/logs/' \
        + 'monthly_events_panel_construction_clean.log'

# Array to store the locations of data parts
data_all = []

# Output data path
full_data_path = '/home/user/projects/urban/data/processed/descriptive/monthly_events_panel_200_clean.csv'

################################################################################


############################# SQL Statements ###################################

# Create the table with vintaged patterns
create_patterns_table_statement = """
CREATE TEMPORARY TABLE {patterns_table_name}(
    sname_place_id text PRIMARY KEY,
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


# Select restaurants, add location column, create indices (sname_place_id
# and spatial).
create_restaurants_geo_all_no_visits_statement = """
CREATE TEMPORARY TABLE restaurants AS (
    SELECT
        p.*,
        g.cbg AS cbg,
        g.cbsa AS cbsa
    FROM
        pois AS p
    INNER JOIN
        geometry_pois AS g
    ON
        p.sname_place_id = g.sname_place_id
    WHERE 
        p.naics_code IN (722511, 722513)
);

CREATE UNIQUE INDEX restaurants_sg_idx 
ON restaurants (sname_place_id);

CREATE TEMPORARY TABLE restaurants_with_geo AS
    SELECT
        r.sname_place_id,
        g.location::geography AS location
    FROM
        restaurants AS r
    LEFT JOIN
        geometry_pois AS g
    ON
        r.sname_place_id = g.sname_place_id
;
CREATE INDEX restaurants_with_geo_location_idx
ON restaurants_with_geo
USING GIST (location);
"""


# Select non-restaurants, add location column, create indices (sname_place_id
# and spatial).
create_non_restaurants_geo_all_no_visits_statement = """
CREATE TEMPORARY TABLE non_restaurants AS
    SELECT
        *
    FROM
        pois
    WHERE 
        naics_code NOT IN (722511, 722513)
;

CREATE UNIQUE INDEX non_restaurants_sg_idx 
ON non_restaurants (sname_place_id);

CREATE TEMPORARY TABLE non_restaurants_with_geo AS
    SELECT
        n.sname_place_id,
        g.location::geography AS location
    FROM
        non_restaurants AS n
    INNER JOIN
        geometry_pois AS g
    ON
        n.sname_place_id = g.sname_place_id
;

CREATE INDEX non_restaurants_with_geo_location_idx
ON non_restaurants_with_geo
USING GIST (location);
"""

# Join restaurants with non-restaurants by distance. That is, all POIs in
# proximity to each restaurant are stored in the resulting table.
spatial_join_meter_statement = """
CREATE TEMPORARY TABLE joined_by_distance_all AS (
    SELECT 
        rg.sname_place_id AS sname_place_id, 
        nrg.sname_place_id AS co_sname_place_id
    FROM 
        restaurants_with_geo AS rg
    LEFT JOIN 
        non_restaurants_with_geo AS nrg
    ON 
        ST_DWithin(rg.location, nrg.location, {meters})
);
"""

# Create a table with restaurants open in a given month (by joining to
# patterns). Then (left) join with non-restaurants to only include POIs that
# are also open in that given month.
create_this_month_statement = """
CREATE TEMPORARY TABLE this_month AS (
    WITH restaurants_filtered_this_month AS (
        SELECT 
            j.*,
            p.dummy AS restaurant_open
        FROM 
            {jbda} AS j
        INNER JOIN
            {ptn} AS p
        ON 
            j.sname_place_id = p.sname_place_id
        )
    SELECT 
        r.*,
        p.raw_visit_counts AS co_visits
    FROM 
        restaurants_filtered_this_month AS r
    LEFT JOIN
        {ptn} AS p
    ON 
        r.co_sname_place_id = p.sname_place_id
);
"""

# group by sname_place_id (restaurant), count POIs open in that month in
# proximity to the restaurant.
create_aggregated_this_month_statement = """
CREATE TEMPORARY TABLE aggregated_this_month AS (
    SELECT 
        sname_place_id,
        SUM(co_visits) AS co_visits_all
    FROM
        this_month AS t
    GROUP BY
        sname_place_id
);
"""

# Select events corresponding to the current month, create a spatial index on
# this temporary table.
create_clean_events_this_month_stamtement = """
CREATE TEMPORARY TABLE clean_events_this_month AS (
    SELECT 
        id,
        location::geography AS location
    FROM
        music_outside_restaurants
    WHERE
        year = {year} AND month = {month} AND geocode_type = 'EVDB Geocoder'
);
CREATE INDEX clean_events_this_month_location_idx
ON clean_events_this_month
USING GIST (location);
"""

# Do the spatial join to mark events that are in proximity to each restaurant.
spatial_join_events_statement = """
CREATE TEMPORARY TABLE clean_coevents_this_month AS (
    WITH events_joined AS (
        SELECT 
            r.sname_place_id AS sname_place_id,
            e.id AS event_id
        FROM 
            restaurants_with_geo AS r
        LEFT JOIN
            clean_events_this_month AS e
        ON
            ST_DWithin(r.location, e.location, {meters})
    )
    SELECT
        sname_place_id,
        COUNT(event_id) AS events_count
    FROM 
        events_joined
    GROUP BY
        sname_place_id
);
CREATE UNIQUE INDEX clean_coevents_this_month_sg_idx
ON clean_coevents_this_month (sname_place_id);
"""

# Statement to export the resulting month-table.
export_statement = """
COPY (SELECT * FROM aggregated_this_month) TO STDOUT
WITH CSV HEADER;
"""
################################################################################


######################## Functions and classes #################################

def join_by_distance_all(cur, meter):
    """ This function creates a table with pairs of all restaurants and
    co-location POIs. """
    # Logging
    logger = logging.getLogger(__name__)

    # Create temporary restaurants and non-restaurants tables 
    logger.info('Creating temporary restaurants and non-restaurants tables.')
    l_create_restaurants_geo_all_no_visits_statement = (
            create_restaurants_geo_all_no_visits_statement
            )
    l_create_non_restaurants_geo_all_no_visits_statement = (
            create_non_restaurants_geo_all_no_visits_statement
            )
    cur.execute(l_create_restaurants_geo_all_no_visits_statement)
    cur.execute(l_create_non_restaurants_geo_all_no_visits_statement)

    # Perform the spatial join
    logger.info(f'Performing the spatial join restaurants-non-restaurants for {meter} meters.')
    l_spatial_join_meter_statement = spatial_join_meter_statement.format(
            meters = meter
            )
    cur.execute(l_spatial_join_meter_statement)
    # Now the table joined_by_distance_all contains all pairs of restaurants
    # with close POIs.

    return "joined_by_distance_all"


def import_patterns_vintage(cur, vintage):
    """ This function imports the Patterns dataset for a particular vintage
    (year, month). """

    # Logging
    logger = logging.getLogger(__name__)

    year, month = vintage
    # Construct path to file from vintage
    file_name = f'all-us-2-years-PATTERNS-{year}_{month}-2019-08-28.csv'
    path_to_file = '/home/user/projects/urban/data/input/Patterns/' + file_name
    # Construct the temporary table name from file 
    patterns_table_name = f'patterns_{year}_{month}' 

    # Create table statement
    logger.info(f'Importing the table for vintage {year}_{month} from file'
    f'{path_to_file}.')
    l_create_patterns_table_statement = create_patterns_table_statement.format(
            patterns_table_name = patterns_table_name)

    # Import the table statement
    import_statement = "COPY {patterns_table_name} FROM '{path_to_file}' " \
    "DELIMITER ',' CSV HEADER;"
    import_statement = import_statement.format(
            patterns_table_name = patterns_table_name,
            path_to_file = path_to_file
            )

    # Execute the create and import statements
    cur.execute(l_create_patterns_table_statement)
    cur.execute(import_statement)

    logger.info(f'Created table {patterns_table_name}.')
    return patterns_table_name


def export_covisits_coevents(cur, vintage):
    """ This function exports the constructed month of the panel data. """
    
    # Logging
    logger = logging.getLogger(__name__)

    year, month = vintage
    output_file_name = f'clean_covisits_coevents_{year}_{month}_{met}.csv'
    output_file_path = ('/home/user/projects/urban/data/processed/descriptive/'
            + output_file_name)

    l_export_statement = export_statement
    
    logger.info(f'Exporting to {output_file_name}.')
    # Output to file
    with open(output_file_path, 'w+') as output_file:
        cur.copy_expert(l_export_statement, output_file)

    return output_file_path


def main_join_events_then_count(conn, cur, meter):
    """ This function loops through vintages, constructs the co-visits and
    co-events dataset corresponding to each vintage. """
    # Allow to access the list of paths to which the data has been exported
    global data_all

    logger = logging.getLogger(__name__)
    # Construct the co-location table (restaurants vs other establishments. 
    logger.info(f'Constructing the colocation-coevents table for {meter} meters.')
    jbda = join_by_distance_all(cur, meter)
    conn.commit()
    logger.info(f'Done constructing the colocation-coevents table for {meter} meters.')

    vintages = vintages_all
    logger.info(f'Will be looping through vintages {vintages}.')
    for vintage in vintages:
        year, month = vintage
        # Import the patterns data of that vintage
        logger.info(f'Importing vintage {year}_{month}.')
        ptn = import_patterns_vintage(cur, vintage)

        logger.info('Adding the dummy column.')
        add_dummy_statement = """
        ALTER TABLE {ptn}
        ADD COLUMN dummy INTEGER;
        """.format(ptn = ptn)
        cur.execute(add_dummy_statement)

        set_dummy_statement = """
        UPDATE {ptn}
        SET dummy = 1;
        """.format(ptn = ptn)
        cur.execute(set_dummy_statement)

        # Join restaurants with patterns to mark which are open and the record
        # the visit count
        logger.info(f'Selecting open restaurants for vintage {year}_{month}.')
        l_create_this_month_statement = create_this_month_statement.format(
                ptn = ptn,
                jbda = jbda
                )
        cur.execute(l_create_this_month_statement)

        # Left inner join joined_by_distance with patterns to aggregate
        # co_visits after grouping by restaurant's sname place id.
        logger.info(f'Counting co-visits.')
        l_create_aggregated_this_month_statement = (
                create_aggregated_this_month_statement
                )
        cur.execute(l_create_aggregated_this_month_statement)

        # Add month year columns
        logger.info(f'Adding year {year} and month {month} columns.')
        add_year_month_statement = """
        ALTER TABLE aggregated_this_month
        ADD COLUMN year INTEGER,
        ADD COLUMN month INTEGER;
        """
        cur.execute(add_year_month_statement)

        set_year_month_statement = """
        UPDATE aggregated_this_month
        SET year = {year},
         month = {month};
        """.format(year = year, month = month)
        cur.execute(set_year_month_statement)

        # Add cbg and cbsa columns
        logger.info('Adding cbg and cbsa columns.')
        add_cbg_cbsa_statement = """
        ALTER TABLE aggregated_this_month 
        ADD COLUMN cbg TEXT,
        ADD COLUMN cbsa varchar(5);
        """
        cur.execute(add_cbg_cbsa_statement)

        set_cbg_cbsa_statement = """
        UPDATE aggregated_this_month
        SET 
            cbg = g.cbg,
            cbsa = g.cbsa
        FROM geometry_pois AS g
        WHERE aggregated_this_month.sname_place_id = g.sname_place_id;
        """
        cur.execute(set_cbg_cbsa_statement)

        # Add own_visits column
        logger.info('Adding the own_visits column.')
        add_own_visits_statement = """
        ALTER TABLE aggregated_this_month 
        ADD COLUMN own_visits INTEGER;
        """
        cur.execute(add_own_visits_statement)

        set_own_visits_statement = """
        UPDATE aggregated_this_month
        SET
            own_visits = p.raw_visit_counts
        FROM {ptn} AS p
        WHERE aggregated_this_month.sname_place_id = p.sname_place_id;
        """.format(ptn = ptn)
        cur.execute(set_own_visits_statement)

        # Select events this month
        logger.info('Selecting events for year {year}, month {month}.')
        l_create_clean_events_this_month_stamtement = create_clean_events_this_month_stamtement.format(year = year, month = month)
        cur.execute(l_create_clean_events_this_month_stamtement)

        # Spatial join aggregated_this_month with clean_events_this_month,
        # count events nearby each restaurant.
        logger.info('Doing a spatial join of places and events.')
        l_spatial_join_events_statement = spatial_join_events_statement.format(meters = meter)
        cur.execute(l_spatial_join_events_statement)

        # Add co_events column
        logger.info('Adding the co_events column. Only events outside of restaurants are considered.')
        add_coevents_statement = """
        ALTER TABLE aggregated_this_month 
        ADD COLUMN co_events INTEGER;
        """
        cur.execute(add_coevents_statement)
        # Join own visits, covisits and coevents for export
        set_coevents_statement = """
        UPDATE aggregated_this_month
        SET
            co_events = cetm.events_count
        FROM clean_coevents_this_month AS cetm
        WHERE aggregated_this_month.sname_place_id = cetm.sname_place_id;
        """
        cur.execute(set_coevents_statement)

        # Export the month-table
        logger.info(f'Exporting the {year}_{month}-vintage covistits and coevents.')
        ofp = export_covisits_coevents(cur, vintage)
        logger.info(f'Done with vintage {year}_{month}.')

        # Save the path to which the data has been exported
        logger.info(f'Saving the output path: {ofp}.')
        data_all.append(ofp)
        
        # Undo the operations: drop the tables corresponding to the current
        # vintage.
        logger.info(f'Rolling back: deleting the unnecessary tables.')
        conn.rollback()

    return 1

def initiate_logging():
    # Initiate logger
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.DEBUG)
    # Initiate console and file handlers
    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    fh = logging.FileHandler(log_file_path)
    fh.setLevel(logging.DEBUG)
    # Add and set the formatter for the handlers
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    fh.setFormatter(formatter)
    # Add the handlers to the logger
    logger.addHandler(ch)
    logger.addHandler(fh)
    return logger

################################################################################


############################# Main code ########################################

if __name__ == '__main__':

    # Initiate logging 
    logger = initiate_logging()

    # Initiate the database connection
    conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')
    cur = conn.cursor()
    logger.info('Database connection created.')
    m = main_join_events_then_count(conn, cur, met)

    # Close the database connection
    conn.close()
    logger.info('Completed. Database connection closed.')

    # # Append all of the tables for a single panel
    # logger.info('Concatenating data frames.')
    # data = []
    # for data_path in data_all:
        # logger.info(f'Appending file {data_path}.')
        # data = data.append(pd.read_csv(data_path, dtype = {'cbg': pd.Int64Dtype(),
            # 'cbsa': pd.Int64Dtype()}))
    # data = pd.concat(data, ignore_index = False)
    # data.to_csv(full_data_path, index = False)
    # logger.info(f'Data frames concatenated, written to {full_data_path}.')

################################################################################
