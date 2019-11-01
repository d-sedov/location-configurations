################################################################################
################################################################################
#
# FILE: 
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Oct 29 2019
#
# DESC: This code constructs a monthly panel dataset by
#       1) Importing the Patterns datasets of different vintages to PostgreSQL
#       2) Executing the spatial joins to construct the monthly co-location
#       metrics and co-visits.
#       3) Exporting the resulting dataset to CSV
#
# EXEC: 
#      
################################################################################
################################################################################


############################# Libraries ########################################

import logging # Facilitates logging 
import psycopg2 # Interaction with the PostgreSQL databases

################################################################################


############################# SQL Statements ###################################

# Create the table with vintaged patterns
create_patterns_table_statement = """
CREATE TABLE {patterns_table_name}(
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
# and spatial. Only restaurants open in that month are considers (INNER JOIN).
create_restaurants_geo_statement = """
CREATE TEMPORARY TABLE restaurants AS (
    SELECT
        p.*,
        g.cbg AS cbg,
        g.cbsa AS cbsa,
        pa.raw_visit_counts AS 
    FROM
        pois AS p
    INNER JOIN
        geometry_pois AS g
    ON
        p.sname_place_id = g.sname_place_id
    INNER JOIN
        {patterns_table_name} AS pa
    ON
        p.sname_place_id = pa.sname_place_id
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
# and spatial. Only non-restaurants open in that month are considers (INNER JOIN).
create_non_restaurant_geo_statement = """
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
        g.location::geography AS location,
        p.raw_visit_counts AS visits
    FROM
        non_restaurants AS n
    INNER JOIN
        geometry_pois AS g
    ON
        n.sname_place_id = g.sname_place_id
    INNER JOIN
        {patterns_table_name} AS p
    ON
        n.sname_place_id = p.sname_place_id
;

CREATE INDEX non_restaurants_with_geo_location_idx
ON non_restaurants_with_geo
USING GIST (location);
"""

export_statement = """
COPY (SELECT * FROM restaurants) TO STDOUT
WITH CSV HEADER;
"""

################################################################################


######################## Functions and classes #################################

def import_patterns_vintage(cur, vintage):
    """ This function imports the Patterns dataset for a particular vintage
    (year, month). """

    # Logging
    logger = logging.getLogger(__name__)

    # Construct path to file from vintage
    path_to_file = 
    # Construct the temporary table name from file 
    patterns_table_name = f'patterns_{vintage}' 

    # Create table statement
    l_create_patterns_table_statement = create_patterns_table_statement.format(
            patterns_table_name = patterns_table_name)

    # Import the table statement
    import_statement = "COPY {patterns_table_name} FROM {path_to_file} " \
    "DELIMITER ',' CSV HEADER;"
    import_statement = import_statement.format(
            patterns_table_name = patterns_table_name,
            path_to_file = path_to_file
            )

    # Execute the create and import statements
    cur.execute(l_create_patterns_table_statement)
    cur.execute(import_statement)

    return patterns_table_name


def construct_colocation_metrics(cur, vintage, meter):
    """ This function constructs a temporary table with monthly-level
    co-location metrics for each restaurant. """
    
    # Logging
    logger = logging.getLogger(__name__)

    # Construct the temporary table name from file 
    patterns_table_name = f'patterns_{vintage}' 

    # Create temporary restaurants and non-restaurants tables 
    l_create_restaurants_geo_statement = create_restaurants_geo_statement.format(
            patterns_table_name = patterns_table_name)
    l_create_non_restaurants_geo_statement = (
            create_non_restaurant_geo_statement.format(
                patterns_table_name = patterns_table_name
                )
            )
    cur.execute(l_create_patterns_table_statement)
    cur.execute(l_create_non_restaurants_geo_statement)

    # Perform the spatial joins, counting POIs in proximity and visits in
    # proximity. The columns are added to the temporary restaurants table.
    perform_statement = "PERFORM pois_within({});".format(meter)
    cur.execute(perform_statement)

    return patterns_table_name


def export_colocation_metrics(cur, vintage):
    """ This function exports the constructed month of the panel data. """
    
    # Logging
    logger = logging.getLogger(__name__)

    output_file_name = 
    output_file_path = 

    l_export_statement = export_statement

    # Output to file
    with open(output_file_path, 'w+') as output_file:
        cur.copy_expert(l_export_statement, output_file)

    return output_file_path


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
    logger.info('Database connection created')

    # Loop through the available months
    logger.info('Starting the database operations.')
    vintages = []
    for vintage in vintages:
        # Import the monthly patterns
        logger.info(f'Importing vintage {vintage}.')
        ptn = import_patterns_vintage(cur, vintage)
        # Construct the colocation-metrics table for this month
        logger.info(f'Starting to create colocation metrics for vintage {vintage}.')
        for meter in meters:
            logger.info(f'Constructing {meter}-meters metric for vintage
                    {vintage}')
            ptn = construct_colocation_metrics(cur, vintage, meter)
            logger.info(f'Done constructing {meter}-meters metric for vintage
                    {vintage}')
        # Export the colocation-metrics table
        logger.info('Exporting the {vintage}-vintage colocation metrics')
        ofp = export_colocation_metrics(cur, vintage)
        logger.info(f'Done importing vintage {vintage}.')

################################################################################
