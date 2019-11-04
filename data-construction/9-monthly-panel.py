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


create_this_month_statement = """
CREATE TEMPORARY TABLE this_month AS (
    WITH restauratns_filtered_this_month AS (
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
        p.dummy AS co_poi_open
    FROM 
        restauratns_filtered_this_month AS r
    INNER JOIN
        {ptn} AS p
    ON 
        r.co_sname_place_id = p.sname_place_id
);
"""


create_aggregated_this_month_statement = """
CREATE TEMPORARY TABLE aggregated_this_month AS (
    SELECT 
        sname_place_id,
        COUNT (DISTINCT co_sname_place_id)
    FROM
        this_month AS t
    GROUP BY
        sname_place_id
);
"""


export_statement = """
COPY (SELECT * FROM aggregated_this_month) TO STDOUT
WITH CSV HEADER;
"""

################################################################################


######################## Functions and classes #################################

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

    return patterns_table_name


def construct_colocation_metrics(cur, vintage, meter):
    """ This function constructs a temporary table with monthly-level
    co-location metrics for each restaurant. """
    
    # Logging
    logger = logging.getLogger(__name__)

    # Construct the temporary table name from file 
    patterns_table_name = f'patterns_{year}_{month}' 

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

    year, month = vintage
    output_file_name = f'pois_close_{year}_{month}.csv'
    output_file_path = ('/home/user/projects/urban/data/processed/descriptive/'
            + output_file_name)

    l_export_statement = export_statement

    # Output to file
    with open(output_file_path, 'w+') as output_file:
        cur.copy_expert(l_export_statement, output_file)

    # TODO: DROP UNECESSARY TABLES
    conn.rollback()

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


def main_sequential_joins(cur, meter):
    """ This function import patterns sequentially, performs joins and counts
    POIs in proximity. """
    # Loop through the available months
    logger = logging.getLogger(__name__)
    logger.info('Starting the database operations.')
    vintages = []
    for vintage in vintages:
        year, month = vintage
        # Import the monthly patterns
        logger.info(f'Importing vintage {year}_{month}.')
        ptn = import_patterns_vintage(cur, vintage)
        # Construct the colocation-metrics table for this month
        logger.info(f'Starting to create colocation metrics for vintage {year}_{month}.')
        for meter in meters:
            logger.info(f'Constructing {meter}-meters metric for vintage
                    {year}_{month}.')
            ptn = construct_colocation_metrics(cur, vintage, meter)
            logger.info(f'Done constructing {meter}-meters metric for vintage
                    {year}_{month}.')
        # Export the colocation-metrics table
        logger.info('Exporting the {year}_{month}-vintage colocation metrics.')
        ofp = export_colocation_metrics(cur, vintage)
        logger.info(f'Done importing vintage {year}_{month}.')


def join_by_distance_all(cur, meter):
    """ This function creates a table with pairs of all restaurants and
    co-location POIs. """
    # Logging
    logger = logging.getLogger(__name__)

    # Create temporary restaurants and non-restaurants tables 
    l_create_restaurants_geo_all_no_visits_statement = (
            create_restaurants_geo_all_no_visits_statement
            )
    l_create_non_restaurants_geo_all_no_visits_statement = (
            create_non_restaurants_geo_all_no_visits_statement
            )
    cur.execute(l_create_restaurants_geo_all_no_visits_statement)
    cur.execute(l_create_non_restaurants_geo_all_no_visits_statement)

    # Perform the spatial join
    l_spatial_join_meter_statement = spatial_join_meter_statement.format(
            meters = meter
            )
    cur.execute(l_spatial_join_meter_statement)
    # Now the table joined_by_distance_all contains all pairs of restaurants
    # with close POIs.

    return "joined_by_distance_all"


def main_join_then_count(cur, meter):
    
    # Loop through distances? 
    # Join restaurants with non-restaurants by distance to obtain
    # joined_by_distance

    jbda = join_by_distance_all(cur, meter)

    # Loop through vintages
    vintages = [] 
    for vintage in vintages:
        year, month = vintage
        # Import the patterns data of that vintage
        ptn = import_patterns_vintage(cur, vintage)

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
        l_create_this_month_statement = create_this_month_statement.format(
                ptn = ptn,
                jbda = jbda
                )
        cur.execute(l_create_this_month_statement)

        # Left inner join joined_by_distance with patterns to mark which POIs are open
        # in proximity, aggregate for the count of such open POIs after grouping 
        # by restaurants sname_place_id.
        l_create_aggregated_this_month_statement = (
                create_aggregated_this_month_statement
                )
        cur.execute(l_create_aggregated_this_month_statement)

        # Add month year columns
        add_year_statement = """
        ALTER TABLE aggregated_this_month
        ADD COLUMN year INTEGER;
        """
        cur.execute(add_year_statement)

        set_year_statement = """
        UPDATE aggregated_this_month
        SET year = {year};
        """.format(year = year)
        cur.execute(set_year_statement)
        
        add_month_statement = """
        ALTER TABLE aggregated_this_month
        ADD COLUMN month INTEGER;
        """
        cur.execute(add_month_statement)

        set_month_statement = """
        UPDATE aggregated_this_month
        SET month = {month};
        """.format(month = month)
        cur.execute(set_month_statement)

        # Add cbg and cbsa columns
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
        

################################################################################


############################# Main code ########################################

if __name__ == '__main__':

    # Initiate logging 
    logger = initiate_logging()

    # Initiate the database connection
    conn = psycopg2.connect('dbname=dataname1 user={user} password={user_pass}')
    cur = conn.cursor()
    logger.info('Database connection created.')


################################################################################
