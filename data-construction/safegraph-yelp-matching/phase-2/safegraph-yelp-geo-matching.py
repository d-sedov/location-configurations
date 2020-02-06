################################################################################
################################################################################
#
# FILE: sname-yelp-geo-matching.py
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Feb 5 2020
#
# DESC: This file contains the code that creates a matching between
# sname_place_id and Yelp id based on:
#       1) geographical proximity
#
# EXEC: 
#      
################################################################################
################################################################################


############################# Libraries ########################################

import logging # Facilitates logging 
import json # Facilitates handling of json objects
import pandas as pd # Working with tables of data
import sqlalchemy as db # Working with databases
import psycopg2 # PostgreSQL interactions

################################################################################


############################## SQL statements ##################################

# Select restaurants, add location column, create indices (sname_place_id
# and spatial).
create_restaurants_geo_all_statement = """
CREATE TEMPORARY TABLE restaurants AS (
    SELECT
        p.sname_place_id,
        p.location_name,
        p.zip_code,
        p.city,
        p.state,
        p.street_address,
        p.phone_number,
        g.location::geography AS location
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

CREATE INDEX restaurants_location_idx
ON restaurants
USING GIST (location);
"""

create_yelp_all_copy_statement = """
CREATE TEMPORARY TABLE yelp_all_copy AS (
    SELECT 
        *,
        location::geography AS location_geography
    FROM
        all_yelp_restaurants
);

CREATE INDEX yelp_all_copy_location_geography_idx
ON yelp_all_copy
USING GIST (location_geography);
"""

sname_yelp_spatial_join_statement = """
CREATE TEMPORARY TABLE match_candidates AS (
    SELECT
        r.sname_place_id,
        r.location_name AS r_location_name,
        r.zip_code AS r_zip_code,
        r.city AS r_city,
        r.state AS r_state,
        r.street_address AS r_street_address,
        r.phone_number AS r_phone_number,
        y.id AS y_id,
        y.name AS y_name,
        y.phone AS y_phone,
        y.zip_code AS y_zip_code,
        y.address1 AS y_address1,
        ST_Distance(r.location, y.location_geography) AS distance
    FROM
        restaurants AS r
    LEFT JOIN
        yelp_all_copy AS y
    ON 
        ST_DWithin(r.location, y.location_geography, 100)
    ORDER BY 
        r.sname_place_id,
        distance
);
"""


################################################################################


def match_candidates_by_distance(con):
    """ This function creates a table with potential pairs of SG and Yelp place
    matches. """

    # Logging
    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)

    # Connection engine to database
    # engine = db.create_engine('postgresql://{user}:{user_pass}@{host}/{dataname1}')
    # logger.info('Connection engine created.')

    # Create restaurants table
    logger.info('Creating restaurants table.')
    l_create_restaurants_geo_all_statement = (
            create_restaurants_geo_all_statement
            )
    con.execute(l_create_restaurants_geo_all_statement)
    logger.info('Restaurants table created.')

    # Create a copy of all_yelp_restaurants table
    logger.info('Creating Yelp copy.')
    l_create_yelp_all_copy_statement = (
            create_yelp_all_copy_statement
            )
    con.execute(l_create_yelp_all_copy_statement)
    logger.info('Yelp copy created.')

    logger.info('Doing the spatial join.')
    # Create candidates for matching from yelp by joining by distance
    l_sname_yelp_spatial_join_statement = (
            sname_yelp_spatial_join_statement
            )
    con.execute(l_sname_yelp_spatial_join_statement)
    logger.info('Spatial join completed.')

    # Export the resulting potential matches as a pandas table
    match_candidates = pd.read_sql_query('SELECT * FROM match_candidates', con = con)
    return match_candidates

################################################################################
