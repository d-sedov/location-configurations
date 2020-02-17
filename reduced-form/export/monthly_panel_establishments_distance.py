################################################################################
################################################################################
#
# FILE: monthly_panel_establishments_distance.py
#
# BY: Dmitry Sedov 
#
# CREATED: Sat 15 Feb 2020
#
# DESC: This code constructs a monthly panel with events dataset by
#
# EXEC: python3 monthly_panel_establishments_distance.py
#      
################################################################################
################################################################################

# Select restaurants with id and location columns only, create indices 
# (sname_place_id and spatial).
create_restaurants_geo_all_no_visits_statement = """
CREATE TEMPORARY TABLE restaurants_with_geo AS (
    SELECT
        r.sname_place_id,
        r.location::geography AS location
    FROM
        restaurants AS r
);

CREATE UNIQUE INDEX restaurants_sg_idx 
ON restaurants_with_geo (sname_place_id);

CREATE INDEX restaurants_with_geo_location_idx
ON restaurants_with_geo
USING GIST (location);
"""

# Select establishments with id and location columns only, create indices 
# (sname_place_id and spatial).
create_establishment_geo_all_no_visits_statement = """
CREATE TEMPORARY TABLE establishments_with_geo AS (
    SELECT
        e.sname_place_id,
        e.location::geography AS location
    FROM
        establishments AS e
);

CREATE UNIQUE INDEX establishments_sg_idx 
ON establishments_with_geo (sname_place_id);

CREATE INDEX establishments_with_geo_location_idx
ON establishments_with_geo
USING GIST (location);
"""

# Join restaurants with establishments by distance. That is, all POIs in
# proximity to each restaurant are stored in the resulting table.
spatial_join_meter_statement = """
CREATE TEMPORARY TABLE joined_by_distance AS (
    SELECT 
        rg.sname_place_id AS sname_place_id, 
        nrg.sname_place_id AS co_sname_place_id
    FROM 
        restaurants_with_geo AS rg
    LEFT JOIN 
        establishments_with_geo AS eg
    ON 
        ST_DWithin(rg.location, eg.location, {meters})
);
"""

# Select month of visits data 
create_one_month_visits = """
CREATE TEMPORARY TABLE visits_{year}_{month} AS (
    SELECT 
        sname_place_id,
        raw_visits_count,
        1 AS open
    FROM 
        visits 
    WHERE
        year = {year}
        AND
        month = {month}
);
"""

# Join the spatially joined table to visits table to mark which establishments
# are open in the 'current' month.
create_aggregated_month_statement = """
CREATE TEMPORARY TABLE aggregated_month AS (
    WITH joined_by_distance_with_visits AS ( 
        SELECT 
            j.sname_place_id,
            v.open
        FROM
            joined_by_distance AS j
        LEFT JOIN
            visits_{year}_{month} AS v
        ON 
            j.co_sname_place_id = v.sname_place_id
    )
    SELECT
       sname_place_id, 
       SUM(open) AS est_open_near
    FROM 
        joined_by_distance_with_visits
    GROUP BY
        sname_place_id;
);
"""

# This is the final table restaurants, count of establishments open in
# proximity, raw visit count to the restaurant.
create_month_final_statement = """
CREATE TEMPORARY TABLE month_final_{year}_{month} AS (
    SELECT 
        a.sname_place_id,
        v.raw_visit_counts,
        a.est_open_near,
        {year} AS year,
        {month} AS month
    FROM 
        aggregated_month AS a
    LEFT JOIN
        visits_{year}_{month} AS v
    ON
        a.sname_place_id = v.sname_place_id
);
"""
