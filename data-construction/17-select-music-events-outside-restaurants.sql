/*******************************************************************************
*******************************************************************************/
--
-- FILE: 17-select-music-events-outside-restaurants.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Wed Nov 27 2019
--
-- DESC: This file creates a table with music events ourside of restaurants
-- only.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Select restaurants
CREATE TEMPORARY TABLE restaurants AS
    SELECT
        *
    FROM
        pois
    WHERE
        naics_code IN (722511, 722513)
;

-- Restaurants with corresponding polygons
CREATE TEMPORARY TABLE restaurants_with_geo AS
    SELECT
        r.sname_place_id,
        g.geom AS location
    FROM
        restaurants AS r
    LEFT JOIN
        geometry_pois AS g
    ON
        r.sname_place_id = g.sname_place_id
;
-- Create spatial index
CREATE INDEX restaurants_with_geo_location_idx
ON restaurants_with_geo
USING GIST (location);

-- Create a temporary table to mark events that are outside of restaurants
CREATE TEMPORARY TABLE music_events AS
    SELECT 
        *
    FROM 
        eventful_by_point
    WHERE 
        search_category = 'music';

-- Change type of lat, lon columns to numeric
ALTER TABLE music_events 
ALTER COLUMN longitude TYPE NUMERIC USING longitude::NUMERIC,
ALTER COLUMN latitude TYPE NUMERIC USING latitude::NUMERIC; 

-- Add location column
SELECT AddGeometryColumn('music_events', 'location', 4326, 'POINT', 2);
UPDATE music_events
SET location = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326);
-- Create index
CREATE INDEX music_geo_idx
ON music_events
USING GIST(location);

-- Assign sname_place_id to each event
ALTER TABLE music_events
ADD column sg_id text;

WITH matched AS (
    SELECT 
        m.id AS id,
        r.sname_place_id AS sg_id
    FROM
        music_events AS m
    JOIN 
        restaurants_with_geo AS r
    ON 
        ST_Contains(r.location, m.location)
    )
UPDATE 
    music_events AS mus
SET
    sg_id = m.sg_id
FROM 
    matched AS m
WHERE 
    mus.id = m.id;

-- Select events outside of restaurants
CREATE TABLE music_outside_restaurants AS
    SELECT 
        *
    FROM
        music_events
    WHERE 
        sg_id IS NULL;

-- Convert start_time to timestamp (new column created)
ALTER TABLE 
    music_outside_restaurants
ADD column start_time_ts TIMESTAMP;

UPDATE 
    music_outside_restaurants 
SET 
    start_time_ts = to_date(start_time, 'YYYY-MM-DD HH24:MI:SS');
-- Alternative:
-- ALTER TABLE <tablename> ALTER COLUMN <columnname> TYPE DATE
-- using to_date(<columnname>, 'YYYY-MM-DD');


ALTER TABLE 
    music_outside_restaurants
ADD COLUMN year INTEGER,
ADD COLUMN month INTEGER;

UPDATE 
    music_outside_restaurants
SET
    year = date_part('year', start_time_ts),
    month = date_part('month', start_time_ts);
