/*******************************************************************************
*******************************************************************************/
--
-- FILE: 
--
-- BY: Dmitry Sedov 
--
-- CREATED: Tue Oct 29 2019
--
-- DESC: This file contains the code that constructs a restaurant-day-level
--       panel from a month of data. The time-changing columns are visits to 
--       co-located POIs and restaurants. The time-invariant columns are 
--       restaurant id and CBG.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Create a table with restaurants only
CREATE TEMPORARY TABLE restaurants AS (
    SELECT
        p.*,
        g.cbg AS cbg,
        g.cbsa AS cbsa,
        pa.visits_by_day AS visits_by_day
    FROM
        pois AS p
    INNER JOIN
        geometry_pois AS g
    ON
        p.sname_place_id = g.sname_place_id
    INNER JOIN
        patterns AS pa
    ON
        p.sname_place_id = pa.sname_place_id
    WHERE 
        p.naics_code IN (722511, 722513)
);
-- Create key on restaurants        
CREATE UNIQUE INDEX restaurants_sg_idx                                                                                                                                                                                                        
ON restaurants (sname_place_id);

-- Restaurant transformed to daily observations
CREATE TEMPORARY TABLE restaurants_expanded AS (
    SELECT 
        sname_place_id,
        cbg,
        cbsa,
        value::INTEGER AS own_visits,
        ordinality AS day
    FROM restaurants
    CROSS JOIN json_array_elements_text(visits_by_day) WITH ordinality
);


-- Join restaurants to get their coordinates
CREATE TEMPORARY TABLE restaurants_with_geo AS
    SELECT
        r.sname_place_id,
        r.visits_by_day,
        g.location::geography AS location
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


-- Non-restaurant locations and visits 
CREATE TEMPORARY TABLE non_restaurants_with_geo AS
    SELECT
        p.sname_place_id,
        g.location::geography AS location,
        pa.visits_by_day AS visits_by_day
    FROM
        pois AS p
    INNER JOIN
        geometry_pois AS g
    ON
        p.sname_place_id = g.sname_place_id
    INNER JOIN
        patterns AS pa
    ON
        p.sname_place_id = pa.sname_place_id
    WHERE 
        p.naics_code NOT IN (722511, 722513)
;
-- Create spatial index
CREATE INDEX non_restaurants_with_geo_location_idx
ON non_restaurants_with_geo
USING GIST (location);


-- Function that creates a table with rows containing visits to nearby POIs
-- for each restaurant-day.
-- TODO: Deal with co_pois (invariant in monthly day level panel in any case)
CREATE OR REPLACE FUNCTION pois_visits_within_daily(meters INTEGER)
    RETURNS void AS
    $$
    DECLARE
        colname1 text;
    BEGIN
        colname1 := format('visits_within_%s', meters);
        RAISE NOTICE 'Creating column: %.', colname1;
        EXECUTE format('ALTER TABLE restaurants_expanded ADD COLUMN %I INTEGER', colname1);
        -- Join on distance-within
        CREATE TEMPORARY TABLE joined_by_distance AS (
            SELECT 
                rg.sname_place_id AS sname_place_id, 
                nrg.sname_place_id AS co_sname_place_id,
                nrg.visits_by_day AS co_visits_count
            FROM 
                restaurants_with_geo AS rg
            LEFT JOIN 
                non_restaurants_with_geo AS nrg
            ON 
                ST_DWithin(rg.location, nrg.location, meters)
        );
        CREATE TEMPORARY TABLE co_visits_by_day AS (
            WITH json_unpacked AS (
                SELECT 
                    sname_place_id,
                    value::INTEGER AS co_visits,
                    ordinality AS day
                FROM joined_by_distance
                CROSS JOIN json_array_elements_text(co_visits_count) WITH ordinality
            )
            SELECT
                sname_place_id,
                day,
                SUM(co_visits) AS co_visits_all
            FROM json_unpacked
            GROUP BY sname_place_id, day
            ORDER BY sname_place_id, day
        );
        RAISE NOTICE 'Updating restaurants_expanded.';
        EXECUTE format('UPDATE restaurants_expanded
            SET 
                %1$I = c.co_visits_all
            FROM co_visits_by_day AS c
            WHERE restaurants_expanded.sname_place_id = c.sname_place_id 
            AND restaurants_expanded.day = c.day;', colname1);
        DROP TABLE co_visits_by_day;
        DROP TABLE joined_by_distance;
    END;
    $$
    LANGUAGE plpgsql;

-- Function to count competitors and visits to competitors in proximity
-- TODO: Deal with co_comp (invariant in monthly day level panel in any case)
CREATE OR REPLACE FUNCTION comp_visits_within_daily(meters INTEGER)
    RETURNS void AS
    $$
    DECLARE
        colname1 text;
    BEGIN
        colname1 := format('comp_visits_within_%s', meters);
        RAISE NOTICE 'Creating columns: %.', colname1;
        EXECUTE format('ALTER TABLE restaurants_expanded ADD COLUMN %I INTEGER', colname1);
        -- Join on distance-within
        CREATE TEMPORARY TABLE joined_by_distance AS (
            SELECT 
                rg.sname_place_id AS sname_place_id, 
                rgo.sname_place_id AS comp_sname_place_id,
                rgo.visits_by_day AS comp_visits_count
            FROM 
                restaurants_with_geo AS rg
            LEFT JOIN 
                restaurants_with_geo AS rgo
            ON 
                ST_DWithin(rg.location, rgo.location, meters)
            WHERE
                rg.sname_place_id != rgo.sname_place_id
        );
        CREATE TEMPORARY TABLE comp_visits_by_day AS (
            WITH json_unpacked AS (
                SELECT 
                    sname_place_id,
                    value::INTEGER AS comp_visits,
                    ordinality AS day
                FROM joined_by_distance
                CROSS JOIN json_array_elements_text(comp_visits_count) WITH ordinality
            )
            SELECT
                sname_place_id,
                day,
                SUM(comp_visits) AS comp_visits_all
            FROM json_unpacked
            GROUP BY sname_place_id, day
            ORDER BY sname_place_id, day
        );
        RAISE NOTICE 'Updating restaurants_expanded.';
        EXECUTE format('UPDATE restaurants_expanded
            SET 
                %1$I = c.comp_visits_all
            FROM comp_visits_by_day AS c
            WHERE restaurants_expanded.sname_place_id = c.sname_place_id 
            AND restaurants_expanded.day = c.day;', colname1);
        -- TODO: REPLACE NULLs with 0s in newly created columns
        DROP TABLE comp_visits_by_day;
        DROP TABLE joined_by_distance;
    END;
    $$
    LANGUAGE plpgsql;


\copy (SELECT * FROM restaurants_expanded) to '/home/user/projects/urban/data/processed/descriptive/restaurants_daily.csv' with csv header
