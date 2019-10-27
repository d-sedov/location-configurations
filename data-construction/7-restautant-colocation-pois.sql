/*******************************************************************************
*******************************************************************************/
--
-- FILE: 
--
-- BY: Dmitry Sedov 
--
-- CREATED: Thu Oct 10 2019
--
-- DESC: This file constructs a table that counts non-restaurant POIs and visits
--       to the in proximity to each restaurant.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Create a table with restaurants only
CREATE TEMPORARY TABLE restaurants AS
    SELECT
        *
    FROM
        pois
    WHERE 
        naics_code IN (722511, 722513)
;
        

-- Join restaurants to get their coordinates
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
-- Create spatial index
CREATE INDEX restaurants_with_geo_location_idx
ON restaurants_with_geo
USING GIST (location);


-- Create a table with non-restaurants only
CREATE TEMPORARY TABLE non_restaurants AS
    SELECT
        *
    FROM
        pois
    WHERE 
        naics_code NOT IN (722511, 722513)
;
        

-- Join non-restaurants to get their coordinates
CREATE TEMPORARY TABLE non_restaurants_with_geo AS
    SELECT
        n.sname_place_id,
        g.location::geography AS location,
        p.raw_visit_counts AS visits
    FROM
        non_restaurants AS n
    LEFT JOIN
        geometry_pois AS g
    ON
        n.sname_place_id = g.sname_place_id
    LEFT JOIN
        patterns AS p
    ON
        n.sname_place_id = p.sname_place_id
;
-- Create spatial index
CREATE INDEX non_restaurants_with_geo_location_idx
ON non_restaurants_with_geo
USING GIST (location);

-- Function that adds a column to the restaurants table
-- with a POI count / visit count within a radius.
CREATE OR REPLACE FUNCTION pois_within(meters INTEGER)
    RETURNS void AS
    $$
    DECLARE
        colname1 text;
        colname2 text;
    BEGIN
        colname1 := format('pois_within_%s', meters);
        colname2 := format('visits_within_%s', meters);
        RAISE NOTICE 'Creating columns: %, %', colname1, colname2;
        EXECUTE format('ALTER TABLE restaurants ADD COLUMN %I INTEGER', colname1);
        EXECUTE format('ALTER TABLE restaurants ADD COLUMN %I INTEGER', colname2);
        -- Join on distance-to-cbg to count the devices in proximity
        CREATE TEMPORARY TABLE distance_to_pois AS
            SELECT 
                rg.sname_place_id AS sname_place_id, 
                COUNT(DISTINCT nrg.sname_place_id) AS pois_count,
                SUM(nrg.visits) AS visits_count
            FROM 
                restaurants_with_geo AS rg
            LEFT JOIN 
                non_restaurants_with_geo AS nrg
            ON 
                ST_DWithin(rg.location, nrg.location, meters)
            GROUP BY
                rg.sname_place_id
        ;
        EXECUTE format('UPDATE restaurants 
            SET 
                %1$I = d.pois_count,
                %2$I = d.visits_count
            FROM distance_to_pois AS d
            WHERE restaurants.sname_place_id = d.sname_place_id;', colname1, colname2);
        DROP TABLE distance_to_pois;
    END;
    $$
    LANGUAGE plpgsql;


-- Add the pois-in-radius columns
DO
$$
BEGIN
    FOR counter IN 500..1500 BY 500 LOOP
        PERFORM pois_within(counter);
    END LOOP;
END
$$;

DO
$$
BEGIN
    FOR counter IN 2000..20000 BY 6000 LOOP
        SELECT * FROM pois_within(counter);
    END LOOP;
END
$$;

-- Create index on restaurants table
CREATE UNIQUE INDEX restaurants_sg_idx 
ON restaurants (sname_place_id);

-- Export the restaurant data for initial estimation
CREATE TEMPORARY TABLE restaurants_for_export AS
    SELECT 
        r.*,
        g.area_m2 AS area_m2,
        g.cbg AS cbg,
        p.raw_visit_counts AS visits,
        p.raw_visitor_counts AS visitors
    FROM 
        restaurants AS r
    LEFT JOIN
        geometry_pois AS g
    ON
        r.sname_place_id = g.sname_place_id
    LEFT JOIN 
        patterns AS p
    ON
        r.sname_place_id = p.sname_place_id
;

\copy (SELECT * FROM restaurants_for_export) to '/home/user/projects/urban/data/processed/descriptive/restaurants_colocation.csv' with csv header
