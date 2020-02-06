/*******************************************************************************
*******************************************************************************/
--
-- FILE: 
--
-- BY: Dmitry Sedov 
--
-- CREATED: Sun Nov 17 2019
--
-- DESC: This file constructs a table that counts non-restaurant POIs and visits
--       to the in proximity to each restaurant FROM YELP DATASET.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/

-- Create a table with non-restaurants only
CREATE TEMPORARY TABLE non_restaurants AS
    SELECT
        *
    FROM
        pois
    WHERE 
        naics_code NOT IN (722511, 722513)
;
        
-- Create index for faster join
CREATE INDEX non_restaurants_sname_place_id_idx
ON non_restaurants(sname_place_id);

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

-- Function that adds a column to the yelp_restaurants table
-- witha POI count / visits count within a radius
CREATE OR REPLACE FUNCTION yelp_pois_within(meters INTEGER)
    RETURNS void AS
    $$
    DECLARE
        colname1 text;
        colname2 text;
    BEGIN
        colname1 := format('pois_within_%s', meters);
        colname2 := format('visits_within_%s', meters);
        RAISE NOTICE 'Creating columns: %, %', colname1, colname2;
        EXECUTE format('ALTER TABLE yelp_restaurants ADD COLUMN %I INTEGER', colname1);
        EXECUTE format('ALTER TABLE yelp_restaurants ADD COLUMN %I INTEGER', colname2);
        -- Join on distance-to-poi to count the pois and visits in proximity
        CREATE TEMPORARY TABLE distance_to_pois AS
            SELECT 
                yr.id AS id,
                COUNT(DISTINCT nrg.sname_place_id) AS pois_count,
                SUM(nrg.visits) AS visits_count
            FROM 
                yelp_restaurants AS yr
            LEFT JOIN 
                non_restaurants_with_geo AS nrg
            ON 
                ST_DWithin(yr.location, nrg.location, meters)
            GROUP BY
                yr.id
        ;
        EXECUTE format('UPDATE yelp_restaurants 
            SET 
                %1$I = d.pois_count,
                %2$I = d.visits_count
            FROM distance_to_pois AS d
            WHERE yelp_restaurants.id = d.id;', colname1, colname2);
        DROP TABLE distance_to_pois;
    END;
    $$
    LANGUAGE plpgsql;

ALTER TABLE yelp_restaurants
ADD COLUMN category1 VARCHAR;

UPDATE yelp_restaurants
SET category1 = (regexp_match(categories, '({''alias'': '')(.*?)('',)'))[2];

-- Export the restaurant data for initial estimation
CREATE TEMPORARY TABLE yelp_restaurants_for_export AS
    SELECT 
        price,
        review_count,
        rating,
        category1,
        cbg,
        cbsa,
        pois_within_100,
        pois_within_500,
        pois_within_900,
        visits_within_100,
        visits_within_500,
        visits_within_900
    FROM 
        yelp_restaurants;

\copy (SELECT * FROM yelp_restaurants_for_export) to '/home/user/projects/urban/data/processed/descriptive/yelp_restaurants_colocation.csv' with csv header
