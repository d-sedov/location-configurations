/*******************************************************************************
*******************************************************************************/
--
-- FILE: mean-distance-to-consumer-calculation.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Mon Jan 20 2020
--
-- DESC: Code to compute the average distance between a restaurant and consumers
-- in the home panel dataset. 
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/


-- Create restaurants table
CREATE TEMPORARY TABLE restaurants_with_geo AS (
    SELECT
        p.sname_place_id,
        g.cbg AS cbg,
        g.cbsa AS cbsa,
        g.location::geography AS location
    FROM
        pois AS p
    LEFT JOIN
        geometry_pois AS g
    ON
        p.sname_place_id = g.sname_place_id
    WHERE 
        p.naics_code IN (722511, 722513)
);
-- Create key on restaurants        
CREATE UNIQUE INDEX restaurants_with_geo_idx 
ON restaurants_with_geo (sname_place_id);
-- Create spatial index
CREATE INDEX restaurants_with_geo_location_idx
ON restaurants_with_geo
USING GIST (location);


-- Compute distance from each restautant to each CBG in the same MSA
-- Then average distance to CBGs according to device counts
CREATE TEMPORARY TABLE consumer_proximity AS (
    WITH distance_matrix AS (
        SELECT 
            r.*,
            ST_Distance(r.location, g.centroid::geography) AS distance,
            g.devices_residing AS devices_residing
        FROM 
            restaurants_with_geo AS r
        LEFT JOIN 
            geometry_cbg AS g
        ON 
            r.cbg = g.censusblockgroup
    )
    SELECT 
        d.sname_place_id,
        SUM(distance * devices_residing) / SUM(devices_residing) AS mean_distance
    FROM 
        distance_matrix
    GROUP BY
        d.sname_place_id;
);


-- Update restaurants table
ALTER TABLE restaurants_with_geo 
ADD COLUMN mean_consumer_distance NUMERIC;

UPDATE restaurants_with_geo 
SET restaurants_with_geo.mean_consumer_distance = consumer_proximity.mean_distance
FROM consumer_proximity
WHERE restaurants_with_geo.sname_place_id = consumer_proximity.sname_place_id;
