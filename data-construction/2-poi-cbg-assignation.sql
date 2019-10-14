/*******************************************************************************
*******************************************************************************/
--
-- FILE: 2-poi-cbg-assignation.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Tue Sep 17 2019 
--
-- DESC: This file contains the code that adds the 'cbg' column to the
--       geometry_pois table by doing a spatial join on geometry_cbg.wkb_geometry
--       containing the geometry_pois(longitude, latitude) location.
-- 
-- REF:
--
-- COMMENT: 
--          For 'security' purposes TABLE geometry_pois is substituted with
--          TABLE test_cbg_assignation.
/*******************************************************************************
*******************************************************************************/

/**************************** CBG ASSIGNATION *********************************/
/******************************************************************************/

-- Add location column (simply a point).
SELECT AddGeometryColumn('test_cbg_assignation','location', 4326, 'POINT', 2);
UPDATE test_cbg_assignation 
SET location = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326);


-- Add a spatial index on geometry_pois
CREATE INDEX test_cbg_assignation_location_idx 
ON test_cbg_assignation 
USING GIST(location);

-- NOTE: the next paragraph of code works considerably faster when an index on
-- sname_place_id in temporary column is present.
-- Add CBG column
ALTER TABLE test_cbg_assignation 
ADD COLUMN cbg text; 
-- Spatial join: find CBG containing the POI
WITH matched AS (
    SELECT t.sname_place_id AS sname_place_id,
           g.censusblockgroup AS cbg
    FROM test_cbg_assignation AS t
    JOIN geometry_cbg AS g
    ON ST_Contains(g.wkb_geometry, t.location)
)
-- Update the main table
UPDATE test_cbg_assignation AS tcg
SET cbg = m.cbg
FROM matched AS m
WHERE 
tcg.sname_place_id = m.sname_place_id;

/******************************************************************************/



/************************** TEST CBG ASSIGNATION ******************************/

-- Create test table
CREATE TEMPORARY TABLE test_cbg_assignation (
    sname_place_id text PRIMARY KEY, 
    latitude NUMERIC,
    longitude NUMERIC
);

INSERT INTO test_cbg_assignation (sname_place_id, latitude, longitude) 
    VALUES
    ('id1', 32.804563, -117.03831),
    ('id2', 33.970445, -78.390476),
    ('id3', 32.755291, -97.329815),
    ('id4', 0.0, 0.0);

-- Desired output after CBG ASSIGNATION execution
SELECT * FROM test_cbg_assignation;

/*
 sname_place_id | latitude  | longitude  |                      location                      |     cbg      
--------------------+-----------+------------+----------------------------------------------------+--------------
 id4                |       0.0 |        0.0 | 0101000020E610000000000000000000000000000000000000 | 
 id1                | 32.804563 | -117.03831 | 0101000020E61000000647C9AB73425DC02C499EEBFB664040 | 060730098053
 id2                | 33.970445 | -78.390476 | 0101000020E6100000DE770C8FFD9853C08AC8B08A37FC4040 | 370190206012
 id3                | 32.755291 | -97.329815 | 0101000020E6100000BCAE5FB01B5558C048FB1F60AD604040 | 484391233002
*/

/******************************************************************************/
/******************************************************************************/
