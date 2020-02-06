/*******************************************************************************
*******************************************************************************/
--
-- FILE: 12-yelp-cbg-assignation.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Sun Nov 17 2019 
--
-- DESC: This file contains the SQL code that assigns a CBG to each restaurant
-- downloaded from Yelp.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

-- Add location column
SELECT AddGeometryColumn('yelp_restaurants', 'location', 4326, 'POINT', 2);
-- Set location column
UPDATE yelp_restaurants
SET location = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326);


-- Add a spatial index on yelp_restaurants
CREATE INDEX yelp_restaurants_location_idx
ON yelp_restaurants
USING GIST(location);

-- Add the CBG column
ALTER TABLE yelp_restaurants
ADD COLUMN cbg TEXT;

-- Spatial join to determine which CBG the restaurant belongs to
UPDATE yelp_restaurants AS yr
SET cbg = g.censusblockgroup
FROM 
geometry_cbg AS g
WHERE ST_Contains(g.wkb_geometry, yr.location);

