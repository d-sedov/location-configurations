/*******************************************************************************
*******************************************************************************/
--
-- FILE: 15-events-location-assignation.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Mon Nov 18 2019
--
-- DESC: This file contains the SQL code that create the location column for
-- eventful_by_point table in the database.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

ALTER TABLE eventful_by_point
ALTER COLUMN longitude TYPE NUMERIC USING longitude::NUMERIC,
ALTER COLUMN latitude TYPE NUMERIC USING latitude::NUMERIC;

-- Add location column
SELECT AddGeometryColumn('eventful_by_point', 'location', 4326, 'POINT', 2);
-- Set location column
UPDATE eventful_by_point
SET location = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326);

-- Add a spatial index on eventful_by_point
CREATE INDEX eventful_by_point_location_idx
ON eventful_by_point
USING GIST(location);


