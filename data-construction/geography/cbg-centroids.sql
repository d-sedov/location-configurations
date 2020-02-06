/*******************************************************************************
*******************************************************************************/
--
-- FILE: cbg-centroids.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Mon Jan 20 2020
--
-- DESC: Code that adds a centroid point to the geometry_cbg table.
-- 
-- REF:
--
-- COMMENT: 
--
/*******************************************************************************
*******************************************************************************/


/******************************************************************************/
-- Add location column
SELECT AddGeometryColumn('geometry_cbg', 'centroid', 4326, 'POINT', 2);
UPDATE geometry_cbg
SET centroid = ST_Centroid(wkb_geometry);

-- Add a spatial index on geometry_cbg
CREATE INDEX geometry_cbg_centroid_idx
ON geometry_cbg
USING GIST(centroid);
/******************************************************************************/
