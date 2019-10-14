/*******************************************************************************
*******************************************************************************/
--
-- FILE: 
--
-- BY: Dmitry Sedov 
--
-- CREATED: Thu Oct 10 2019
--
-- DESC: This code constructs a table that counts devices in proximity to 
--       each restaurant in the dataset.
-- 
-- REF:
--
-- COMMENT: 
/*******************************************************************************
*******************************************************************************/

-- Create a view with restaurants only
CREATE TEMPORARY VIEW restaurants AS
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
        r.*,
        g.location
    FROM
        restaurants AS r
    LEFT JOIN
        geometry_cbg AS g
    ON
        r.sname_place_id = g.sname_place_id
;


-- Append/join devices quantity to the geometry_cbg
CREATE TEMPORARY TABLE cbg_with_devices AS
    SELECT
        gc.censusblockgroup,
        gc.location,
        hp.number_devices_residing AS devices
    FROM
        geometry_cbg AS gc
    LEFT JOIN
        home_panel AS hp
    ON 
        gc.censusblockgroup = hp.census_block_group
;


-- Join on distance-to-cbg to count the devices in proximity
CREATE TEMPORARY TABLE distance_to_consumers AS
    SELECT 
        rg.*,
        SUM(gc.devices)
    FROM 
        restaurants_with_geo AS rg
    LEFT JOIN 
        cbg_with_devices AS gc
    ON 
        ST_DWithin(rg.locationi::geography, gc.wkb_geometry::geography, __PARAMETER__)
    GROUP BY
        rg.sname_place_id
;
