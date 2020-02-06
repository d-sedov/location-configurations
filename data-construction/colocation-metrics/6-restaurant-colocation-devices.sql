/*******************************************************************************
*******************************************************************************/
--
-- FILE: 6-restaurant-colocation-devices.sql
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


-- Append/join devices quantity to the geometry_cbg
CREATE TEMPORARY TABLE cbg_with_devices AS
    SELECT
        gc.censusblockgroup,
        gc.wkb_geometry::geography AS geog,
        hp.number_devices_residing AS devices
    FROM
        geometry_cbg AS gc
    LEFT JOIN
        home_panel AS hp
    ON 
        gc.censusblockgroup = hp.census_block_group
;
-- Create spatial index
CREATE INDEX cbg_with_devices_geog_idx
ON cbg_with_devices
USING GIST (geog);

-- Function that adds a column to the restaurants table
-- with a device count within a radius.
CREATE OR REPLACE FUNCTION devices_within(meters INTEGER)
    RETURNS void AS
    $$
    DECLARE
        colname text;
    BEGIN
        colname := format('devices_within_%s', meters);
        RAISE NOTICE 'Creating column: %', colname;
        EXECUTE format('ALTER TABLE restaurants ADD COLUMN %I INTEGER', colname);
        -- Join on distance-to-cbg to count the devices in proximity
        CREATE TEMPORARY TABLE distance_to_consumers AS
            SELECT 
                rg.sname_place_id AS sname_place_id, 
                SUM(gc.devices) AS devices
            FROM 
                restaurants_with_geo AS rg
            LEFT JOIN 
                cbg_with_devices AS gc
            ON 
                ST_DWithin(rg.location, gc.geog, meters)
            GROUP BY
                rg.sname_place_id
        ;
        EXECUTE format('UPDATE restaurants 
            SET %I = d.devices 
            FROM distance_to_consumers AS d
            WHERE restaurants.sname_place_id = d.sname_place_id;', colname);
        DROP TABLE distance_to_consumers;
    END;
    $$
    LANGUAGE plpgsql;


-- Add the devices-in-radius columns
DO
$$
BEGIN
    FOR counter IN 500..1500 BY 500 LOOP
        PERFORM devices_within(counter);
    END LOOP;
END
$$;

DO
$$
BEGIN
    FOR counter IN 2000..20000 BY 6000 LOOP
        PERFORM devices_within(counter);
    END LOOP;
END
$$;
