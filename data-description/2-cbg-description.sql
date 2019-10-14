/*******************************************************************************
*******************************************************************************/
--
-- FILE: 2-cbg-description.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Thu Sep 25 2019
--
-- DESC: This file contains the code that produces "summary statistics" for
--       CBGs.
-- 
-- REF:
--
-- COMMENT: The desired summary statistics are the means and quartiles of the
--          following variables
--          * Area
--          * Devices
--          * Number of POIs
--          * Number of POI categories
--          * Total visits
--          * Percent of POIs with low market shares (lower than fair share / 3)
--          * Percent of POIs with missing visits data
/*******************************************************************************
*******************************************************************************/

-- Compute the basic CBG properties: area, devices
CREATE TEMPORARY TABLE cbg_properties AS
    SELECT 
        gc.censusblockgroup AS cbg,
        hp.number_devices_residing AS devices,
        gc.area_m2 AS area_m2
    FROM
        geometry_cbg AS gc
    LEFT JOIN
        home_panel AS hp
    ON 
        gc.censusblockgroup = hp.census_block_group;


-- Compute the N(POIs), N(NAICS categories), total visits to CBG
CREATE TEMPORARY TABLE cbg_pois AS
    SELECT
        gp.cbg AS cbg,
        COUNT(DISTINCT po.sname_place_id) AS number_pois,
        COUNT(pa.raw_visit_counts) AS pois_non_missing_visits,
        COUNT(DISTINCT po.naics_code) AS number_naics_codes,
        SUM(pa.raw_visit_counts) AS visits,
        SUM(pa.raw_visitor_counts) AS visitors
    FROM 
        pois AS po
    LEFT JOIN 
        patterns AS pa
    ON 
        po.sname_place_id = pa.sname_place_id
    LEFT JOIN 
        geometry_pois AS gp
    ON 
        po.sname_place_id = gp.sname_place_id
    GROUP BY 
        gp.cbg;


-- Create a temporary table with all pois marked with their CBG
CREATE TEMPORARY TABLE temp_poi AS
    SELECT 
        po.sname_place_id AS sname_place_id,
        pa.raw_visit_counts AS raw_visit_counts,
        gp.cbg AS cbg
    FROM
        pois AS po
    LEFT JOIN 
        patterns AS pa
    ON 
        po.sname_place_id = pa.sname_place_id
    LEFT JOIN 
        geometry_pois AS gp
    ON 
        po.sname_place_id = gp.sname_place_id;


-- ADD total CBG visits column in order to compute inside-CBG market shares
ALTER TABLE temp_poi
ADD COLUMN total_visits INTEGER,
ADD COLUMN pois_non_missing_visits INTEGER,
ADD COLUMN low_share INTEGER DEFAULT 0,
ADD COLUMN squared_shares NUMERIC;


-- Add the total CBG visits to the temporary temp_poi table
-- Add the total number of pois with non-missing visits into CBG
UPDATE
    temp_poi
SET 
    total_visits = cp.visits,
    pois_non_missing_visits = cp.pois_non_missing_visits
FROM
    cbg_pois AS cp
WHERE 
    temp_poi.cbg = cp.cbg;

-- Mark pois with lower than fair share of visits as 'low-share'
UPDATE
    temp_poi
SET
    low_share = 1
WHERE
    pois_non_missing_visits > 0 
    AND
    (raw_visit_counts::NUMERIC / total_visits::NUMERIC) < (1.0 / pois_non_missing_visits::NUMERIC);


-- Compute the square shares for HHI index
UPDATE
    temp_poi
SET 
    squared_shares = POWER(raw_visit_counts::NUMERIC / total_visits::NUMERIC, 2);


-- Gather all of the CBG-related data
CREATE TEMPORARY TABLE cbgs AS (
    WITH part1 AS (
        SELECT 
            cbg,
            SUM(squared_shares) AS hhi,
            SUM(low_share) AS low_share_count
        FROM
            temp_poi
        GROUP BY 
            cbg
    )
    SELECT
        cp.cbg,
        cp.devices,
        cp.area_m2,
        cp2.number_pois,
        cp2.pois_non_missing_visits,
        cp2.number_naics_codes,
        cp2.visits,
        cp2.visitors,
        p.hhi,
        p.low_share_count
    FROM 
        cbg_properties AS cp
    LEFT JOIN 
        cbg_pois AS cp2
    ON 
        cp.cbg = cp2.cbg
    LEFT JOIN
        part1 AS p
    ON
        cp.cbg = p.cbg
);
/******************************************************************************/

-- Export the resulting table
\copy (SELECT * FROM cbgs) to '/home/user/projects/urban/data/processed/descriptive/cbgs.csv' with csv header

/******************************************************************************/





