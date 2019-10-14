/*******************************************************************************
*******************************************************************************/
--
-- FILE: 1-cbsa-description.sql
--
-- BY: Dmitry Sedov 
--
-- CREATED: Thu Sep 25 2019
--
-- DESC: This file contains the code that produces "summary statistics" for
--       CBSAs.
-- 
-- REF:
--
-- COMMENT: The desired summary statistics are the means and quartiles of the
--          following variables
--          * Area
--          * Devices
--          * Number of CBGs
--          * Number of POIs
--          * Number of POI categories
--          * Percent of POIs with missing visits
--          * Total visits
--          * Category HHI in terms of visits
/*******************************************************************************
*******************************************************************************/

/**************************** CBSA SUMMARY STATS ******************************/

-- Compute the CBSA area, count number of CBGs and devices
CREATE TEMPORARY TABLE cbsa_properties AS
    SELECT 
        gc.cbsa AS cbsa,
        COUNT(DISTINCT gc.censusblockgroup) AS number_cbgs,
        SUM(gc.area_m2) AS area_m2,
        SUM(hp.number_devices_residing) AS devices
    FROM 
        geometry_cbg AS gc
    LEFT JOIN
        home_panel AS hp
    ON 
        gc.censusblockgroup = hp.census_block_group
    GROUP BY
        gc.cbsa;


-- Compute the number of POIs, number of visits, share of POIs with missing
-- visits data by NAICS first 2 digits and CBSA.
CREATE TEMPORARY TABLE cbsa_pois AS
    SELECT
        gp.cbsa AS cbsa,
        po.naics_first2 AS naics_first2,
        COUNT(DISTINCT po.naics_code) AS number_naics_codes,
        COUNT(po.sname_place_id) AS number_pois,
        COUNT(pa.raw_visit_counts) AS pois_non_missing_visits,
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
        gp.cbsa, po.naics_first2;


-- Compute the total number of visits in CBSA, add as a column to the temporary
-- table cbsa_pois.
ALTER TABLE cbsa_pois
ADD COLUMN total_visits INTEGER;
-- NOTE that IS NOT DISTINCT FROM allows to join the NULL values also
WITH cbsa_total_visits AS (
    SELECT 
        cbsa, 
        SUM(visits) AS total_visits
    FROM 
        cbsa_pois
    GROUP BY 
        cbsa
)
UPDATE 
    cbsa_pois
SET
    total_visits = ctv.total_visits
FROM 
    cbsa_total_visits AS ctv
WHERE 
    cbsa_pois.cbsa IS NOT DISTINCT FROM ctv.cbsa;


-- Add the squared category shares column to the cbsa_pois table
ALTER TABLE cbsa_pois
ADD COLUMN squared_shares numeric;
UPDATE cbsa_pois
SET squared_shares = POWER(visits::numeric / total_visits::numeric, 2);


-- Compute all cbsa properties, store them in new temp table
CREATE TEMPORARY TABLE cbsas AS (
    WITH part1 AS (
        SELECT 
            cbsa,
            SUM(number_naics_codes) AS total_codes,
            SUM(visits) AS visits,
            SUM(visitors) AS visitors,
            SUM(squared_shares) AS hhi,
            SUM(number_pois) AS number_pois,
            SUM(pois_non_missing_visits) AS pois_non_missing_visits
        FROM cbsa_pois
        GROUP BY cbsa)
    SELECT 
        p.cbsa,
        p.total_codes,
        p.visits,
        p.hhi,
        p.number_pois,
        p.pois_non_missing_visits,
        cp.number_cbgs,
        cp.area_m2,
        cp.devices
    FROM cbsa_properties AS cp
    LEFT JOIN part1 AS p
    ON cp.cbsa = p.cbsa
);
/******************************************************************************/

-- Export the resulting table
\copy (SELECT * FROM cbsas) to '/home/user/projects/urban/data/processed/descriptive/cbsas.csv' with csv header

/******************************************************************************/
